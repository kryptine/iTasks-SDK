implementation module iTasks.Internal.SDSService

import iTasks

import iTasks.Extensions.Distributed._Formatter
import iTasks.SDS.Definition
import iTasks.Internal.AsyncSDS
import iTasks.Internal.Distributed.Symbols

from iTasks.Internal.TaskStore import queueRefresh
import iTasks.Internal.TaskState
import iTasks.Internal.Task
import iTasks.Internal.TaskServer
import iTasks.Internal.TaskEval
import iTasks.Internal.IWorld
import qualified Data.Set as Set
import qualified Data.Map as Map
import Text

import StdDebug, StdMisc

/** hostname, connection on which to send the reply, accumulated data received **/
:: SDSServiceState = SDSProcessing String ConnectionId [String]

:: SDSOperation = OPRead | OPWrite | OPModify | OPRegister

/** (done, connection on which to send the result, hostname of request, string of the evaluation state) **/
:: SDSEvaluations :== Map ConnectionId (Bool, String, String)

sdsServiceTask :: Int -> Task ()
sdsServiceTask port = catchAll (Task eval) (\m. trace_n ("Exception in SDS Service task: " +++ m) (treturn ()))
where
	share :: SDSLens () SDSEvaluations SDSEvaluations
	share = (sharedStore "sdsServiceTaskShare" 'Map'.newMap)

	eval event evalOpts tree=:(TCInit taskId ts) iworld
	# (symbols, iworld) = case read symbolsShare EmptyContext iworld of
		(Ok (ReadResult symbols _), iworld) = (readSymbols symbols, iworld)
	# (mbError, iworld) = addListener taskId port True (wrapIWorldConnectionTask (handlers symbols taskId) share) iworld
	| mbError=:(Error _) = (ExceptionResult (fromError mbError), iworld)
	| not (trace_tn ("SDS server listening on " +++ toString port)) = undef
	= (ValueResult (Value () False) {TaskEvalInfo|lastEvent=ts,removedTasks=[],refreshSensitive=True} (ReplaceUI (ui UIEmpty)) (TCBasic taskId ts (DeferredJSONNode JSONNull) False), iworld)

	eval (RefreshEvent taskIds cause) evalOpts tree=:(TCBasic taskId ts data bla) iworld
	| not ('Set'.member taskId taskIds) = (ValueResult (Value () False) {TaskEvalInfo|lastEvent=ts,removedTasks=[],refreshSensitive=True} NoChange (TCBasic taskId ts data bla), iworld)
	| not (trace_tn ("Refresh SDS Service: " +++ cause)) = undef
	# (symbols, iworld) = case read symbolsShare EmptyContext iworld of
		(Ok (ReadResult symbols _), iworld) = (readSymbols symbols, iworld)
	# (readResult, iworld) = read share EmptyContext iworld
	| readResult=:(Error _) = (ExceptionResult (fromError readResult), iworld)
	# shareValue = 'Map'.toList (directResult (fromOk readResult))
	| not (trace_tn ("Re-evaluate " +++ toString (length shareValue) +++ " shares")) = undef
	| not (trace_tn ("Share value before: \n" +++ concat (map (\(connid, (done, host, received)). "\t" +++toString connid +++ ": " +++ toString done +++ ", " +++ host +++ "\n") shareValue))) = undef
	# (results, iworld) = reevaluateShares symbols  taskId shareValue iworld
	| results=:(Error _) = trace_n ("SDSService: Error evaluating shared: " +++ fromError results) (ExceptionResult (exception (fromError results)), iworld)
	| not (trace_tn ("Evaluated " +++ toString (length (fromOk results)) +++ " shares")) = undef
	| not (trace_tn ("Share value after: \n" +++ concat (map (\(connid, (done, host, received)). "\t" +++toString connid +++ ": " +++ toString done +++ ", " +++ host +++ "\n") (fromOk results)))) = undef
	# (writeResult, iworld) = write ('Map'.fromList (fromOk results)) share EmptyContext iworld
	| writeResult=:(Error _) = trace_n "Error writing to share" (ExceptionResult (fromError writeResult), iworld)
	= (ValueResult (Value () False) {TaskEvalInfo|lastEvent=ts,removedTasks=[],refreshSensitive=True} NoChange tree, iworld)

	handlers symbols taskId = {ConnectionHandlersIWorld|onConnect = onConnect
    	, onData = onData symbols taskId
    	, onShareChange = onShareChange
    	, onTick = onTick
    	, onDisconnect = onDisconnect
		}

	reevaluateShares :: !{#Symbol} !TaskId ![(ConnectionId, (Bool, String, String))] *IWorld -> (MaybeErrorString [(ConnectionId, (Bool, String, String))], *IWorld)
	reevaluateShares symbols taskId evals iworld = reevaluateShares` symbols taskId evals [] iworld
	where
		reevaluateShares` symbols taskId [] acc iworld = (Ok acc, iworld)
		reevaluateShares` symbols taksId [(connId, (done, host, val)):rest] acc iworld
		# (result, iworld) = performRequest symbols taskId host val iworld
		= case result of
			Error e 		= trace_n ("Evaluated got error: " +++ e) (Error e, iworld)
			Ok (Left val)	= trace_n ("Evaluated share for " +++ host +++ ":" +++ toString connId +++ ": Done") (reevaluateShares` symbols taskId rest [(connId, (True, host, val)) : acc] iworld)
			Ok (Right val)
			# ioStates = iworld.IWorld.ioStates
			| not (trace_tn ("reevaluateShares " +++ ioStateString ioStates)) = undef
			= trace_n ("Evaluated share for " +++ host +++ ":" +++ toString connId +++ ": Busy") (reevaluateShares` symbols taskId rest [(connId, (False, host, val)) : acc] iworld)

	onConnect :: !ConnectionId !String !SDSEvaluations !*IWorld -> *(!MaybeErrorString SDSServiceState, Maybe SDSEvaluations, ![String], !Bool, !*IWorld)
	onConnect connId clientName sdsValue iworld
	| not (trace_tn ("Received new connection from " +++ clientName +++ ". Connection id: " +++ toString connId)) = undef
	= (Ok (SDSProcessing clientName connId []), Nothing, [], False, iworld)

	onData :: {#Symbol} !TaskId !String !SDSServiceState !SDSEvaluations !*IWorld -> *(!MaybeErrorString SDSServiceState, Maybe SDSEvaluations, ![String], !Bool, !*IWorld)
	onData symbols taskId receivedData state=:(SDSProcessing host connId received) sdsValue iworld
	| not (trace_tn ("SDS service onData received: " +++ (concat received))) = undef
	| not (trace_tn ("SDS service onData received data: " +++ receivedData)) = undef
	| not (endsWith "\n" receivedData) = (Ok (SDSProcessing host connId (received ++ [receivedData])), Nothing, [], False, iworld)
	# receivedData = concat (received ++ [receivedData])
	| not (trace_tn ("Received request " +++ host +++ ":" +++ toString connId +++ " [" +++ receivedData +++ "]")) = undef
	= case performRequest symbols taskId host receivedData iworld of
		(Error e, iworld) 					= trace_n ("Got error: " +++ e) (Error e, Nothing, [], True, iworld)
		(Ok (Left response), iworld) 		= trace_n ("Evaluated share for " +++ host +++ ":" +++ toString connId +++ ": Immediately done. Response: [" +++ response +++ "\n]") (Ok state, Nothing, [response +++ "\n"], True, iworld)
		(Ok (Right continuation), iworld=:{ioStates})
		| not (trace_tn ("onData " +++ ioStateString ioStates)) = undef
		= trace_n ("Evaluated share for " +++ host +++ ":" +++ toString connId +++ ": Busy") (Ok state, Just ('Map'.put connId (False, host, continuation) sdsValue), [], False, {iworld & ioStates = ioStates})

	onShareChange :: !SDSServiceState !SDSEvaluations !*IWorld -> *(!MaybeErrorString SDSServiceState, Maybe SDSEvaluations, ![String], !Bool, !*IWorld)
	onShareChange state=:(SDSProcessing host connId _) sdsValue iworld = case 'Map'.get connId sdsValue of
		Nothing = (Ok state, Nothing, [], False, iworld)
		// Not yet completed evaluating the sds, do nothing.
		Just (False, _, result) = trace_n ("Share change for " +++ host +++ ":" +++ toString connId +++ ": Not done") (Ok state, Nothing, [], False, iworld)
		// We have completed evaluating the SDS, send the result to the client.
		Just (True, _, result) = trace_n ("Share change for " +++ host +++ ":" +++ toString connId +++ ": Replying with [" +++ result +++ "\n]") (Ok state, Just ('Map'.del connId sdsValue), [result +++ "\n"], True, iworld)

	onTick        :: !SDSServiceState !SDSEvaluations !*IWorld -> *(!MaybeErrorString SDSServiceState, Maybe SDSEvaluations, ![String], !Bool, !*IWorld)
	onTick state sdsValue iworld = (Ok state, Nothing, [], False, iworld)

	onDisconnect  :: !SDSServiceState !SDSEvaluations !*IWorld -> *(!MaybeErrorString SDSServiceState, Maybe SDSEvaluations, !*IWorld)
	onDisconnect state=:(SDSProcessing host connId _) sdsValue iworld = trace_n ("Disconnecting " +++ host +++ ":" +++ toString connId) (Ok state, Just ('Map'.del connId sdsValue), iworld)

	// Left: Done
	// Right: Still need to do work..
	performRequest :: !{#Symbol} !TaskId !String !String !*IWorld -> !(MaybeErrorString !(Either !String !String), !*IWorld)
	performRequest symbols taskId host request iworld
	| newlines (fromString request) > 1 = abort ("Multiple requests: " +++ request)
	= case deserializeFromBase64 request symbols of
		(SDSReadRequest sds p)
		# ioStates = iworld.IWorld.ioStates
		| not (trace_tn ("performRequest " +++ ioStateString ioStates)) = undef
		= trace_n ("Got read for task " +++ toString taskId) (case readSDS sds p (TaskContext taskId) Nothing (sdsIdentity sds) iworld of
			(Error (_, e), iworld)							= (Error e, iworld)
			(Ok (ReadResult v _), iworld)					= trace_n "Done reading" (Ok (Left (serializeToBase64 v)), iworld)
			(Ok (AsyncRead sds), iworld)					= trace_n "Async read" (Ok (Right (serializeToBase64 (SDSReadRequest sds p))), iworld))
		(SDSRegisterRequest sds p reqSDSId remoteSDSId reqTaskId port) = trace_n ("Got register for " +++ remoteSDSId) (case readSDS sds p (RemoteTaskContext reqTaskId taskId remoteSDSId host port) (Just taskId) reqSDSId iworld of
			(Error (_, e), iworld) 							= (Error e, iworld)
			(Ok (ReadResult v _), iworld)					= trace_n "Done registering" (Ok (Left (serializeToBase64 v)), iworld)
			(Ok (AsyncRead sds), iworld)					= trace_n "Async register" (Ok (Right (serializeToBase64 (SDSRegisterRequest sds p reqSDSId remoteSDSId taskId port))), iworld))
		(SDSWriteRequest sds p val) 					= trace_n "Got write" (case writeSDS sds p (TaskContext taskId) val iworld of
			(Error (_, e), iworld) 							= (Error e, iworld)
			(Ok (WriteResult notify _), iworld)				= trace_n "Done writing" (Ok (Left (serializeToBase64 ())), queueNotifyEvents (sdsIdentity sds) notify iworld)
			(Ok (AsyncWrite sds), iworld)					= trace_n "Async write" (Ok (Right (serializeToBase64 (SDSWriteRequest sds p val))), iworld))
		(SDSModifyRequest sds p f)						= trace_n "Got modify" (case modifySDS f sds p (TaskContext taskId) iworld of
			(Error (_, e), iworld) 							= (Error e, iworld)
			(Ok (ModifyResult r w _), iworld)				= trace_n "Done modifying" (Ok (Left (serializeToBase64 (r,w))), iworld)
			(Ok (AsyncModify sds f), iworld)				= trace_n "Async modify" (Ok (Right (serializeToBase64 (SDSModifyRequest sds p f))), iworld))
		(SDSRefreshRequest refreshTaskId sdsId)
		// If we receive a request to refresh the sds service task, we find all remote registrations for the SDS id and send requests to refresh them.
		| taskId == refreshTaskId = refreshRemoteTasks sdsId host iworld
		= trace_n ("Got refresh for " +++ toSingleLineText refreshTaskId +++ sdsId) (Ok (Left "Refresh queued"), queueRefresh [(refreshTaskId, "Notification for remote write of " +++ sdsId)] iworld)
	where
		newlines [] = 0
		newlines ['\n':xs] = inc (newlines xs)
		newlines [x: xs] = newlines xs

		refreshRemoteTasks sdsId host iworld=:{sdsNotifyRequests}
		// We need to adjust the SDS id so that with the host information from which we received the refresh message.
		| not (trace_tn ("refreshRemoteTasks: " +++ sdsId)) = undef
		| not (trace_tn ("Requests: " +++ concat (map (\s. s +++ "\n") ('Map'.keys sdsNotifyRequests)))) = undef
		= case 'Map'.get sdsId sdsNotifyRequests of
			Nothing = (Ok (Left ("No requests available")), iworld)
			Just requestsToTime
			# notifyRequests = 'Map'.keys requestsToTime
			| not (trace_tn ("Notify requests for sds: " +++ concat (map (\s. toSingleLineText s +++ "\n") notifyRequests))) = undef
			= (Ok (Left "Requests re-queued"), queueNotifyEvents sdsId ('Set'.fromList notifyRequests) iworld)