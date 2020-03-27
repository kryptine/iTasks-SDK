implementation module iTasks.Internal.SDSService

import iTasks

import Data.Functor
from Data.Func import $
from StdMisc import abort, undef
import StdArray

import iTasks.Internal.Distributed.Formatter
import iTasks.SDS.Definition
import iTasks.Internal.AsyncSDS

import iTasks.Internal.TaskState 
import iTasks.Internal.Util
import iTasks.Internal.Task
import iTasks.Internal.TaskState
import iTasks.Internal.TaskIO
import iTasks.Internal.TaskServer
import iTasks.Internal.TaskEval
import iTasks.Internal.IWorld
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Map.GenJSON
import Text

/** hostname, connection on which to send the reply, accumulated data received **/
:: SDSServiceState = SDSProcessing String ConnectionId [String]

/** (done, hostname of request, string of the evaluation state) **/
:: SDSEvaluations :== Map ConnectionId (Bool, String, String)

sdsServiceTask :: Int -> Task ()
sdsServiceTask port = withShared 'Map'.newMap \sds->Task (evalinit sds)
where
	evalinit _ DestroyEvent _ iworld
		= (DestroyedResult, iworld)
	evalinit sds event {taskId,lastEval} iworld
		# (mbError, iworld) = addListener taskId port True (wrapIWorldConnectionTask handlers sds) iworld
		| mbError=:(Error _) = showException "initialization" (fromError mbError) iworld
		# iworld = iShow ["SDS server listening on " +++ toString port] iworld
		= (ValueResult
			(Value () False)
			(mkTaskEvalInfo lastEval)
			(mkUIIfReset event (ui UIEmpty))
			(Task eval)
		, iworld)
	where
		showException base taskException=:(_, str) iworld
			# iworld = iShow ["SDSService exception during " +++ base +++ ": " +++ str] iworld
			= (ExceptionResult taskException, iworld)

		eval DestroyEvent evalOpts iworld=:{ioStates}
			# ioStates = case 'Map'.get taskId ioStates of
				Just (IOActive values) = 'Map'.put taskId (IODestroyed values) ioStates
				_                      = ioStates
        	= (DestroyedResult, {iworld & ioStates = ioStates})
		eval (RefreshEvent taskIds) {lastEval} iworld
			| not ('Set'.member taskId taskIds)
				= (ValueResult
					(Value () False)
					(mkTaskEvalInfo lastEval)
					NoChange
					(Task eval)
				, iworld)
			# (readResult, iworld) = read sds EmptyContext iworld
			| readResult=:(Error _) = showException "read from share value" (fromError readResult) iworld
			# shareValue = 'Map'.toList (directResult (fromOk readResult))
			# (results, iworld) = reevaluateShares shareValue iworld
			| results=:(Error _) = showException "re-evaluating share values" (exception (fromError results)) iworld
			# (writeResult, iworld) = write ('Map'.fromList (fromOk results)) sds EmptyContext iworld
			| writeResult=:(Error _) = showException "writing result share values" (fromError writeResult) iworld
			= (ValueResult
				(Value () False)
				(mkTaskEvalInfo lastEval)
				NoChange
				(Task eval)
				, iworld)
		eval ResetEvent {lastEval} iworld
			= (ValueResult
				(Value () False)
				(mkTaskEvalInfo lastEval)
				NoChange
				(Task eval)
				, iworld)
		eval event _ iworld
			= (ExceptionResult (dynamic event, "Unknown event in sdsServiceTask"), iworld)

		handlers =
			{ ConnectionHandlersIWorld
			| onConnect = onConnect
			, onData = onData
			, onShareChange = onShareChange
			, onTick = onTick
			, onDisconnect = onDisconnect
			, onDestroy = \s iw->(Ok s, [], iw)
			}
		onConnect :: !ConnectionId !String !SDSEvaluations !*IWorld -> *(!MaybeErrorString SDSServiceState, Maybe SDSEvaluations, ![String], !Bool, !*IWorld)
		onConnect connId clientName sdsValue iworld = (Ok (SDSProcessing clientName connId []), Nothing, [], False, iworld)
	
		onData :: !String !SDSServiceState !SDSEvaluations !*IWorld -> *(!MaybeErrorString SDSServiceState, Maybe SDSEvaluations, ![String], !Bool, !*IWorld)
		onData receivedData state=:(SDSProcessing host connId received) sdsValue iworld
		| not (endsWith "\n" receivedData) = (Ok (SDSProcessing host connId (received ++ [receivedData])), Nothing, [], False, iworld)
		# receivedData = concat (received ++ [receivedData])
		= case performRequest host receivedData iworld of
			(Error e, iworld)
				# exception = serializeToBase64 $ Error $ exception $ "Exception onData:" +++ e
				= (Ok state, Nothing, [exception], True, iworld)
			(Ok (Left response), iworld) 		= (Ok state, Nothing, [response +++ "\n"], True, iworld)
			(Ok (Right continuation), iworld=:{ioStates})
			# sdsValue = 'Map'.put connId (False, host, continuation) sdsValue
			= (Ok state, Just sdsValue, [], False, {iworld & ioStates = ioStates})
	
		onShareChange :: !SDSServiceState !SDSEvaluations !*IWorld -> *(!MaybeErrorString SDSServiceState, Maybe SDSEvaluations, ![String], !Bool, !*IWorld)
		onShareChange state=:(SDSProcessing host connId _) sdsValue iworld = case 'Map'.get connId sdsValue of
			Nothing = (Ok state, Nothing, [], False, iworld)
			// Not yet completed evaluating the sds, do nothing.
			Just (False, _, result) = (Ok state, Nothing, [], False, iworld)
			// We have completed evaluating the SDS, send the result to the client.
			Just (True, _, result) = (Ok state, Just ('Map'.del connId sdsValue), [result +++ "\n"], True, iworld)
	
		onTick        :: !SDSServiceState !SDSEvaluations !*IWorld -> *(!MaybeErrorString SDSServiceState, Maybe SDSEvaluations, ![String], !Bool, !*IWorld)
		onTick state sdsValue iworld = (Ok state, Nothing, [], False, iworld)
	
		onDisconnect  :: !SDSServiceState !SDSEvaluations !*IWorld -> *(!MaybeErrorString SDSServiceState, Maybe SDSEvaluations, !*IWorld)
		onDisconnect state=:(SDSProcessing host connId _) sdsValue iworld = (Ok state, Just ('Map'.del connId sdsValue), iworld)

		reevaluateShares :: ![(ConnectionId, (Bool, String, String))] *IWorld -> (MaybeErrorString [(ConnectionId, (Bool, String, String))], *IWorld)
		reevaluateShares evals iworld = reevaluateShares` evals [] iworld
		where
			reevaluateShares` [] acc iworld = (Ok acc, iworld)
			reevaluateShares` [(connId, (done, host, val)):rest] acc iworld
			# (result, iworld) = performRequest host val iworld
			= case result of
				Error e
					# exception = serializeToBase64 $ Error $ exception $ "Exception reevaluateShares: " +++ e
					= reevaluateShares` rest [(connId, (True, host, exception)) : acc] iworld
				Ok (Left val)	= reevaluateShares` rest [(connId, (True, host, val)) : acc] iworld
				Ok (Right val)	= reevaluateShares` rest [(connId, (False, host, val)) : acc] iworld

		// Left: Done
		// Right: Still need to do work
		performRequest :: !String !String !*IWorld -> (MaybeErrorString (Either String String), !*IWorld)
		performRequest host request iworld=:{symbols}
			| size request == 0 = (Error "Received empty request", iworld)
			| newlines (fromString request) > 1 = (Error ("Received multiple requests (only one is allowed): " +++ request), iworld)
			= case deserializeFromBase64 request symbols of
				SDSReadRequest sds p = case readSDS sds p (TaskContext taskId) iworld of
					(ReadResult v _, iworld)       = (Ok (Left (serializeToBase64 (Ok v))), iworld)
					(AsyncRead sds, iworld)        = (Ok (Right (serializeToBase64 (SDSReadRequest sds p))), iworld)
					(ReadException (_, e), iworld) = (Error e, iworld)
				SDSRegisterRequest sds p reqSDSId remoteSDSId reqTaskId port = case readRegisterSDS sds p (RemoteTaskContext reqTaskId taskId remoteSDSId host port) taskId reqSDSId iworld of
					(ReadResult v _, iworld)       = (Ok (Left (serializeToBase64 (Ok v))), iworld)
					(AsyncRead sds, iworld)        = (Ok (Right (serializeToBase64 (SDSRegisterRequest sds p reqSDSId remoteSDSId taskId port))), iworld)
					(ReadException (_, e), iworld) = (Error e, iworld)
				SDSWriteRequest sds p val = case writeSDS sds p (TaskContext taskId) val iworld of
					(WriteResult notify _, iworld)  = (Ok (Left (serializeToBase64 (Ok ()))), queueNotifyEvents (sdsIdentity sds) notify iworld)
					(AsyncWrite sds, iworld)        = (Ok (Right (serializeToBase64 (SDSWriteRequest sds p val))), iworld)
					(WriteException (_, e), iworld) = (Error e, iworld)
				SDSModifyRequest sds p f = case modifySDS f sds p (TaskContext taskId) iworld of
					(ModifyResult notify r w _, iworld) = (Ok (Left (serializeToBase64 (Ok (r,w)))), queueNotifyEvents (sdsIdentity sds) notify iworld)
					(AsyncModify sds f, iworld)         = (Ok (Right (serializeToBase64 (SDSModifyRequest sds p f))), iworld)
					(ModifyException (_, e), iworld)    = (Error e, iworld)
				SDSRefreshRequest refreshTaskId sdsId
				// If we receive a request to refresh the sds service task, we find all remote
				// registrations for the SDS id and send requests to refresh them to their respective clients.
				| taskId == refreshTaskId = refreshRemoteTasks sdsId iworld
				= (Ok (Left "Refresh queued"), queueRefresh refreshTaskId iworld)
		where
			newlines [] = 0
			newlines ['\n':xs] = inc (newlines xs)
			newlines [x: xs] = newlines xs
	
			refreshRemoteTasks sdsId iworld=:{sdsNotifyRequests}
				= case 'Map'.get sdsId.id_hash sdsNotifyRequests of
					Nothing = (Ok (Left ("No requests available")), iworld)
					Just requestsToTime =
						( Ok (Left "Requests re-queued")
						, queueNotifyEvents
							sdsId
							('Set'.fromList $ (\req -> (req.reqTaskId, req.remoteOptions)) <$> 'Map'.keys requestsToTime)
							iworld
						)
