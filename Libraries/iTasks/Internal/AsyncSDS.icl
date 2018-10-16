implementation module iTasks.Internal.AsyncSDS

import Data.Maybe, Data.Either, Data.List, Data.Func
import Text, Text.GenJSON
import StdMisc, StdArray
import Internet.HTTP

import iTasks.Engine
import iTasks.Internal.Distributed.Symbols
import iTasks.Internal.IWorld
import iTasks.Internal.SDS
import iTasks.Internal.Task
import iTasks.SDS.Definition
import iTasks.WF.Tasks.IO

import iTasks.Extensions.Distributed._Formatter

from iTasks.Internal.TaskServer import addConnection
from iTasks.SDS.Sources.Core import unitShare
import iTasks.Internal.SDSService

import qualified Data.Map as DM

derive JSONEncode SDSNotifyRequest, RemoteNotifyOptions

createRequestString req = serializeToBase64 req

onConnect reqq connId _ _ = (Ok (Left []), Nothing, [createRequestString reqq +++ "\n"], False)

onData data (Left acc) _ = (Ok (Left (acc ++ [data])), Nothing, [], False)

onShareChange acc _ = (Ok acc, Nothing, [], False)

queueSDSRequest :: !(SDSRequest p r w) !String !Int !TaskId !{#Symbol} !*IWorld -> (!MaybeError TaskException ConnectionId, !*IWorld) | TC r
queueSDSRequest req host port taskId symbols env
= case addConnection taskId host port connectionTask env of
	(Error e, env)  		= (Error e, env)
	(Ok (id, _), env)     	= (Ok id, env)
where
	connectionTask = wrapConnectionTask (handlers req) unitShare

	handlers :: (SDSRequest p r w) -> ConnectionHandlers (Either [String] (MaybeError TaskException r)) () () | TC r
	handlers _ = {ConnectionHandlers| onConnect = onConnect req,
		onData = onData,
		onShareChange = onShareChange,
		onDisconnect = onDisconnect}

	onDisconnect (Left acc) _
	# textResponse = concat acc
	| size textResponse < 1 = (Error ("queueSDSRequest: Server " +++ host +++ " disconnected without responding"), Nothing)
	= (Ok $ Right $ deserializeFromBase64 textResponse symbols, Nothing)

queueModifyRequest :: !(SDSRequest p r w) !String !Int !TaskId !{#Symbol} !*IWorld -> (!MaybeError TaskException ConnectionId, !*IWorld) | TC r & TC w
queueModifyRequest req=:(SDSModifyRequest p r w) host port taskId symbols env = case addConnection taskId host port connectionTask env of
	(Error e, env)          = (Error e, env)
	(Ok (id, _), env)       = (Ok id, env)
where
	connectionTask = wrapConnectionTask (handlers req) unitShare

	handlers :: (SDSRequest p r w) -> ConnectionHandlers (Either [String] (MaybeError TaskException (r, w))) () () | TC r & TC w
	handlers _ = {ConnectionHandlers| onConnect = onConnect req,
		onData = onData,
		onShareChange = onShareChange,
		onDisconnect = onDisconnect}

	onDisconnect (Left acc) _
	# textResponse = concat acc
	| size textResponse == 0 = (Error ("queueModifyRequest: Server" +++ host +++ " disconnected without responding"), Nothing)
	= (Ok $ Right $ deserializeFromBase64 textResponse symbols, Nothing)

queueServiceRequest :: !(SDSRemoteService p r w) p !TaskId !*IWorld -> (!MaybeError TaskException ConnectionId, !*IWorld) | gText{|*|} p & TC p & TC r
queueServiceRequest (SDSRemoteService (HttpShareOptions req parse)) p taskId env = case addConnection taskId req.server_name req.server_port connectionTask env of
	(Error e, env) = (Error e, env)
	(Ok (id, _), env) = (Ok id, env)
where
	connectionTask = wrapConnectionTask handlers unitShare

	handlers  = {ConnectionHandlers| onConnect = onConnect,
		onData = onData,
		onShareChange = onShareChange,
		onDisconnect = onDisconnect}

	onConnect _ _ _ = (Ok (Left []), Nothing, [toString {HTTPRequest|req & req_headers = 'DM'.put "Connection" "Close" req.HTTPRequest.req_headers}], False)

	onData data (Left acc) _ = (Ok (Left (acc ++ [data])), Nothing, [], False)

	onShareChange acc _ = (Ok acc, Nothing, [], False)

	onDisconnect (Left acc) _
	# textResponse = concat acc
	| size textResponse == 0 = (Error ("queueServiceRequest: Server" +++ req.server_name +++ ":" +++ toString req.server_port +++ " disconnected without responding"), Nothing)
	= case parseResponse textResponse of
		Nothing = (Error ("Unable to parse HTTP response, got: " +++ textResponse), Nothing)
		(Just parsed) = case parse parsed of
			(Left error) = (Error error, Nothing)
			(Right a) = (Ok (Right a), Nothing)

queueRead :: !(SDSRemoteSource p r w) p !TaskId !Bool !SDSIdentity !*IWorld -> (!MaybeError TaskException ConnectionId, !*IWorld) | gText{|*|} p & TC p & TC r & TC w
queueRead rsds=:(SDSRemoteSource sds {SDSShareOptions|domain, port}) p taskId register reqSDSId env
# (symbols, env) = case read symbolsShare EmptyContext env of
	(Ok (ReadingDone r), env) = (readSymbols r, env)
	_ = abort "Reading symbols failed!"
# (request, env) = buildRequest register env
= queueSDSRequest request domain port taskId symbols env
where
	buildRequest True env=:{options}= (SDSRegisterRequest sds p reqSDSId (sdsIdentity rsds) taskId options.sdsPort, env)
	buildRequest False env = (SDSReadRequest sds p, env)

queueRemoteRefresh :: ![SDSNotifyRequest] !*IWorld -> *IWorld
queueRemoteRefresh [] iworld = iworld
queueRemoteRefresh [notifyRequest : reqs] iworld=:{options}
# (symbols, iworld) = case read symbolsShare EmptyContext iworld of
	(Ok (ReadingDone r), iworld) = (readSymbols r, iworld)
# (host, port, sdsId) = case notifyRequest.remoteOptions of
	(Just {hostToNotify, portToNotify, remoteSdsId}) = (hostToNotify, portToNotify, remoteSdsId)
# request = reqq notifyRequest.reqTaskId sdsId
= case queueSDSRequest request host port SDSSERVICE_TASK_ID symbols iworld of
	(_, iworld) = queueRemoteRefresh reqs iworld
where
	// Hack to get it to compile. The Refresh Request alternative does not use any of the parameters.
	reqq :: !TaskId !SDSIdentity -> SDSRequest () String ()
	reqq taskId sdsId = SDSRefreshRequest taskId sdsId

queueWrite :: !w !(SDSRemoteSource p r w) p !TaskId !*IWorld -> (!MaybeError TaskException ConnectionId, !*IWorld) | gText{|*|} p & TC p & TC r & TC w
queueWrite w rsds=:(SDSRemoteSource sds share=:{SDSShareOptions|domain, port}) p taskId env
# (symbols, env) = case read symbolsShare EmptyContext env of
	(Ok (ReadingDone r), env) = (readSymbols r, env)
# request = SDSWriteRequest sds p w
= queueSDSRequest request domain port taskId symbols env

queueModify :: !(r -> MaybeError TaskException w) !(SDSRemoteSource p r w) p !TaskId !*IWorld -> (!MaybeError TaskException ConnectionId, !*IWorld) | gText{|*|} p & TC p & TC r & TC w
queueModify f rsds=:(SDSRemoteSource sds share=:{SDSShareOptions|domain, port}) p taskId env
# (symbols, env) = case read symbolsShare EmptyContext env of
	(Ok (ReadingDone r), env) = (readSymbols r, env)
# request = SDSModifyRequest sds p f
= queueModifyRequest request domain port taskId symbols env

getAsyncReadValue :: !(sds p r w) !TaskId !ConnectionId IOStates -> MaybeError TaskException (Maybe r) | TC r
getAsyncReadValue _ taskId connectionId ioStates
=  case 'DM'.get taskId ioStates of
		Nothing                             = Error (exception "No iostate for this task")
		(Just ioState)                      = case ioState of
			(IOException exc)                   = Error (exception exc)
			(IOActive connectionMap)            = getValue connectionId connectionMap
			(IODestroyed connectionMap)         = getValue connectionId connectionMap
where
	getValue connectionId connectionMap = case 'DM'.get connectionId connectionMap of
		(Just (value :: Either [String] (MaybeError TaskException r^), _)) = case value of
			(Left _)                                = Ok Nothing
			(Right (Ok val))                        = Ok (Just val)
			(Right (Error e))						= Error e
		(Just (dyn, _))							= Error (exception ("Dynamic not of the correct write type, got" +++ toString (typeCodeOfDynamic dyn)))
		Nothing                             	= Ok Nothing

getAsyncWriteValue :: !(sds p r w) !TaskId !ConnectionId IOStates -> MaybeError TaskException (Maybe w) | TC w
getAsyncWriteValue _ taskId connectionId ioStates =  case 'DM'.get taskId ioStates of
		Nothing                             = Error (exception "No iostate for this task")
		(Just ioState)                      = case ioState of
			(IOException exc)                   = Error (exception exc)
			(IOActive connectionMap)            = getValue connectionId connectionMap
			(IODestroyed connectionMap)         = getValue connectionId connectionMap
where
	getValue connectionId connectionMap = case 'DM'.get connectionId connectionMap of
		(Just (value :: Either [String] (MaybeError TaskException w^), _)) = case value of
			(Left _)                                    = Ok Nothing
			(Right (Ok val))                            = Ok (Just val)
			(Right (Error e))							= Error e
		(Just (dyn, _))						= Error (exception ("Dynamic not of the correct write type, got" +++ toString (typeCodeOfDynamic dyn)))
		Nothing                             = Ok Nothing

getAsyncModifyValue :: !(sds p r w) !TaskId !ConnectionId IOStates -> MaybeError TaskException (Maybe (r,w)) | TC w & TC r
getAsyncModifyValue _ taskId connectionId ioStates =  case 'DM'.get taskId ioStates of
		Nothing                             = Error (exception "No iostate for this task")
		(Just ioState)                      = case ioState of
			(IOException exc)                   = Error (exception exc)
			(IOActive connectionMap)            = getValue connectionId connectionMap
			(IODestroyed connectionMap)         = getValue connectionId connectionMap
where
	getValue connectionId connectionMap
	= case 'DM'.get connectionId connectionMap of
		(Just (value :: Either [String] (MaybeError TaskException (r^, w^)), _)) = case value of
			(Left _)						= Ok Nothing
			(Right (Ok val))				= Ok (Just val)
			(Right (Error e))				= Error e
		(Just (dyn, _))					= Error (exception ("Dynamic not of the correct modify type, got " +++ toString (typeCodeOfDynamic dyn)))
		Nothing 						= Ok Nothing