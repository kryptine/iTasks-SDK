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

queueWriteRequest :: !(SDSRequest p r w) !String !Int !TaskId !{#Symbol} !*IWorld ->  (!MaybeError TaskException ConnectionId, !*IWorld) | TC r & TC w
queueWriteRequest req=:(SDSWriteRequest sds p w) host port taskId symbols env = case addConnection taskId host port connectionTask env of
	(Error e, env)          = (Error e, env)
	(Ok (id, _), env)       = (Ok id, env)
where
	connectionTask = wrapConnectionTask (handlers req) unitShare

	handlers :: (SDSRequest p r w) -> ConnectionHandlers (Either [String] (MaybeError TaskException ())) () () | TC r & TC w
	handlers req = {ConnectionHandlers| onConnect = onConnect req,
		onData = onData,
		onShareChange = onShareChange,
		onDisconnect = onDisconnect}

	onDisconnect (Left acc) _
	# textResponse = concat acc
	| size textResponse == 0 = (Error ("queueWriteRequest: Server" +++ host +++ " disconnected without responding"), Nothing)
	= (Ok $ Right $ deserializeFromBase64 textResponse symbols, Nothing)

import StdDebug, StdMisc
queueServiceRequest :: !(SDSRemoteService p r w) p !TaskId !Bool !*IWorld -> (!MaybeError TaskException ConnectionId, !*IWorld) | gText{|*|} p & TC p & TC r
queueServiceRequest service=:(SDSRemoteService (HTTPShareOptions {host, port, createRequest, fromResponse})) p taskId _ env
= case addConnection taskId host port connectionTask env of
	(Error e, env) = (Error e, env)
	(Ok (id, _), env) = (Ok id, env)
where
	connectionTask = wrapConnectionTask (handlers service) unitShare

	handlers req = {ConnectionHandlers| onConnect = onConnect,
		onData = onData,
		onShareChange = onShareChange,
		onDisconnect = onDisconnect}

	onConnect _ _ _
	# req = createRequest p
	# sreq = toString {HTTPRequest|req & req_headers = 'DM'.put "Connection" "Close" req.HTTPRequest.req_headers}
	= (Ok (Left []), Nothing, [sreq], False)

	onData data (Left acc) _ = (Ok (Left (acc ++ [data])), Nothing, [], False)

	onShareChange acc _ = (Ok acc, Nothing, [], False)

	onDisconnect (Left []) _ = (Error ("queueServiceRequest: Server" +++ host +++ ":" +++ toString port +++ " disconnected without responding"), Nothing)
	onDisconnect (Left acc) _
	# textResponse = concat acc
	= case parseResponse textResponse of
		Nothing = (Error ("Unable to parse HTTP response, got: " +++ textResponse), Nothing)
		(Just parsed) = case fromResponse parsed p of
			(Error error) = (Error error, Nothing)
			(Ok a) = (Ok (Right a), Nothing)

queueServiceRequest service=:(SDSRemoteService (TCPShareOptions {host, port, createMessage, fromTextResponse})) p taskId keepOpen env
= case addConnection taskId host port connectionTask env of
	(Error e, env) = (Error e, env)
	(Ok (id, _), env) = (Ok id, env)
where
	connectionTask = wrapConnectionTask (handlers service) unitShare
	handlers req = {ConnectionHandlers| onConnect = onConnect,
		onData = onData,
		onShareChange = onShareChange,
		onDisconnect = onDisconnect}

	onConnect connId _ _	= trace_n ("New TCP connection: " +++ toString connId) (Ok ([], []), Nothing, [createMessage p +++ "\n"], False)

	onData data (previous, acc) _
	# newacc = acc ++ [data]
	= case fromTextResponse (concat newacc) p of
		Error e = (Error e, Nothing, [], True)
		Ok Nothing = (Ok (previous, newacc), Nothing, [], False)
		Ok (Just r)
		# rrs = [r : previous]
		| not (trace_tn ("Number of r's: " +++ toString (length rrs))) = undef
		= (Ok (rrs, []), Nothing, [], False)

	onShareChange state _ = (Ok state, Nothing, [], False)
	onDisconnect state _ = (Ok state, Nothing)

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
= queueWriteRequest request domain port taskId symbols env

queueModify :: !(r -> MaybeError TaskException w) !(SDSRemoteSource p r w) p !TaskId !*IWorld -> (!MaybeError TaskException ConnectionId, !*IWorld) | gText{|*|} p & TC p & TC r & TC w
queueModify f rsds=:(SDSRemoteSource sds share=:{SDSShareOptions|domain, port}) p taskId env
# (symbols, env) = case read symbolsShare EmptyContext env of
	(Ok (ReadingDone r), env) = (readSymbols r, env)
# request = SDSModifyRequest sds p f
= queueModifyRequest request domain port taskId symbols env

getAsyncServiceValue :: !(SDSRemoteService p r w) !TaskId !ConnectionId IOStates -> MaybeError TaskException (Maybe r) | TC r & TC w & TC p
getAsyncServiceValue service taskId connectionId ioStates
# getValue = case service of
	(SDSRemoteService (HTTPShareOptions _)) = getValueHttp
	(SDSRemoteServiceQueued _ _ (HTTPShareOptions _)) = getValueHttp
	(SDSRemoteService (TCPShareOptions _)) = getValueTCP
	(SDSRemoteServiceQueued _ _ (TCPShareOptions _)) = getValueTCP
=  case 'DM'.get taskId ioStates of
		Nothing                             = Error (exception "No iostate for this task")
		(Just ioState)                      = case ioState of
			(IOException exc)                   = Error (exception exc)
			(IOActive connectionMap)            = getValue connectionId connectionMap
			(IODestroyed connectionMap)         = getValue connectionId connectionMap
where
	getValueHttp connectionId connectionMap = case 'DM'.get connectionId connectionMap of
		(Just (value :: Either [String] r^, _)) = case value of
			(Left _)                                = Ok Nothing
			(Right val)                     		= Ok (Just val)
		(Just (dyn, _))							= Error (exception ("Dynamic not of the correct service type, got: " +++ toString (typeCodeOfDynamic dyn) +++ ", required: " +++ toString (typeCodeOfDynamic (dynamic service))))
		Nothing                             	= Ok Nothing

	getValueTCP connectionId connectionMap
	| not (trace_tn ("Get value from TCP service for connection " +++ toString connectionId)) = undef
	= case 'DM'.get connectionId connectionMap of
		(Just (value :: ([r^], [String]), _))
			| not (trace_tn "Got some value..") = undef
			= case value of
				([], _)                                 = trace_n "No read value yet" (Ok Nothing)
				([r : rs],_)                     		= trace_n "Got value!!" Ok (Just r)
		(Just (dyn, _))							= Error (exception ("Dynamic not of the correct service type, got: " +++ toString (typeCodeOfDynamic dyn) +++ ", required: " +++ toString (typeCodeOfDynamic (dynamic service))))
		Nothing                             	= Ok Nothing

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
		(Just (dyn, _))							= Error (exception ("Dynamic not of the correct read type, got" +++ toString (typeCodeOfDynamic dyn)))
		Nothing                             	= Ok Nothing

getAsyncWriteValue :: !(sds p r w) !TaskId !ConnectionId IOStates -> MaybeError TaskException (Maybe ()) | TC w
getAsyncWriteValue _ taskId connectionId ioStates =  case 'DM'.get taskId ioStates of
		Nothing                             = Error (exception "No iostate for this task")
		(Just ioState)                      = case ioState of
			(IOException exc)                   = Error (exception exc)
			(IOActive connectionMap)            = getValue connectionId connectionMap
			(IODestroyed connectionMap)         = getValue connectionId connectionMap
where
	getValue connectionId connectionMap = case 'DM'.get connectionId connectionMap of
		(Just (value :: Either [String] (MaybeError TaskException ()), _)) = case value of
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