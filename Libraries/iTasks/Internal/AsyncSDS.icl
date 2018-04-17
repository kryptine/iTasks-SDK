implementation module iTasks.Internal.AsyncSDS

import Data.Maybe, Data.Either, Data.List
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

import qualified Data.Map as DM
import StdDebug

derive JSONEncode SDSNotifyRequest, RemoteNotifyOptions

queueSDSRequest :: !(SDSRequest p r w) !String !Int !TaskId !{#Symbol} !*IWorld -> (!MaybeError TaskException ConnectionId, !*IWorld) | TC r
queueSDSRequest req host port taskId symbols env = case addConnection taskId host port connectionTask env of
    (Error e, env)  = (Error e, env)
    (Ok (id, _), env)     = (Ok id, env)
where
    connectionTask = wrapConnectionTask (handlers req) unitShare

    handlers :: (SDSRequest p r w) -> ConnectionHandlers (Either [String] r) () ()
    handlers _ = {ConnectionHandlers| onConnect = onConnect,
        onData = onData,
        onShareChange = onShareChange,
        onDisconnect = onDisconnect}

    headers = 'DM'.fromList [("Connection", "Close"), ("Content-Length", toString (size data))]
    data = serializeToBase64 req
    request = toString {newHTTPRequest & server_name = host, server_port = port, req_path =  "/sds/", req_version = "HTTP/1.1", req_data = data, req_headers = headers}

    onConnect _ _ 
    | not (trace_tn ("Do request:====================\n" +++ request +++ "\n====================")) = undef
    = (Ok (Left []), Nothing, [request], False) 

    onData data (Left acc) _ = (Ok (Left (acc ++ [data])), Nothing, [], False)

    onShareChange acc _ = (Ok acc, Nothing, [], False)

    onDisconnect (Left acc) _
    # rawResponse = concat acc
    = case parseResponse rawResponse of
        Nothing = (Error ("Unable to parse HTTP response, got: " +++ rawResponse), Nothing)
        (Just parsed) = (Ok (Right (deserializeFromBase64 parsed.rsp_data symbols)), Nothing)

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

    onConnect _ _ = (Ok (Left []), Nothing, [toString {HTTPRequest|req & req_headers = 'DM'.put "Connection" "Keep-Alive" req.HTTPRequest.req_headers}], False) 

    onData data (Left acc) _ = (Ok (Left (acc ++ [data])), Nothing, [], False)

    onShareChange acc _ = (Ok acc, Nothing, [], False)

    onDisconnect (Left acc) _
    # rawResponse = concat acc
    = case parseResponse rawResponse of
        Nothing = (Error ("Unable to parse HTTP response, got: " +++ rawResponse), Nothing)
        (Just parsed) = case parse parsed of
            (Left error) = (Error error, Nothing)
            (Right a) = (Ok (Right a), Nothing)

queueRead :: !(SDSRemoteSource p r w) p !TaskId !Bool !SDSIdentity !*IWorld -> (!MaybeError TaskException ConnectionId, !*IWorld) | gText{|*|} p & TC p & TC r & TC w
queueRead rsds=:(SDSRemoteSource sds {SDSShareOptions|domain, port}) p taskId register reqSDSId env
# (symbols, env) = case read symbolsShare EmptyContext env of
    (Ok (ReadResult r), env) = (readSymbols r, env)
    _ = abort "Reading symbols failed!"
# (request, env) = buildRequest register env
= queueSDSRequest request domain port taskId symbols env
where
    buildRequest True env=:{options}= (SDSRegisterRequest sds p reqSDSId taskId options.serverPort, env)
    buildRequest False env = (SDSReadRequest sds p, env) 

queueRemoteRefresh :: !SDSIdentity [SDSNotifyRequest] !*IWorld -> *IWorld
queueRemoteRefresh _ [] iworld = iworld
queueRemoteRefresh sdsId [notifyRequest : reqs] iworld
# (symbols, iworld) = case read symbolsShare EmptyContext iworld of
    (Ok (ReadResult r), iworld) = (readSymbols r, iworld)
# request = reqq notifyRequest.reqTaskId sdsId
# (host, port) = case notifyRequest.remoteOptions of
    (Just (RemoteNotifyOptions host port)) = (host, port)
= case queueSDSRequest request host port notifyRequest.reqTaskId symbols iworld of 
    (_, iworld) = queueRemoteRefresh sdsId reqs iworld
where
    // Hack to get it to compile. The Refresh Request alternative does not use any of the parameters.
    reqq :: !TaskId !SDSIdentity -> SDSRequest () String ()
    reqq taskId sdsId = SDSRefreshRequest taskId sdsId

queueWrite :: !w !(SDSRemoteSource p r w) p !TaskId !*IWorld -> (!MaybeError TaskException ConnectionId, !*IWorld) | gText{|*|} p & TC p & TC r & TC w
queueWrite w rsds=:(SDSRemoteSource sds share=:{SDSShareOptions|domain, port}) p taskId env
# (symbols, env) = case read symbolsShare EmptyContext env of
    (Ok (ReadResult r), env) = (readSymbols r, env)
# request = SDSWriteRequest sds p w
= queueSDSRequest request domain port taskId symbols env

queueModify :: !(r -> MaybeError TaskException w) !(SDSRemoteSource p r w) p !TaskId !*IWorld -> (!MaybeError TaskException ConnectionId, !*IWorld) | gText{|*|} p & TC p & TC r & TC w
queueModify f rsds=:(SDSRemoteSource sds share=:{SDSShareOptions|domain, port}) p taskId env
# (symbols, env) = case read symbolsShare EmptyContext env of
    (Ok (ReadResult r), env) = (readSymbols r, env)
# request = SDSModifyRequest sds p f
= queueSDSRequest request domain port taskId symbols env

getAsyncReadValue :: !(sds p r w) !TaskId !ConnectionId IOStates -> Either String (Maybe r) | TC r
getAsyncReadValue _ taskId connectionId ioStates =  case 'DM'.get taskId ioStates of        
        Nothing                             = Left "No iostate for this task"
        (Just ioState)                      = case ioState of
            (IOException exc)                   = Left exc
            (IOActive connectionMap)            = getValue connectionId connectionMap
            (IODestroyed connectionMap)         = getValue connectionId connectionMap
where
    getValue connectionId connectionMap = case 'DM'.get connectionId connectionMap of
        (Just (value :: Either [String] r^, _)) = case value of
            (Left _)                                    = Right Nothing
            (Right val)                                 = (Right (Just val))
        (Just _)= Left "Dynamic not of the correct type"

getAsyncWriteValue :: !(sds p r w) !TaskId !ConnectionId IOStates -> Either String (Maybe w) | TC w
getAsyncWriteValue _ taskId connectionId ioStates =  case 'DM'.get taskId ioStates of
        Nothing                             = Left "No iostate for this task"
        (Just ioState)                      = case ioState of
            (IOException exc)                   = Left exc
            (IOActive connectionMap)            = getValue connectionId connectionMap
            (IODestroyed connectionMap)         = getValue connectionId connectionMap
where
    getValue connectionId connectionMap = case 'DM'.get connectionId connectionMap of
        (Just (value :: Either [String] w^, _)) = case value of
            (Left _)                                    = Right Nothing
            (Right val)                                 = (Right (Just val))
        (Just _)= Left "Dynamic not of the correct type"