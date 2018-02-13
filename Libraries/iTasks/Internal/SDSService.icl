implementation module iTasks.Internal.SDSService

import iTasks

from Internet.HTTP					import :: HTTPRequest {req_method, req_path, req_data}, :: HTTPResponse(..), :: HTTPMethod(..)
from iTasks.Internal.WebService   import :: ConnectionState, :: WebSockState, :: WebService(..)
from iTasks.Internal.TaskState 	import :: TIUIState

import iTasks.Internal.HtmlUtil, iTasks.Internal.DynamicUtil
import iTasks.Internal.RemoteAccess
from iTasks.Extensions.Web import callHTTP
import iTasks.Internal.SDS
import iTasks.Internal.AsyncSDS

from StdFunc import o
import StdString, StdList
import qualified Data.Map as DM
import Data.Maybe, Data.Error
import Text.GenJSON, Text.URI
import StdMisc, graph_to_sapl_string
import Data.Queue, Data.Functor
 
import iTasks.Extensions.Distributed._Formatter
import iTasks.SDS.Definition
import iTasks.Internal.Distributed.Symbols

from iTasks.Internal.TaskStore import queueRefresh

sdsService :: WebService a a
sdsService = { urlMatchPred    = matchFun
             , completeRequest = True
             , onNewReq        = reqFun
             , onData          = dataFun
             , onShareChange   = onShareChange
             , onTick          = onTick
             , onDisconnect    = disconnectFun
             }
where
    matchFun :: String -> Bool
    matchFun reqUrl = case pathToSegments reqUrl of
    					["","sds",_] = True
    							  	 = False

	reqFun :: !HTTPRequest a !*IWorld -> *(!HTTPResponse, !Maybe ConnectionState, !Maybe a, !*IWorld)	
	reqFun req=:{req_data, server_name} _ iworld
	# (symbols, iworld) = case read symbolsShare EmptyContext iworld of
		(Ok (Result symbols), iworld) = (readSymbols symbols, iworld)
	= case deserializeFromBase64 req_data symbols of
		(SDSReadRequest sds Nothing) 				= case read sds EmptyContext iworld of
			(Error (_, e), iworld) 						= (errorResponse e, Nothing, Nothing, iworld)
			(Ok (Result v), iworld)						= (base64Response (serializeToBase64 v), Nothing, Nothing, iworld)
		// TODO : FIX PORT!
		(SDSReadRequest sds (Just (taskId, port))) 	= case read sds (RemoteTaskContext taskId server_name port) iworld of
			(Error (_, e), iworld) 						= (errorResponse e, Nothing, Nothing, iworld)
			(Ok (Result v), iworld)						= (base64Response (serializeToBase64 v), Nothing, Nothing, iworld)
		(SDSWriteRequest sds val)					= case write val sds EmptyContext iworld of
			(Error (_, e), iworld) 						= (errorResponse e, Nothing, Nothing, iworld)
			(Ok (), iworld)								= (base64Response (serializeToBase64 ()), Nothing, Nothing, iworld)
		(SDSModifyRequest sds f)					= case modify f sds EmptyContext iworld of
			(Error (_, e), iworld) 						= (errorResponse e, Nothing, Nothing, iworld)
			(Ok (Result v), iworld)						= (base64Response (serializeToBase64 v), Nothing, Nothing, iworld)
		(SDSRefreshRequest taskId sdsId)
			# iworld = (queueRefresh [(taskId, "Notification for remote write of " +++ sdsId)] iworld)
			= (plainResponse "Refresh queued", Nothing, Nothing, iworld)	

	plainResponse string
		= {okResponse & rsp_headers = [("Content-Type","text/plain")], rsp_data = string}

	base64Response string = {okResponse & rsp_headers = [("Content-Type","text/plain;base64")], rsp_data = string}
				
    dataFun req _ data instanceNo iworld = ([], True, instanceNo, Nothing, iworld)

    onShareChange _ _ s iworld = ([], True, s, Nothing, iworld)
    onTick _ _ instanceNo iworld = ([], True, instanceNo, Nothing, iworld)

    disconnectFun :: !HTTPRequest a !ConnectionState !*IWorld -> (!Maybe a, !*IWorld)
	disconnectFun _ _ _ iworld = (Nothing,iworld)