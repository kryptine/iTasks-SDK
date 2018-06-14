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
import iTasks.Internal.IWorld

from StdFunc import o
import StdString, StdList, StdArray
import qualified Data.Map as DM
import Data.Maybe, Data.Error
import Text.GenJSON, Text.URI
import StdMisc, graph_to_sapl_string
import Data.Queue, Data.Functor
 
import iTasks.Extensions.Distributed._Formatter
import iTasks.SDS.Definition
import iTasks.Internal.Distributed.Symbols

from iTasks.Internal.TaskStore import queueRefresh
import StdDebug
import qualified Data.Set as Set
import Text.GenPrint

derive gPrint HTTPRequest, Map, HTTPUpload, HTTPMethod, HTTPProtocol

derive JSONEncode SDSNotifyRequest, RemoteNotifyOptions

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
		(Ok (ReadResult symbols _), iworld) = (readSymbols symbols, iworld)
	= case deserializeFromBase64 req_data symbols of
		(SDSReadRequest sds p)							= case readSDS sds p EmptyContext Nothing (sdsIdentity sds) iworld of
				(Error (_, e), iworld) 						= (errorResponse e, Nothing, Nothing, iworld)
				(Ok (ReadResult v _), iworld)				= trace_n ("Got read") (base64Response (serializeToBase64 v), Nothing, Nothing, iworld)
		(SDSRegisterRequest sds p reqSDSId taskId port)	= case readSDS sds p (RemoteTaskContext taskId server_name port) (Just taskId) reqSDSId iworld of
				(Error (_, e), iworld) 						= (errorResponse e, Nothing, Nothing, iworld)
				(Ok (ReadResult v _), iworld)				= trace_n ("Got register") (base64Response (serializeToBase64 v), Nothing, Nothing, iworld)
		(SDSWriteRequest sds p val)						= case writeSDS sds p EmptyContext val iworld of
				(Error (_, e), iworld) 						= (errorResponse e, Nothing, Nothing, iworld)
				(Ok (WriteResult notify _), iworld)			= trace_n "Got write" (base64Response (serializeToBase64 ()), Nothing, Nothing, queueNotifyEvents (sdsIdentity sds) notify iworld)
		(SDSModifyRequest sds p f)						= case modifySDS f sds p EmptyContext iworld of
				(Error (_, e), iworld) 						= (errorResponse e, Nothing, Nothing, iworld)
				(Ok (ModifyResult r w _), iworld)			= trace_n ("Got modify") (base64Response (serializeToBase64 (r,w)), Nothing, Nothing, iworld)
		(SDSRefreshRequest taskId sdsId)
			# iworld = (queueRefresh [(taskId, "Notification for remote write of " +++ sdsId)] iworld)
			= (plainResponse "Refresh queued", Nothing, Nothing, iworld)	

	plainResponse string
		= {okResponse & rsp_headers = [("Content-Type","text/plain"), ("Content-Length", toString (size string))], rsp_data = string}

	base64Response string = {okResponse & rsp_headers = [("Content-Type","text/plain;base64"), ("Content-Length", toString (size string))], rsp_data = string}
				
    dataFun req _ data instanceNo iworld =  ([], True, instanceNo, Nothing, iworld)

    onShareChange _ _ s iworld = ([], True, s, Nothing, iworld)
    onTick _ _ instanceNo iworld =([], True, instanceNo, Nothing, iworld)
	disconnectFun _ _ _ iworld = (Nothing,iworld)
