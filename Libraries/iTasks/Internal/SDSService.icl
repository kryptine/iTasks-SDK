implementation module iTasks.Internal.SDSService

import iTasks

from Internet.HTTP					import :: HTTPRequest {req_method, req_path, req_data}, :: HTTPResponse(..), :: HTTPMethod(..)
from iTasks.Internal.IWorld		import :: IWorld {exposedShares}
from iTasks.Internal.WebService   import :: ConnectionState, :: WebSockState, :: WebService(..)
from iTasks.Internal.TaskState 	import :: TIUIState

import iTasks.Internal.HtmlUtil, iTasks.Internal.DynamicUtil
import iTasks.Internal.RemoteAccess
import iTasks.Internal.SDS
from iTasks.Extensions.Web import callHTTP

from StdFunc import o
import StdString, StdList
import qualified Data.Map as DM
import Data.Maybe, Data.Error
import Text.JSON, Text.URI
import StdMisc, graph_to_sapl_string
import Data.Queue, Data.Functor
import StdDebug

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
    					["","sds",_] = trace_n "Handling SDS Service request" True
    							  	 = False

	reqFun :: !HTTPRequest a !*IWorld -> *(!HTTPResponse, !Maybe ConnectionState, !Maybe a, !*IWorld)
	reqFun req _ iworld | hasParam "client_session_id" req
		= abort "Shareds on clients are not supported yet"
	
	reqFun req _ iworld=:{exposedShares}
		# (sdsurl, iworld) = getURLbyId ((hd o tl o tl) (pathToSegments req.HTTPRequest.req_path)) iworld
		= case 'DM'.get sdsurl exposedShares of
			Nothing = trace_n ("Could not find share " +++ sdsurl +++ "\nAvailable exposed shares: " +++ (foldr (\l r. l +++ "\n" +++ r) "" ('DM'.keys exposedShares))) (notFoundResponse req,Nothing,Nothing, iworld) 
			(Just (dyn, jsonShared)) = case read jsonShared EmptyContext iworld of
				(Ok (Result json), iworld) = (jsonResponse json, Nothing, Nothing, iworld)
				(Error (e,msg), iworld) = (errorResponse msg, Nothing, Nothing, iworld)
	
	jsonResponse json
		= {okResponse & rsp_headers = [("Content-Type","text/json"), ("Connection", "close")], rsp_data = toString json +++ "\r\n\r\n"}			

	plainResponse string
		= {okResponse & rsp_headers = [("Content-Type","text/plain")], rsp_data = string}			
				
	dataFun :: !HTTPRequest a !String !ConnectionState !*IWorld -> (![{#Char}], !Bool, !ConnectionState,!Maybe a, !*IWorld)
    dataFun req _ data instanceNo iworld = ([], True, instanceNo, Nothing, iworld)

    onShareChange _ _ s iworld = ([], True, s, Nothing, iworld)
    onTick _ _ instanceNo iworld = ([], True, instanceNo, Nothing, iworld)

    disconnectFun :: !HTTPRequest a !ConnectionState !*IWorld -> (!Maybe a, !*IWorld)
	disconnectFun _ _ _ iworld = (Nothing,iworld)

readRemoteSDS  :: !String !*IWorld -> *(!MaybeErrorString JSONNode, !*IWorld)
readRemoteSDS url iworld=:{exposedShares}
	= case convertURL url Nothing of
		(Ok uri) 	= load uri iworld
		(Error e) 	= (Error e, iworld)
where
	load uri iworld
		# (response, iworld) = httpRequest HTTP_GET uri Nothing iworld
		= if (isOkResponse response)
					(Ok (fromString response.HTTPResponse.rsp_data), iworld)
					(Error ("Request failed: "+++response.HTTPResponse.rsp_reason), iworld)

convertURL :: !String !(Maybe JSONNode) -> MaybeErrorString URI
convertURL url mbp
	= case parseURI url of
		Nothing 	= Error ("Malformed URL: "+++url)
		(Just uri) 	= if (maybe False ((<>) "sds") uri.uriScheme) 
							(Error ("Invalid URL: "+++url))
							(Ok (convert uri))
where
	convert u = {nullURI & uriScheme	= Just "http",
						   uriQuery		= fmap (\p -> "focus="+++escapeString okInQuery (toString p)) mbp,
						   uriRegName	= u.uriRegName, 
						   uriPort		= u.uriPort,
						   uriPath		= "/sds" +++ u.uriPath}
							
writeRemoteSDS :: !JSONNode !String !*IWorld -> *(!MaybeErrorString (), !*IWorld)
writeRemoteSDS val url iworld
	= case convertURL url Nothing of
		(Ok uri) 	= load uri val iworld
		(Error e) 	= (Error e, iworld)
where
	load uri val iworld
		# (response, iworld) = httpRequest HTTP_PUT uri (Just (toString val)) iworld
		= if (isOkResponse response)
					(Ok (), iworld)
					(Error ("Request failed: "+++response.HTTPResponse.rsp_reason), iworld)

remoteJSONShared :: !String -> JSONShared
remoteJSONShared url = SDSDynamic f
where
	f _ iworld=:{exposedShares}
        = case 'DM'.get url exposedShares of
		    Nothing          = (Ok (createReadWriteSDS "remoteShare" url rread rwrite), iworld)
			Just (_, shared) = (Ok shared, iworld)
			Just dyn         = (Error (exception ("Exposed share type mismatch: " +++ url)), iworld)

	rread _ iworld
        = case readRemoteSDS url iworld of
            (Ok v, iworld) = (Ok v, iworld)
            (Error msg, iworld) = (Error (exception msg), iworld)
	rwrite _ jsonw iworld
		= case writeRemoteSDS jsonw url iworld of
			(Ok (), iworld) = (Ok (const False), iworld)
			(Error msg, iworld) = (Error (exception msg), iworld)
		
