implementation module iTasks.Framework.SDSService

import iTasks

from Internet.HTTP					import :: HTTPRequest {req_method, req_path, req_data}, :: HTTPResponse(..), :: HTTPMethod(..)
from iTasks.Framework.IWorld		import :: IWorld {exposedShares}
from iTasks.Framework.Engine	    import :: ConnectionType

import iTasks.Framework.HtmlUtil, iTasks.Framework.DynamicUtil
import iTasks.Framework.RemoteAccess
from iTasks.Framework.SDS as SDS import qualified read, write, :: Shared, :: JSONShared
from iTasks.Framework.SDS import getURLbyId
from iTasks.API.Core.IntegrationTasks import callHTTP2

from StdFunc import o
import StdString, StdList
from Data.Map import qualified get, fromList
from Data.Map import fromList
import Data.Maybe, Data.Void, Data.Error
import Text.URI
import StdMisc, graph_to_sapl_string

sdsService ::   (!(String -> Bool)
				 ,!Bool
                 ,!(HTTPRequest *IWorld -> *(!HTTPResponse, !Maybe ConnectionType, !*IWorld))
				 ,!(HTTPRequest (Maybe {#Char}) ConnectionType *IWorld -> (![{#Char}], !Bool, !ConnectionType, !*IWorld))
				 ,!(HTTPRequest ConnectionType *IWorld -> *IWorld)
				 )

sdsService = (matchFun,True,reqFun,dataFun,disconnectFun)
where
    matchFun :: String -> Bool
    matchFun reqUrl = case pathToSegments reqUrl of
    					["","sds",_] = True
    							  	 = False

	reqFun :: !HTTPRequest !*IWorld -> *(!HTTPResponse, !Maybe ConnectionType, !*IWorld)
	reqFun req iworld | hasParam "client_session_id" req
		= abort "Shareds on clients are not supported yet"
	reqFun req iworld=:{exposedShares} | hasParam "focus" req
		# (sdsurl, iworld) = getURLbyId ((hd o tl o tl) (pathToSegments req.req_path)) iworld
		= case 'Data.Map'.get sdsurl exposedShares of
				Nothing = (notFoundResponse req,Nothing,iworld) 
				(Just (_, shared)) = case req.req_method of
									HTTP_GET = readit shared iworld
									HTTP_PUT = writeit shared iworld
											 = (badRequestResponse "Invalid method",Nothing,iworld)
	where
		focus = fromString (paramValue "focus" req)
	
		readit shared iworld
			# (res, iworld) = 'SDS'.read (sdsFocus focus shared) iworld
			= case res of
				(Ok json)       = (jsonResponse json, Nothing, iworld)
				(Error (e,msg)) = (errorResponse msg, Nothing, iworld)			
			
		writeit shared iworld
			# (res, iworld) = 'SDS'.write (fromString req.req_data) (sdsFocus focus shared) iworld
			= case res of
				(Ok _)          = (okResponse, Nothing, iworld)
				(Error (e,msg)) = (errorResponse msg, Nothing, iworld)			
	
	reqFun req iworld=:{exposedShares}
		# (sdsurl, iworld) = getURLbyId ((hd o tl o tl) (pathToSegments req.req_path)) iworld
		= case 'Data.Map'.get sdsurl exposedShares of
			Nothing = (notFoundResponse req,Nothing,iworld) 
			(Just (dyn, _)) = (plainResponse (toString (unpackType dyn)), Nothing, iworld)
	
	jsonResponse json
		= {okResponse & rsp_headers = [("Content-Type","text/json")], rsp_data = toString json}			

	plainResponse string
		= {okResponse & rsp_headers = [("Content-Type","text/plain")], rsp_data = string}			
				
	dataFun :: !HTTPRequest !(Maybe {#Char}) !ConnectionType !*IWorld -> (![{#Char}], !Bool, !ConnectionType, !*IWorld)
    dataFun req mbData instanceNo iworld = ([], True, instanceNo, iworld)

    disconnectFun :: !HTTPRequest !ConnectionType !*IWorld -> *IWorld
	disconnectFun _ _ iworld = iworld

readRemoteSDS  :: !JSONNode !String !*IWorld -> *(!MaybeErrorString JSONNode, !*IWorld)
readRemoteSDS p url iworld=:{exposedShares}
	= case convertURL url (Just p) of
		(Ok uri) 	= load uri iworld
		(Error e) 	= (Error e, iworld)
where
	load uri iworld
		# (response, iworld) = httpRequest HTTP_GET uri Nothing iworld
		= if (isOkResponse response)
					(Ok (fromString response.rsp_data), iworld)
					(Error ("Request failed: "+++response.rsp_reason), iworld)

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
							
writeRemoteSDS :: !JSONNode !JSONNode !String !*IWorld -> *(!MaybeErrorString Void, !*IWorld)
writeRemoteSDS p val url iworld
	= case convertURL url (Just p) of
		(Ok uri) 	= load uri val iworld
		(Error e) 	= (Error e, iworld)
where
	load uri val iworld
		# (response, iworld) = httpRequest HTTP_PUT uri (Just (toString val)) iworld
		= if (isOkResponse response)
					(Ok Void, iworld)
					(Error ("Request failed: "+++response.rsp_reason), iworld)

remoteJSONShared :: !String -> JSONShared
remoteJSONShared url = SDSDynamic f
where
	f _ iworld=:{exposedShares}
        = case 'Data.Map'.get url exposedShares of
		    Nothing          = (Ok (createReadWriteSDS "remoteShare" url rread rwrite), iworld)
			Just (_, shared) = (Ok shared, iworld)
			Just dyn         = (Error (exception ("Exposed share type mismatch: " +++ url)), iworld)

	rread jsonp iworld
        = case readRemoteSDS jsonp url iworld of
            (Ok v, iworld) = (Ok v, iworld)
            (Error msg, iworld) = (Error (exception msg), iworld)
	rwrite jsonp jsonw iworld
		= case writeRemoteSDS jsonp jsonw url iworld of
			(Ok Void, iworld) = (Ok (const False), iworld)
			(Error msg, iworld) = (Error (exception msg), iworld)

openRemoteSDS :: !String !((Maybe (RWShared p r w)) -> Task a) -> Task a | iTask a & JSONEncode{|*|} p & JSONDecode{|*|} r & JSONEncode{|*|} w & TC p & TC r & TC w
openRemoteSDS url cont 
	= case convertURL url Nothing of
			(Error e) = throw e
			(Ok uri)  = callHTTP2 HTTP_GET uri "" conv >>= \ty -> if (check ty) (cont (Just f)) (throw "Type check failed")
where
	conv rsp = Ok rsp.rsp_data
	check srvty = clnty == srvty
	f = fromJSONShared (remoteJSONShared url)
	clnty = toString (unpackType (dynamic f))
		
