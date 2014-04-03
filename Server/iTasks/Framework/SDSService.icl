implementation module iTasks.Framework.SDSService

import iTasks

from Internet.HTTP					import :: HTTPRequest {req_method, req_path, req_data}, :: HTTPResponse(..), :: HTTPMethod(..)
from iTasks.Framework.IWorld		import :: IWorld {exposedShares}
from iTasks.Framework.Engine	    import :: ConnectionType

import iTasks.Framework.HtmlUtil, iTasks.Framework.DynamicUtil
import iTasks.Framework.RemoteAccess
from iTasks.Framework.SDS as SDS import qualified readp, writep, :: Shared, :: JSONShared
from iTasks.Framework.SDS import getURLbyId
from iTasks.API.Core.IntegrationTasks import callHTTP2

from StdFunc import o
import StdString, StdList
from Data.Map import qualified get, fromList
from Data.Map import fromList
import Data.Maybe, Data.Void, Data.Error
import Text.URI
import StdMisc, StdDebug, graph_to_sapl_string

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
			# (res, iworld) = 'SDS'.readp focus shared iworld
			= case res of
				(Ok json) = (jsonResponse json, Nothing, iworld)
				(Error e) = (errorResponse e, Nothing, iworld)			
			
		writeit shared iworld
			# (res, iworld) = 'SDS'.writep focus (fromString req.req_data) shared iworld
			= case res of
				(Ok _)    = (jsonResponse "OK", Nothing, iworld)
				(Error e) = (errorResponse e, Nothing, iworld)			
	
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
		// Check if SDS is local
		= case 'Data.Map'.get url exposedShares of
			Nothing 			# (response, iworld) = httpRequest HTTP_GET uri Nothing iworld
								= if (isOkResponse response)
										(Ok (fromString response.rsp_data), iworld)
										(Error ("Request failed: "+++response.rsp_reason), iworld)
			(Just (_, shared))  = readp p shared iworld

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
writeRemoteSDS p val url iworld=:{exposedShares}
	= case convertURL url (Just p) of
		(Ok uri) 	= load uri val iworld
		(Error e) 	= (Error e, iworld)
where
	load uri val iworld
		// Check if SDS is local
		= case 'Data.Map'.get url exposedShares of	
			Nothing				# (response, iworld) = httpRequest HTTP_PUT uri (Just (toString val)) iworld
								= if (isOkResponse response)
										(Ok Void, iworld)
										(Error ("Request failed: "+++response.rsp_reason), iworld)
			(Just (_, shared))  = writep p val shared iworld										

remoteJSONShared :: !String -> JSONShared
remoteJSONShared url = createReadWriteSDS "remoteShared" url read` write`
where
	read` jsonp iworld = readRemoteSDS jsonp url iworld
	write` jsonp jsonw iworld 
		= case writeRemoteSDS jsonp jsonw url iworld of
			(Ok Void, iworld) = (Ok (const False), iworld)
			(Error e, iworld) = (Error e, iworld)

openRemoteSDS :: !String !((Maybe (RWShared p r w)) -> Task a) -> Task a | iTask a & JSONEncode{|*|} p & JSONDecode{|*|} r & JSONEncode{|*|} w & TC p & TC r & TC w
openRemoteSDS url cont 
	= case convertURL url Nothing of
			(Error e) = throw e
			(Ok uri)  = callHTTP2 HTTP_GET uri "" conv >>= \ty -> trace_n (check ty) (cont (Just f))
where
	conv rsp = Ok rsp.rsp_data
	check srvty = clnty == srvty
	f = fromJSONShared (remoteJSONShared url)
	clnty = toString (unpackType (dynamic f))
	
