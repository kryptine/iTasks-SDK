implementation module iTasks.Framework.SDSService

from Internet.HTTP					import :: HTTPRequest {req_method, req_path, req_data}, :: HTTPResponse(..), :: HTTPMethod(..)
from iTasks.Framework.IWorld		import :: IWorld {exposedShares}, instance reportSDSChange Void, class reportSDSChange
from iTasks.Framework.Engine	    import :: ConnectionType

import iTasks.Framework.HtmlUtil, iTasks.Framework.DynamicUtil
import iTasks.Framework.RemoteAccess
from iTasks.Framework.SDS as SDS import qualified read, write, :: Shared
from iTasks.Framework.SDS import getURLbyId

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
	reqFun req iworld=:{exposedShares} | hasParam "type" req
		# (sdsurl, iworld) = getURLbyId ((hd o tl o tl) (pathToSegments req.req_path)) iworld
		= case 'Data.Map'.get sdsurl exposedShares of
			Nothing = (notFoundResponse req,Nothing,iworld) 
			(Just (dyn, _)) = (jsonResponse (toString (unpackType dyn)), Nothing, iworld)
	reqFun req iworld=:{exposedShares}
		# (sdsurl, iworld) = getURLbyId ((hd o tl o tl) (pathToSegments req.req_path)) iworld
		= case 'Data.Map'.get sdsurl exposedShares of
			Nothing = (notFoundResponse req,Nothing,iworld) 
			(Just (_, shared)) = case req.req_method of
									HTTP_GET = readit shared iworld
									HTTP_PUT = writeit shared iworld
											 = (badRequestResponse "Invalid method",Nothing,iworld)
	where
		readit shared iworld
			# (res, iworld) = 'SDS'.read shared iworld
			= case res of
				(Ok json) = (jsonResponse json, Nothing, iworld)
				(Error e) = (errorResponse e, Nothing, iworld)			
			
		writeit shared iworld
			# (res, iworld) = 'SDS'.write (fromString req.req_data) shared iworld
			= case res of
				(Ok _)    = (jsonResponse "OK", Nothing, iworld)
				(Error e) = (errorResponse e, Nothing, iworld)			
			
	jsonResponse json
		= {okResponse & rsp_headers = [("Content-Type","text/json")], rsp_data = toString json}			
				
	dataFun :: !HTTPRequest !(Maybe {#Char}) !ConnectionType !*IWorld -> (![{#Char}], !Bool, !ConnectionType, !*IWorld)
    dataFun req mbData instanceNo iworld = ([], True, instanceNo, iworld)

    disconnectFun :: !HTTPRequest !ConnectionType !*IWorld -> *IWorld
	disconnectFun _ _ iworld = iworld
	
readRemoteSDS  :: !String !*IWorld -> *(!MaybeErrorString JSONNode, !*IWorld)
readRemoteSDS url iworld 
	= case convertURL url of
		(Ok url) 	= load url iworld
		(Error e) 	= (Error e, iworld)
where
	load url iworld
		# (response, iworld) = httpRequest HTTP_GET url Nothing iworld
		= if (isOkResponse response)
				(Ok (fromString response.rsp_data), iworld)
				(Error ("Request failed: "+++response.rsp_reason), iworld)

convertURL url
	= case parseURI url of
		Nothing 	= Error ("Malformed URL: "+++url)
		(Just uri) 	= if (maybe False ((<>) "sds") uri.uriScheme) 
							(Error ("Invalid URL: "+++url))
							(Ok (toString (convert uri)))
where
	convert u = {nullURI & uriScheme	= Just "http", 
						   uriRegName	= u.uriRegName, 
						   uriPort		= u.uriPort,
						   uriPath		= "/sds" +++ u.uriPath}
							
writeRemoteSDS :: !JSONNode !String !*IWorld -> *(!MaybeErrorString Void, !*IWorld)
writeRemoteSDS val url iworld 
	= case convertURL url of
		(Ok url) 	= load url val iworld
		(Error e) 	= (Error e, iworld)
where
	load url val iworld
		# (response, iworld) = httpRequest HTTP_PUT url (Just (toString (toJSON val))) iworld
		= if (isOkResponse response)
				(Ok Void, iworld)
				(Error ("Request failed: "+++response.rsp_reason), iworld)


	
	
	
