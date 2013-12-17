implementation module iTasks.Framework.SDSService

from Internet.HTTP					import :: HTTPRequest {req_method, req_path, req_data}, :: HTTPResponse(..), :: HTTPMethod(..)
from iTasks.Framework.IWorld		import :: IWorld {exposedShares}
from iTasks.API.Core.SystemTypes	import :: InstanceNo

import iTasks.Framework.HtmlUtil, iTasks.Framework.Shared, iTasks.Framework.RemoteAccess
from Data.SharedDataSource import qualified read, write

from StdFunc import o
import StdString, StdList
from Data.Map import qualified get, fromList
from Data.Map import fromList
import Data.Maybe, Data.Void
import Text.URI
import StdMisc, StdDebug, graph_to_sapl_string

sdsService ::   (!(String -> Bool)
				 ,!Bool
                 ,!(HTTPRequest *IWorld -> *(!HTTPResponse, !Maybe ConnectionType, !*IWorld))
				 ,!(HTTPRequest (Maybe {#Char}) ConnectionType *IWorld -> (!Maybe {#Char}, !Bool, !ConnectionType, !*IWorld))
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
			# (res, iworld) = 'Data.SharedDataSource'.read shared iworld
			= case res of
				(Ok json) = (jsonResponse json, Nothing, iworld)
				(Error e) = (errorResponse e, Nothing, iworld)			
			
		writeit shared iworld 
			# (res, iworld) = 'Data.SharedDataSource'.write (fromString req.req_data) shared iworld
			= case res of
				(Ok _)    = (jsonResponse "OK", Nothing, iworld)
				(Error e) = (errorResponse e, Nothing, iworld)			
			
	jsonResponse json
		= {okResponse & rsp_headers = fromList [("Content-Type","text/json")], rsp_data = toString json}			
				
	dataFun :: !HTTPRequest !(Maybe {#Char}) !ConnectionType !*IWorld -> (!Maybe {#Char}, !Bool, !ConnectionType, !*IWorld)
    dataFun req mbData instanceNo iworld = (mbData, True, instanceNo, iworld)

    disconnectFun :: !HTTPRequest !ConnectionType !*IWorld -> *IWorld
	disconnectFun _ _ iworld = iworld	
	
readRemoteSDS  :: !String !*IWorld -> *(!MaybeErrorString JSONNode, !*IWorld)
readRemoteSDS url iworld 
	= case parseURI url of
		Nothing 	= (Error ("Malformed URL: "+++url), iworld)
		(Just uri) 	= if (maybe False ((<>) "sds") uri.uriScheme) 
							(Error ("Invalid URL: "+++url), iworld)
							(Ok JSONNull, trace_n (toString (convert uri)) iworld)
where
//	load url iworld
//		# (response, iworld) = httpRequest HTTP_GET url Nothing iworld

	convert u = {nullURI & uriScheme	= Just "http",
						   uriRegName	= u.uriRegName,
						   uriPort		= u.uriPort,
						   uriPath		= "/sds" +++ u.uriPath}
							
writeRemoteSDS :: !JSONNode !String !*IWorld -> *(!MaybeErrorString Void, !*IWorld)
writeRemoteSDS val url iworld = undef
