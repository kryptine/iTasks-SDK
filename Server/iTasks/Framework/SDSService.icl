implementation module iTasks.Framework.SDSService

from Internet.HTTP					import :: HTTPRequest {req_method, req_path, req_data}, :: HTTPResponse(..), :: HTTPMethod(..)
from iTasks.Framework.IWorld		import :: IWorld {exposedShares}
from iTasks.API.Core.SystemTypes	import :: InstanceNo

import iTasks.Framework.HtmlUtil, iTasks.Framework.Shared
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
                 ,!(HTTPRequest *IWorld -> *(!HTTPResponse, !Maybe InstanceNo, !*IWorld))
				 ,!(HTTPRequest (Maybe {#Char}) InstanceNo *IWorld -> (!Maybe {#Char}, !Bool, !InstanceNo, !*IWorld))
				 ,!(HTTPRequest InstanceNo *IWorld -> *IWorld)
				 )

sdsService = (matchFun,True,reqFun,dataFun,disconnectFun)
where
    matchFun :: String -> Bool
    matchFun reqUrl = case pathToSegments reqUrl of
    					["","sds",_] = True
    							  	 = False

	reqFun :: !HTTPRequest !*IWorld -> *(!HTTPResponse, !Maybe InstanceNo, !*IWorld)
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
		= {HTTPResponse | rsp_headers = fromList [("Content-Type","text/json")], rsp_data = toString json}			
			
	errorResponse msg
		= {HTTPResponse | rsp_headers = fromList [("Status","500 Internal Server Error")], rsp_data = msg}	
	
	badRequestResponse msg
		= {HTTPResponse | rsp_headers = fromList [("Status","400 Bad Request")], rsp_data = msg}	
	
	dataFun :: !HTTPRequest !(Maybe {#Char}) !InstanceNo !*IWorld -> (!Maybe {#Char}, !Bool, !InstanceNo, !*IWorld)
    dataFun req mbData instanceNo iworld = (mbData, True, instanceNo, iworld)

    disconnectFun :: !HTTPRequest !InstanceNo !*IWorld -> *IWorld
	disconnectFun _ _ iworld = iworld	