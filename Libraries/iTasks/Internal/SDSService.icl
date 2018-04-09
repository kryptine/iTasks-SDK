implementation module iTasks.Internal.SDSService

import iTasks

from Internet.HTTP					import :: HTTPRequest {req_method, req_path, req_data}, :: HTTPResponse(..), :: HTTPMethod(..)
from iTasks.Internal.WebService   import :: ConnectionState, :: WebSockState, :: WebService(..)
from iTasks.Internal.TaskState 	import :: TIUIState

import iTasks.Internal.HtmlUtil, iTasks.Internal.DynamicUtil
import iTasks.Internal.RemoteAccess
from iTasks.Internal.SDS as SDS import qualified read, write
from iTasks.Extensions.Web import callHTTP

from StdFunc import o
import StdString, StdList
from Data.Map import qualified get, fromList
from Data.Map import fromList
import Data.Maybe, Data.Error
import Text.GenJSON, Text.URI
import StdMisc, graph_to_sapl_string
import Data.Queue, Data.Functor

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
	reqFun req _ iworld | hasParam "client_session_id" req
		= abort "Shareds on clients are not supported yet"		
				
	dataFun :: !HTTPRequest a !String !ConnectionState !*IWorld -> (![{#Char}], !Bool, !ConnectionState,!Maybe a, !*IWorld)
    dataFun req _ data instanceNo iworld = ([], True, instanceNo, Nothing, iworld)

    onShareChange _ _ s iworld = ([], True, s, Nothing, iworld)
    onTick _ _ instanceNo iworld = ([], True, instanceNo, Nothing, iworld)

    disconnectFun :: !HTTPRequest a !ConnectionState !*IWorld -> (!Maybe a, !*IWorld)
	disconnectFun _ _ _ iworld = (Nothing,iworld)

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
