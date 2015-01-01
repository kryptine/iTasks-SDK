implementation module iTasks.API.Extensions.Web
import iTasks
import Internet.HTTP, Text, Text.Encodings.MIME, Text.Encodings.UrlEncoding, StdArray, Data.Either

from iTasks.Framework.HttpUtil import http_addRequestData, http_parseArguments
import iTasks.Framework.HtmlUtil

import qualified Data.Map as DM
import qualified Data.List as DL

KEEPALIVE_TIME :== 5

:: HttpConnState
    = Idle String Timestamp
    | ReadingRequest HttpReqState
	| AwaitingResponse String Int Bool

:: HttpReqState =
    { request       :: HTTPRequest
    , method_done   :: Bool
    , headers_done  :: Bool
    , data_done     :: Bool
    , error         :: Bool
    }

derive class iTask HttpConnState, HttpReqState, HTTPRequest, HTTPResponse, HTTPMethod, HTTPProtocol, HTTPUpload, ConnectionType

serveWebService :: Int (HTTPRequest -> Task HTTPResponse) -> Task ()
serveWebService port handler 
	= withShared []
		\io ->
		manageConnections io -&&- handleRequests io
    @! ()
where
	manageConnections io
		= tcplisten port False (currentTimestamp |+< io) {ConnectionHandlers|onConnect=onConnect,whileConnected=whileConnected,onDisconnect}

    onConnect client_name (now,io)
		= (Ok (Idle client_name now), Nothing, [], False)

    whileConnected (Just data) l=:(Idle client_name last) (now,io)
		# request = {newHTTPRequest & client_name = client_name, server_port = port}
	 	# (request, method_done, headers_done, data_done, error) = http_addRequestData request False False False data
		# reqs = {HttpReqState|request=request,method_done=method_done,headers_done=headers_done,data_done=data_done,error=error}
		= whileReadingRequest data reqs now io
	whileConnected (Just data) l=:(ReadingRequest {HttpReqState|request, method_done, headers_done, data_done}) (now,io)
		# (request, method_done, headers_done, data_done, error) = http_addRequestData request method_done headers_done data_done (toString data)
		# reqs = {HttpReqState|request=request,method_done=method_done,headers_done=headers_done,data_done=data_done,error=error}
		= whileReadingRequest data reqs now io

	whileConnected Nothing l=:(AwaitingResponse client_name reqId keepalive) (now,io)
		= case getResponse reqId io of
			(Nothing,_) = (Ok l, Nothing, [], False)
			(Just response,io)
				//Add keep alive header if necessary
				# response	= if keepalive {response & rsp_headers = [("Connection","Keep-Alive"):response.rsp_headers]} response
				# reply		= encodeResponse True response
				= (Ok (Idle client_name now), Just io, [reply], keepalive)

	whileConnected Nothing l=:(Idle client_name (Timestamp last)) (Timestamp now,_) //Close idle connections if the keepalive time passed
		= (Ok l, Nothing, [], now - last > KEEPALIVE_TIME)

    whileConnected _ l (now,io)
		= (Ok l, Nothing, [], False)

	whileReadingRequest data reqs now io
		| reqs.HttpReqState.error
			//Sent bad request response and disconnect
			= (Ok (Idle reqs.HttpReqState.request.client_name now) , Nothing, ["HTTP/1.1 400 Bad Request\r\n\r\n"], True)
		| not reqs.HttpReqState.headers_done
			//Without headers we can't do anything yet
			= (Ok (ReadingRequest reqs), Nothing, [], False)
		| not reqs.HttpReqState.data_done	
			//For now only support full requests
			= (Ok (ReadingRequest reqs), Nothing, [], False)
		//Queue request to get a response
		# request	= http_parseArguments reqs.HttpReqState.request 
		//Determine if a  persistent connection was requested
		# keepalive	= isKeepAlive request
		//Add the request to be handled and wait
		# (reqId,io) = addRequest request io
		= (Ok (AwaitingResponse request.client_name reqId keepalive), Just io, [], False)

    onDisconnect l _        = (Ok l, Nothing)

	isKeepAlive request = maybe (request.req_version == "HTTP/1.1") (\h -> (toLowerCase h == "keep-alive")) ('DM'.get "Connection" request.req_headers)

    encodeResponse autoContentLength response=:{rsp_headers, rsp_data}
	    # rsp_headers = addDefault rsp_headers "Server" "iTasks HTTP Server"
	    # rsp_headers = addDefault rsp_headers "Content-Type" "text/html"
	    # rsp_headers = if autoContentLength
	    					(addDefault rsp_headers "Content-Length" (toString (size rsp_data)))
	    					rsp_headers
	    = toString {response & rsp_headers = rsp_headers}
    where		
    	addDefault headers hdr val = if (('DL'.lookup hdr headers) =: Nothing) [(hdr,val):headers] headers

	handleRequests io
		= 	forever (
				(watch io @ listRequests) 								 //Watch for unhandled requests
			>>* [OnValue (ifValue (not o isEmpty) (createResponses io))] //Handle the new requests and store responses
			)
	
	createResponses slist requests 
		= 	allTasks [handler req \\ (_,req) <- requests]
		>>- \responses ->
			upd (addResponses [(reqId,rsp) \\ (reqId,_) <- requests & rsp <- responses]) slist
		@! ()
	where
		addResponses [] list = list
		addResponses [(reqId,rsp):rest] list = addResponses rest (addResponse reqId rsp list)
		
//The data structure shared between the management of connections and the actual processing of requests
:: ConnectionList :== [(Int,Either HTTPRequest HTTPResponse)]
:: RequestId 	:== Int

addRequest :: HTTPRequest ConnectionList -> (RequestId,ConnectionList)
addRequest req list = addRequest` 0 req list
where
	addRequest` max req [] = let max` = max + 1 in (max`,[(max`,Left req)])
	addRequest` max req [x=:(i,_):xs] = let (id,xs`) = addRequest` (if (i > max) i max) req xs in (id,[x:xs`])

listRequests :: ConnectionList -> [(RequestId,HTTPRequest)]
listRequests list = [(i,req) \\ (i,Left req) <- list]

addResponse :: RequestId HTTPResponse ConnectionList -> ConnectionList
addResponse reqId rsp [] = []
addResponse reqId rsp [x=:(i,_):xs]
	| i == reqId 	= [(i,Right rsp):xs]
	| otherwise		= [x:addResponse reqId rsp xs]

getResponse :: RequestId ConnectionList -> (Maybe HTTPResponse,ConnectionList)
getResponse reqId [] = (Nothing,[])
getResponse reqId [x=:(i,Right rsp):xs]
	| i == reqId 	     = (Just rsp,xs)
	| otherwise  		 = let (mbrsp,xs`) = getResponse reqId xs in (mbrsp,[x:xs])
getResponse reqId [x:xs] = let (mbrsp,xs`) = getResponse reqId xs in (mbrsp,[x:xs])

serveFile :: [FilePath] HTTPRequest -> Task HTTPResponse
serveFile [] req = return (notFoundResponse req)
serveFile [d:ds] req=:{HTTPRequest|req_path}
	= 	try (importTextFile (d +++ filePath) @ toResponse)
		    (\(FileException _ _) -> serveFile ds req)
where
	//Translate a URL path to a filesystem path
	filePath = ((replaceSubString "/" {pathSeparator}) o (replaceSubString ".." "")) (urlDecode req_path)
	mimeType = extensionToMimeType (takeExtension filePath)

	toResponse content
	  = {HTTPResponse|okResponse
		& rsp_headers =
			[("Content-Type", mimeType)
			,("Content-Length", toString (size content))]
		, rsp_data = content
		}
