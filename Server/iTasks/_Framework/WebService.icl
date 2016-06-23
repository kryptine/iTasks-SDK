implementation module iTasks._Framework.WebService

import StdList, StdBool, StdTuple, StdArray, StdFile
from StdFunc import o, const
import Data.Maybe, Data.Functor
from Data.Map import :: Map (..)
import qualified Data.List as DL
import qualified Data.Map as DM
import qualified Data.Queue as DQ
import qualified iTasks._Framework.SDS as SDS

import System.Time, Text, Text.JSON, Internet.HTTP, Data.Error
import System.File, System.FilePath, System.Directory
import iTasks._Framework.Task, iTasks._Framework.TaskState, iTasks._Framework.TaskEval, iTasks._Framework.TaskStore
import iTasks.UI.Definition, iTasks._Framework.Util, iTasks._Framework.HtmlUtil, iTasks._Framework.Engine, iTasks._Framework.IWorld
import iTasks.API.Core.SDSs, iTasks.API.Common.SDSCombinators
import iTasks.API.Core.Types
import Crypto.Hash.SHA1, Text.Encodings.Base64, Text.Encodings.MIME

from iTasks._Framework.HttpUtil import http_addRequestData, http_parseArguments

:: NetTaskState
    = NTIdle String Timestamp
    | NTReadingRequest HttpReqState
	| NTProcessingRequest HTTPRequest ConnectionType

:: HttpReqState =
    { request       :: HTTPRequest
    , method_done   :: Bool
    , headers_done  :: Bool
    , data_done     :: Bool
    , error         :: Bool
    }

// Websocket task access messages
:: WSCReq
    = ReqStartSession !InstanceNo
    | TaskEvent !Event

:: WSCRsp
    = AckStartSession !InstanceNo
    | TaskUpdates !InstanceNo ![UIChange]

//TODO: The upload and download mechanism used here is inherently insecure!!!
// A smarter scheme that checks up and downloads, based on the current session/task is needed to prevent
// unauthorized downloading of documents and DDOS uploading.
:: ChangeQueues :== Map InstanceNo (Queue UIChange)

taskWebService :: !String !(HTTPRequest -> Task a) ->
                 (!(String -> Bool)
                 ,!Bool
                 ,!(HTTPRequest ChangeQueues *IWorld -> (!HTTPResponse,!Maybe ConnectionType, !Maybe ChangeQueues, !*IWorld))
                 ,!(HTTPRequest ChangeQueues (Maybe {#Char}) ConnectionType *IWorld -> (![{#Char}], !Bool, !ConnectionType, !Maybe ChangeQueues, !*IWorld))
                 ,!(HTTPRequest ChangeQueues ConnectionType *IWorld -> (!Maybe ChangeQueues, !*IWorld))
                 ) | iTask a
taskWebService url task = (matchFun url,True,reqFun` url task,dataFun,disconnectFun)
where
    matchFun :: String String -> Bool
    matchFun matchUrl reqUrl = startsWith matchUrl reqUrl && isTaskUrl (reqUrl % (size matchUrl,size reqUrl))
    where
        isTaskUrl "" = True
        isTaskUrl s = case dropWhile ((==)"") (split "/" s) of  // Ignore slashes at the beginning of the path
            ["new"]             = True                          // Creation of new instances 
            ["gui-events"]      = True                          // Events for multiple instances
            ["gui-stream"]      = True                          // Updates stream for multiple instances
            [instanceNo,_]      = toInt instanceNo > 0          // {instanceNo}/{instanceKey}
            [instanceNo,_,_]    = toInt instanceNo > 0          // {instanceNo}/{instanceKey}/{view}
            _                   = False

	reqFun` url task req output iworld=:{server}
		# server_url = "//" +++ req.server_name +++ ":" +++ toString req.server_port
		= reqFun url task req output {IWorld|iworld & server = {server & serverURL = server_url}}

	reqFun url task req output iworld=:{IWorld|server={serverName,customCSS},config}
		//Check for uploads
		| hasParam "upload" req
			# uploads = 'DM'.toList req.arg_uploads
			| length uploads == 0
				= (jsonResponse (JSONArray []),Nothing,Nothing,iworld)
			# (documents, iworld) = createDocumentsFromUploads uploads iworld
			= (jsonResponse (toJSON documents),Nothing,Nothing,iworld)
		//Check for downloads
		| hasParam "download" req
			# (mbContent, iworld)	= loadDocumentContent downloadParam iworld
			# (mbMeta, iworld)		= loadDocumentMeta downloadParam iworld
			= case (mbContent,mbMeta) of
				(Just content,Just {Document|name,mime,size})
					# headers	= [("Content-Type", mime),("Content-Length", toString size),("Content-Disposition","attachment;filename=\"" +++ name +++ "\"")]
					= ({okResponse & rsp_headers = headers, rsp_data = content},Nothing,Nothing,iworld)
				_
					= (notFoundResponse req,Nothing,Nothing,iworld)
        //Check for WebSocket upgrade headers
        | ('DM'.get "Upgrade" req.HTTPRequest.req_headers) =:(Just "websocket") && isJust ('DM'.get "Sec-WebSocket-Key" req.HTTPRequest.req_headers)
            # secWebSocketKey       = fromJust ('DM'.get "Sec-WebSocket-Key" req.HTTPRequest.req_headers)
            # secWebSocketAccept    = webSocketHandShake secWebSocketKey
            //Create handshake response
            # headers = [("Upgrade","websocket"), ("Connection","Upgrade")
                        ,("Sec-WebSocket-Accept",secWebSocketAccept),("Sec-WebSocket-Protocol","itwc")]
            = ({newHTTPResponse 101 "Switching Protocols" & rsp_headers = headers, rsp_data = ""}, Just (WebSocketConnection []),Nothing,iworld)
        | urlSpec == ""
			= (itwcStartResponse url serverName customCSS, Nothing, Nothing, iworld)
        | otherwise
            = case dropWhile ((==)"") (split "/" urlSpec) of
				["new"]
					//Create new instances
                    = case createTaskInstance (task req) iworld of
						(Error (_,err), iworld)
				            = (errorResponse err, Nothing, Nothing, iworld)
                        (Ok (instanceNo,instanceKey),iworld)
							# json = JSONObject [("instanceNo",JSONInt instanceNo),("instanceKey",JSONString instanceKey)]
                    		= (jsonResponse json, Nothing, Nothing, iworld)
                ["gui-events"]
                    //Load task instance and edit / evaluate
                    # instanceNo         = toInt instanceNoParam
                    # (_,iworld)         = updateInstanceLastIO [instanceNo] iworld
					# iworld 			 = queueEvent instanceNo event iworld
					# json				 = JSONObject [("instance",JSONInt instanceNo)]
                    = (jsonResponse json, Nothing, Nothing, iworld)
                ["gui-stream"]
                    //Stream messages for multiple instances
                    # (_,iworld)        = updateInstanceConnect req.client_name instances iworld
					# (messages,output) = dequeueOutput instances output //TODO: Check keys
                    = (eventsResponse messages, Just (EventSourceConnection instances), Just output, iworld)
                [instanceNo,instanceKey]
                    = (itwcStartResponse url serverName customCSS, Nothing, Nothing, iworld)
                [instanceNo,instanceKey,"gui"]
                    //Load task instance and edit / evaluate
                    # instanceNo         = toInt instanceNo
                    # (_,iworld)         = updateInstanceLastIO [instanceNo] iworld
					# iworld 			 = queueEvent instanceNo event iworld
					# json				 = JSONObject [("instance",JSONInt instanceNo)]
                    = (jsonResponse json, Nothing, Nothing, iworld)
                //Stream messages for a specific instance
                [instanceNo,instanceKey,"gui-stream"]
                    # instances         = [toInt instanceNo]
                    # (_,iworld)        = updateInstanceConnect req.client_name instances iworld
					# (messages,output) = dequeueOutput instances output
                    = (eventsResponse messages, Just (EventSourceConnection instances), Nothing, iworld)
                _
				    = (errorResponse "Requested service format not available for this task", Nothing, Nothing, iworld)
	    where
            urlSpec             = req.HTTPRequest.req_path % (size url,size req.HTTPRequest.req_path)

		    downloadParam		= paramValue "download" req
		    uploadParam			= paramValue "upload" req

		    editEventParam		= paramValue "editEvent" req
		    actionEventParam	= paramValue "actionEvent" req
		    focusEventParam		= paramValue "focusEvent" req
            resetEventParam     = paramValue "resetEvent" req
			instanceNoParam 	= paramValue "instanceNo" req

            instances = filter (\i. i > 0) (map toInt (split "," (paramValue "instances" req)))

            event = case (fromJSON (fromString editEventParam)) of
				Just (taskId,JSONNull,JSONString actionId) = ActionEvent (fromString taskId) actionId
			    Just (taskId,JSONString name,value)	= EditEvent (fromString taskId) name value
				_	= case (fromJSON (fromString focusEventParam)) of
					Just taskId			= FocusEvent (fromString taskId)
					_   = if (resetEventParam == "") (RefreshEvent "Browser refresh") ResetEvent

        webSocketHandShake key = base64Encode digest
        where
            (SHA1Digest digest) = sha1StringDigest (key+++"258EAFA5-E914-47DA-95CA-C5AB0DC85B11")

    dataFun req output mbData (WebSocketConnection instances) iworld
        = (maybe [] (\d -> [d]) mbData,False,(WebSocketConnection instances),Nothing,iworld) //TEMPORARY ECHO WEBSOCKET
	dataFun req output _ (EventSourceConnection instances) iworld
		# (messages,output) = dequeueOutput instances output
		= case messages of //Ignore empty updates
			[] = ([],False,(EventSourceConnection instances),Nothing,iworld)
            messages	
                # (_,iworld) = updateInstanceLastIO instances iworld
                = ([formatMessageEvents messages],False,(EventSourceConnection instances),Just output, iworld)

	disconnectFun _ _ (EventSourceConnection instances) iworld    = (Nothing, snd (updateInstanceDisconnect instances iworld))
	disconnectFun _ _ _ iworld                                    = (Nothing, iworld)

	dequeueOutput :: ![InstanceNo] !(Map InstanceNo (Queue UIChange)) -> (![(!InstanceNo,!UIChange)],!Map InstanceNo (Queue UIChange))
	dequeueOutput [] states = ([],states)
	dequeueOutput [i:is] states
		# (output,states) = dequeueOutput is states
		= case 'DM'.get i states of
			Just out 	= ([(i,c) \\ c <- toList out] ++ output,'DM'.put i 'DQ'.newQueue states)
			Nothing  	= (output,states)
	where
		toList q = case 'DQ'.dequeue q of
			(Nothing,q) 	= []
			(Just x,q) 		= [x:toList q]

	jsonResponse json
		= {okResponse & rsp_headers = [("Content-Type","text/json"),("Access-Control-Allow-Origin","*")], rsp_data = toString json}
			
	eventsResponse messages
		= {okResponse &   rsp_headers = [("Content-Type","text/event-stream"),("Cache-Control","no-cache")]
                        , rsp_data = formatMessageEvents messages}
	
	formatMessageEvents messages = concat (map format messages)
    where
        format (instanceNo,change) = "data: {\"instance\":" +++toString instanceNo+++",\"change\":" +++ toString (encodeUIChange change) +++ "}\n\n"

	itwcStartResponse path appName customCSS = {okResponse & rsp_data = toString itwcStartPage}
	where
		itwcStartPage = HtmlTag [] [head,body]
		head = HeadTag [] [MetaTag [CharsetAttr "UTF-8"] []
                          ,MetaTag [NameAttr "viewport",ContentAttr "width=device-width, initial-scale=1.0, maximum-scale=1.0, minimum-scale=1.0, user-scalable=no, minimal-ui"] []
                          ,MetaTag [NameAttr "mobile-web-app-capable",ContentAttr "yes"] []
                          ,MetaTag [NameAttr "apple-mobile-web-app-capable",ContentAttr "yes"] []
                          ,TitleTag [] [Text appName], startUrlScript : styles ++ scripts]
		body = BodyTag [StyleAttr "width: 100%; height: 100%"] []

        startUrlScript = ScriptTag [TypeAttr "text/javascript"]
			[RawText "window.onload = function() { itasks.viewport({}, document.body); };"
            ]
		styles = [LinkTag [RelAttr "stylesheet", HrefAttr file, TypeAttr "text/css"] [] \\ file <- stylefiles]
		scripts = [ScriptTag [SrcAttr file, TypeAttr "text/javascript"] [] \\ file <- scriptfiles]
		
		stylefiles =
            ["itasks-theme-gray/itasks-theme.css"
			: if customCSS [appName +++ ".css"] []]

        scriptfiles = saplScripts ++ itaskScripts
        saplScripts =
            ["/utils.js","/itask.js"
			,"/builtin.js","/dynamic.js"
			,"/sapl-rt.js","/sapl-support.js"
			,"/db.js", "/debug.js","/itasks-js-interface.js"
			]
		itaskScripts = 
			["/itasks-core.js"
			,"/itasks-components-raw.js"
			,"/itasks-components-container.js"
			,"/itasks-components-form.js"
			,"/itasks-components-display.js"
			,"/itasks-components-selection.js"
			,"/itasks-components-action.js"
			,"/itasks-components-misc.js"
			]

	createDocumentsFromUploads [] iworld = ([],iworld)
	createDocumentsFromUploads [(n,u):us] iworld
		# (mbD,iworld)	= createDocument u.upl_filename u.upl_mimetype u.upl_content iworld
		| isError mbD	= createDocumentsFromUploads us iworld
		# (ds,iworld)	= createDocumentsFromUploads us iworld
		= ([fromOk mbD:ds],iworld)


httpServer :: !Int !Int ![(!String -> Bool
				,!Bool
				,!(HTTPRequest r *IWorld -> (!HTTPResponse,!Maybe ConnectionType, !Maybe w, !*IWorld))
				,!(HTTPRequest r (Maybe {#Char}) ConnectionType *IWorld -> (![{#Char}], !Bool, !ConnectionType, !Maybe w, !*IWorld))
				,!(HTTPRequest r ConnectionType *IWorld -> (!Maybe w, !*IWorld))
				)] (RWShared () r w) -> ConnectionTask | TC r & TC w
httpServer port keepAliveTime requestProcessHandlers sds
 = wrapIWorldConnectionTask {ConnectionHandlersIWorld|onConnect=onConnect, whileConnected=whileConnected, onDisconnect=onDisconnect} sds
where
    onConnect host r iworld=:{IWorld|world,clocks}
        = (Ok (NTIdle host clocks.timestamp),Nothing,[],False,{IWorld|iworld & world = world})

	whileConnected mbData connState=:(NTProcessingRequest request localState) r env
		//Select handler based on request path
		= case selectHandler request requestProcessHandlers of
			Just (_,_,_,handler,_)
				# (mbData,done,localState,mbW,env=:{IWorld|world,clocks}) = handler request r mbData localState env
				| done && isKeepAlive request	//Don't close the connection if we are done, but keepalive is enabled
					= (Ok (NTIdle request.client_name clocks.timestamp), mbW, mbData, False,{IWorld|env & world = world})
				| otherwise
					= (Ok (NTProcessingRequest request localState), mbW, mbData,done,{IWorld|env & world = world})
			Nothing
				= (Ok connState, Nothing, ["HTTP/1.1 400 Bad Request\r\n\r\n"], True, env)

	whileConnected (Just data) connState r iworld=:{IWorld|clocks}//(connState is either Idle or ReadingRequest)
		# rstate = case connState of
			(NTIdle client_name _)
				//Add new data to the request
				# request   = {newHTTPRequest & client_name = client_name, server_port = port}
				# (request, method_done, headers_done, data_done, error) = http_addRequestData request False False False data
				= {HttpReqState|request=request,method_done=method_done,headers_done=headers_done,data_done=data_done,error=error}
			(NTReadingRequest {HttpReqState|request, method_done, headers_done, data_done})
				//Add new data to the request
				# (request, method_done, headers_done, data_done, error) = http_addRequestData request method_done headers_done data_done (toString data)
				= {HttpReqState|request=request,method_done=method_done,headers_done=headers_done,data_done=data_done,error=error}
			_
				= {HttpReqState|request=newHTTPRequest,method_done=False,headers_done=False,data_done=False,error=True}
		| rstate.HttpReqState.error
			//Sent bad request response and disconnect
			= (Ok connState, Nothing, ["HTTP/1.1 400 Bad Request\r\n\r\n"], True, iworld)
		| not rstate.HttpReqState.headers_done
			//Without headers we can't select our handler functions yet
			= (Ok (NTReadingRequest rstate), Nothing, [], False, iworld)
		//Determine the handler
		= case selectHandler rstate.HttpReqState.request requestProcessHandlers of
			Nothing
				= (Ok connState, Nothing, ["HTTP/1.1 404 Not Found\r\n\r\n"], True, iworld)
			Just (_,completeRequest,newReqHandler,procReqHandler,_)
				//Process a completed request, or as soon as the headers are done if the handler indicates so
				| rstate.HttpReqState.data_done || (not completeRequest)
					# request	= if completeRequest (http_parseArguments rstate.HttpReqState.request) rstate.HttpReqState.request
					//Determine if a  persistent connection was requested
					# keepalive	= isKeepAlive request
					// Create a response
					# (response,mbLocalState,mbW,iworld)	= newReqHandler request r iworld 
					//Add keep alive header if necessary
					# response	= if keepalive {HTTPResponse|response & rsp_headers = [("Connection","Keep-Alive"):response.HTTPResponse.rsp_headers]} response
					// Encode the response to the HTTP protocol format
					= case mbLocalState of
						Nothing	
							# reply		= encodeResponse True response
							| keepalive
								= (Ok (NTIdle rstate.HttpReqState.request.client_name clocks.timestamp), mbW, [reply], False, iworld)
							| otherwise
								= (Ok connState, mbW, [reply], True, iworld)
						Just localState	
							= (Ok (NTProcessingRequest request localState), mbW, [(encodeResponse False response)], False, iworld)
				| otherwise
					= (Ok (NTReadingRequest rstate), Nothing, [], False, iworld)		

	//Close idle connections if the keepalive time has passed
	whileConnected Nothing connState=:(NTIdle ip (Timestamp t)) r iworld=:{IWorld|clocks={timestamp=Timestamp now}}
		= (Ok connState, Nothing, [], now >= t + keepAliveTime, iworld)

	//Do nothing if no data arrives for now
	whileConnected Nothing connState r iworld = (Ok connState,Nothing,[],False,iworld)

	//If we were processing a request and were interupted we need to
	//select the appropriate handler to wrap up

    onDisconnect connState=:(NTProcessingRequest request localState) r env
		= case selectHandler request requestProcessHandlers of
			Nothing	                        = (Ok connState, Nothing, env)
			Just (_,_,_,_,connLostHandler)  
				# (mbW, env) = connLostHandler request r localState env
				= (Ok connState, mbW, env)
    onDisconnect connState r env = (Ok connState, Nothing, env)

	selectHandler req [] = Nothing
	selectHandler req [h=:(pred,_,_,_,_):hs]
		| pred req.HTTPRequest.req_path	= Just h
										= selectHandler req hs

	isKeepAlive request = maybe (request.HTTPRequest.req_version == "HTTP/1.1") (\h -> (toLowerCase h == "keep-alive")) ('DM'.get "Connection" request.HTTPRequest.req_headers)

    encodeResponse autoContentLength response=:{HTTPResponse|rsp_headers, rsp_data}
	    # rsp_headers = addDefault rsp_headers "Server" "iTasks HTTP Server"
	    # rsp_headers = addDefault rsp_headers "Content-Type" "text/html"
	    # rsp_headers = if autoContentLength
	    					(addDefault rsp_headers "Content-Length" (toString (size rsp_data)))
	    					rsp_headers
	    = toString {HTTPResponse|response & rsp_headers = rsp_headers}
    where		
    	addDefault headers hdr val = if (('DL'.lookup hdr headers) =: Nothing) [(hdr,val):headers] headers

// Request handler which serves static resources from the application directory,
// or a system wide default directory if it is not found locally.
// This request handler is used for serving system wide javascript, css, images, etc...
staticResourceService ::(!(String -> Bool),!Bool,!(HTTPRequest r *IWorld -> (HTTPResponse, Maybe loc, Maybe w ,*IWorld))
                        ,!(HTTPRequest r (Maybe {#Char}) loc *IWorld -> (![{#Char}], !Bool, loc, Maybe w ,!*IWorld))
                        ,!(HTTPRequest r loc *IWorld -> (!Maybe w,!*IWorld)))
staticResourceService = (const True,True,initFun,dataFun,lostFun)
where
	initFun req _ env
		# (rsp,env) = handleStaticResourceRequest req env
		= (rsp,Nothing,Nothing,env)
		
	dataFun _ _ _ s env = ([],True,s,Nothing,env)
	lostFun _ _ s env = (Nothing,env)

	handleStaticResourceRequest :: !HTTPRequest *IWorld -> (!HTTPResponse,!*IWorld)
	handleStaticResourceRequest req iworld=:{IWorld|server={paths={publicWebDirectories}}}
    	= serveStaticResource req publicWebDirectories iworld
	where
    	serveStaticResource req [] iworld
	    	= (notFoundResponse req,iworld)
    	serveStaticResource req [d:ds] iworld=:{IWorld|world}
			# filename		   = d +++ filePath req.HTTPRequest.req_path
			# type			   = mimeType filename
			# (exists, world)  = fileExists filename world
			| not exists
               = serveStaticResource req ds {IWorld|iworld & world = world}
			# (mbContent, world)	= readFile filename world
			= case mbContent of
				(Ok content) = ({ okResponse
	    						& rsp_headers = [("Content-Type", type),("Content-Length", toString (size content))]
                                , rsp_data = content}, {IWorld|iworld & world = world})
                (Error e)    = (errorResponse (toString e +++ " ("+++ filename +++")"), {IWorld|iworld & world = world})

		//Translate a URL path to a filesystem path
		filePath path	= ((replaceSubString "/" {pathSeparator}) o (replaceSubString ".." "")) path
		mimeType path	= extensionToMimeType (takeExtension path)
