implementation module iTasks.Framework.WebService

import StdList, StdBool, StdTuple, StdArray
from StdFunc import o
import Data.Maybe, Data.Functor
from Data.Map import :: Map, :: Size
import qualified Data.List as DL
import qualified Data.Map as DM
import qualified iTasks.Framework.SDS as SDS

import System.Time, Text, Text.JSON, Internet.HTTP, Data.Error
import iTasks.Framework.Task, iTasks.Framework.TaskState, iTasks.Framework.TaskEval, iTasks.Framework.TaskStore
import iTasks.Framework.UIDiff, iTasks.Framework.Util, iTasks.Framework.HtmlUtil, iTasks.Framework.Engine, iTasks.Framework.IWorld
import iTasks.API.Core.SDSs, iTasks.API.Common.SDSCombinators
import iTasks.API.Core.Types
import Crypto.Hash.SHA1, Text.Encodings.Base64

from iTasks.Framework.HttpUtil import http_addRequestData, http_parseArguments

DEFAULT_THEME :== "gray"

//The representation of the JSON service
:: ServiceResponse :== [ServiceResponsePart]
:: ServiceResponsePart =
	{ taskId	:: !String
	, value		:: !JSONNode
	, actions	:: ![String]
	}

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

derive JSONEncode ServiceResponsePart

// Websocket task access messages
:: WSCReq
    = ReqStartSession !InstanceNo
    | TaskEvent !Event

:: WSCRsp
    = AckStartSession !InstanceNo
    | TaskUpdates !InstanceNo ![UIUpdate]

//TODO: The upload and download mechanism used here is inherently insecure!!!
// A smarter scheme that checks up and downloads, based on the current session/task is needed to prevent
// unauthorized downloading of documents and DDOS uploading.
webService :: !String !(HTTPRequest -> Task a) !ServiceFormat ->
						 (!(String -> Bool)
                         ,!(HTTPRequest (Map InstanceNo [UIUpdate]) *IWorld -> (!HTTPResponse,!Maybe ConnectionType, !Maybe (Map InstanceNo [UIUpdate]), !*IWorld))
						 ,!(HTTPRequest (Map InstanceNo [UIUpdate]) (Maybe {#Char}) ConnectionType *IWorld -> (![{#Char}], !Bool, !ConnectionType, !Maybe (Map InstanceNo [UIUpdate]), !*IWorld))
						 ,!(HTTPRequest (Map InstanceNo [UIUpdate]) ConnectionType *IWorld -> (!Maybe (Map InstanceNo [UIUpdate]), !*IWorld))
						 ) | iTask a
webService url task defaultFormat = (matchFun url,reqFun` url task defaultFormat,dataFun,disconnectFun)
where
    matchFun :: String String -> Bool
    matchFun matchUrl reqUrl = startsWith matchUrl reqUrl && isTaskUrl (reqUrl % (size matchUrl,size reqUrl))
    where
        isTaskUrl "" = True
        isTaskUrl s = case dropWhile ((==)"") (split "/" s) of  // Ignore slashes at the beginning of the path
            ["gui-stream"]      = True                          // Updates stream for multiple instances
            [instanceNo,_]      = toInt instanceNo > 0          // {instanceNo}/{instanceKey}
            [instanceNo,_,_]    = toInt instanceNo > 0          // {instanceNo}/{instanceKey}/{view}
            _                   = False

	reqFun` url task defaultFormat req output iworld=:{server}
		# server_url = "//" +++ req.server_name +++ ":" +++ toString req.server_port
		= reqFun url task defaultFormat req output {IWorld|iworld & server = {server & serverURL = server_url}}

	reqFun url task defaultFormat req output iworld=:{IWorld|server={serverName,customCSS},config}
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
        | ('DM'.get "Upgrade" req.req_headers) =:(Just "websocket") && isJust ('DM'.get "Sec-WebSocket-Key" req.req_headers)
            # secWebSocketKey       = fromJust ('DM'.get "Sec-WebSocket-Key" req.req_headers)
            # secWebSocketAccept    = webSocketHandShake secWebSocketKey
            //Create handshake response
            # headers = [("Upgrade","websocket"), ("Connection","Upgrade")
                        ,("Sec-WebSocket-Accept",secWebSocketAccept),("Sec-WebSocket-Protocol","itwc")]
            = ({newHTTPResponse 101 "Switching Protocols" & rsp_headers = headers, rsp_data = ""}, Just (WebSocketConnection []),Nothing,iworld)
        | urlSpec == ""
            = case defaultFormat of
			    (WebApp	opts)
                    = case createTaskInstance (task req) iworld of
                        (Error (_,err), iworld)
				            = (errorResponse err, Nothing, Nothing, iworld)
                        (Ok (instanceNo,instanceKey),iworld)
				            = (itwcStartResponse url instanceNo instanceKey (theme opts) serverName customCSS, Nothing, Nothing, iworld)
                JSONPlain
                    = case createTaskInstance (task req) iworld of
                        (Error (_,err), iworld)
				            = (errorResponse err, Nothing, Nothing, iworld)
                        (Ok (instanceNo,instanceKey),iworld)
                            = case evalTaskInstance instanceNo event iworld of
                                (Error err,iworld)            = (errorResponse err, Nothing, Nothing, iworld)
					            (Ok (_,Value val _,_),iworld) = (jsonResponse val, Nothing, Nothing, iworld)
					            (Ok (_,NoValue,_),iworld)     = (jsonResponse JSONNull, Nothing, Nothing, iworld)
			    _
				    = (jsonResponse (JSONString "Unknown service format"), Nothing, Nothing, iworld)
        | otherwise
            = case dropWhile ((==)"") (split "/" urlSpec) of
                ["gui-stream"]
                    //Stream messages for multiple instances
                    # iworld            = updateInstanceConnect req.client_name instances iworld
					# (messages,output) = popOutput instances output //TODO: Check keys
                    = (eventsResponse messages, Just (EventSourceConnection instances), Just output, iworld)
                [instanceNo,instanceKey]
                    = (itwcStartResponse url instanceNo instanceKey (theme []) serverName customCSS, Nothing, Nothing, iworld)
                [instanceNo,instanceKey,"gui"]
                    //Load task instance and edit / evaluate
                    # instanceNo         = toInt instanceNo
                    # iworld             = updateInstanceLastIO [instanceNo] iworld
                    # (mbResult, iworld) = evalTaskInstance instanceNo event iworld
                    # (json, iworld) = case mbResult of
                        Error err
					        = (JSONObject [("error",JSONString err)],iworld)
                        Ok (lastEventNo,value,updates)
                            //Determine expiry date	
                            # (expiresIn,iworld) = getResponseExpiry instanceNo iworld
                            # json	= JSONObject [("instance",JSONInt instanceNo)
                                                 ,("expiresIn",toJSON expiresIn)
                                                 ,("lastEvent",JSONInt lastEventNo)
                                                 ,("updates",encodeUIUpdates updates)
                                                 ]
                            = (json,iworld)
                        _
					        = (JSONObject [("success",JSONBool False),("error",JSONString  "Unknown exception")],iworld)
                    = (jsonResponse json, Nothing, Nothing, iworld)
                //Stream messages for a specific instance
                [instanceNo,instanceKey,"gui-stream"]
                    # instances         = [toInt instanceNo]
                    # iworld            = updateInstanceConnect req.client_name instances iworld
					# (messages,output) = popOutput instances output
                    = (eventsResponse messages, Just (EventSourceConnection instances), Nothing, iworld)
                _
				    = (errorResponse "Requested service format not available for this task", Nothing, Nothing, iworld)
	    where
            urlSpec             = req.req_path % (size url,size req.req_path)

		    downloadParam		= paramValue "download" req
		    uploadParam			= paramValue "upload" req


		    eventNoParam		= paramValue "eventNo" req
		    eventNo				= toInt eventNoParam

		    editEventParam		= paramValue "editEvent" req
		    actionEventParam	= paramValue "actionEvent" req
		    focusEventParam		= paramValue "focusEvent" req
            resetEventParam     = paramValue "resetEvent" req

            instances = filter (\i. i > 0) (map toInt (split "," (paramValue "instances" req)))

            theme [Theme name:_] = name
            theme _              = DEFAULT_THEME

		    event = case (fromJSON (fromString editEventParam)) of
			    Just (taskId,name,value)	= EditEvent eventNo (fromString taskId) name value
			    _	= case (fromJSON (fromString actionEventParam)) of
				    Just (taskId,actionId)	= ActionEvent eventNo (fromString taskId) actionId
				    _	= case (fromJSON (fromString focusEventParam)) of
					    Just taskId			= FocusEvent eventNo (fromString taskId)
					    _   = if (resetEventParam == "") (RefreshEvent (Just eventNo)) ResetEvent

        webSocketHandShake key = base64Encode digest
        where
            (SHA1Digest digest) = sha1StringDigest (key+++"258EAFA5-E914-47DA-95CA-C5AB0DC85B11")

    dataFun req output mbData (WebSocketConnection instances) iworld
        = (maybe [] (\d -> [d]) mbData,False,(WebSocketConnection instances),Nothing,iworld) //TEMPORARY ECHO WEBSOCKET
	dataFun req output _ (EventSourceConnection instances) iworld
		# (messages,output) = popOutput instances output
		= case filter (not o isEmpty o snd) messages of //Ignore empty updates
			[]	= ([],False,(EventSourceConnection instances),Nothing,iworld)
            messages	
                # iworld = updateInstanceLastIO instances iworld
                = ([formatMessageEvents messages],False,(EventSourceConnection instances),Just output, iworld)

	disconnectFun _ _ (EventSourceConnection instances) iworld    = (Nothing, updateInstanceDisconnect instances iworld)
	disconnectFun _ _ _ iworld                                    = (Nothing, iworld)

	popOutput :: ![InstanceNo] (Map InstanceNo [UIUpdate]) -> (![(!InstanceNo,![UIUpdate])],(Map InstanceNo [UIUpdate]))
	popOutput instances taskOutput
    	# taskOutput 	= 'DM'.toList taskOutput
    	# outUpdates    = [m \\ m=:(instanceNo,updates) <- taskOutput | isMember instanceNo instances]
    	# taskOutput    = [m \\ m=:(instanceNo,_) <- taskOutput | not (isMember instanceNo instances)]
		= (outUpdates, 'DM'.fromList taskOutput)

	jsonResponse json
		= {okResponse & rsp_headers = [("Content-Type","text/json"),("Access-Control-Allow-Origin","*")], rsp_data = toString json}
			
	serviceBusyResponse rep actions attributes
		= JSONObject [("status",JSONString "busy"),("parts",parts),("attributes",JSONObject [(k,JSONString v) \\ (k,v) <- attributes])]
	where
		parts = toJSON [{ServiceResponsePart|taskId = toString taskId, value = value, actions = findActions taskId actions} \\ (taskId,value) <- rep]
		findActions match actions
			= [actionName action \\ {taskId,action,enabled} <- actions | enabled && taskId == match]
	
	serviceDoneResponse val
		= JSONObject [("status",JSONString "complete"),("value",val)]
	serviceErrorResponse e
		= JSONObject [("status",JSONString "error"),("error",JSONString e)]

	eventsResponse messages
		= {okResponse &   rsp_headers = [("Content-Type","text/event-stream"),("Cache-Control","no-cache")]
                        , rsp_data = formatMessageEvents messages}
	
	formatMessageEvents messages = concat (map format messages)
    where
        format (instanceNo,updates) = "data: {\"instance\":" +++toString instanceNo+++",\"updates\":" +++ toString (encodeUIUpdates updates) +++ "}\n\n"

	itwcStartResponse path instanceNo instanceKey theme appName customCSS = {okResponse & rsp_data = toString itwcStartPage}
	where
		itwcStartPage = HtmlTag [] [head,body]
		head = HeadTag [] [MetaTag [CharsetAttr "UTF-8"] []
                          ,MetaTag [NameAttr "viewport",ContentAttr "width=device-width, initial-scale=1.0, maximum-scale=1.0, minimum-scale=1.0, user-scalable=no, minimal-ui"] []
                          ,MetaTag [NameAttr "mobile-web-app-capable",ContentAttr "yes"] []
                          ,MetaTag [NameAttr "apple-mobile-web-app-capable",ContentAttr "yes"] []
                          ,TitleTag [] [Text appName], startUrlScript : styles ++ scripts]
		body = BodyTag [] []

        startUrlScript = ScriptTag [TypeAttr "text/javascript"]
            [RawText "window.itwc = {};"
            ,RawText ("itwc.START_INSTANCE_NO = "+++toString instanceNo+++";")
            ,RawText ("itwc.START_INSTANCE_KEY = \""+++instanceKey+++"\";")
            ]
		styles = [LinkTag [RelAttr "stylesheet", HrefAttr file, TypeAttr "text/css"] [] \\ file <- stylefiles]
		scripts = [ScriptTag [SrcAttr file, TypeAttr "text/javascript"] [] \\ file <- scriptfiles]
		
		stylefiles =
            ["itwc-theme-"+++theme+++"/itwc-theme.css"
			,"css/icons.css"
			,"css/app.css"
			: if customCSS [appName +++ ".css"] []]

        scriptfiles =
            ["/app/taskeval/utils.js","/app/taskeval/itask.js" //TODO: Clean up SAPL mixed mess
			,"/app/taskeval/builtin.js"
			,"/app/taskeval/dynamic.js"
			,"/app/taskeval/sapl-rt.js", "/app/taskeval/sapl-support.js"
			,"/app/taskeval/db.js", "/app/taskeval/debug.js"
			,"/app/taskeval/interface.js"
            ,"/itwc.js"
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
    onConnect host r iworld=:{IWorld|world}
        # (ts,world) = time world
        = (Ok (NTIdle host ts),Nothing,[],False,{IWorld|iworld & world = world})

	whileConnected mbData connState=:(NTProcessingRequest request localState) r env
		//Select handler based on request path
		= case selectHandler request requestProcessHandlers of
			Just (_,_,_,handler,_)
				# (mbData,done,localState,mbW,env=:{IWorld|world}) = handler request r mbData localState env
				| done && isKeepAlive request	//Don't close the connection if we are done, but keepalive is enabled
					# (now,world)       = time world
					= (Ok (NTIdle request.client_name now), mbW, mbData, False,{IWorld|env & world = world})
				| otherwise
					= (Ok (NTProcessingRequest request localState), mbW, mbData,done,{IWorld|env & world = world})
			Nothing
				= (Ok connState, Nothing, ["HTTP/1.1 400 Bad Request\r\n\r\n"], True, env)

	whileConnected (Just data) connState r env  //(connState is either Idle or ReadingRequest)
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
			= (Ok connState, Nothing, ["HTTP/1.1 400 Bad Request\r\n\r\n"], True, env)
		| not rstate.HttpReqState.headers_done
			//Without headers we can't select our handler functions yet
			= (Ok (NTReadingRequest rstate), Nothing, [], False, env)
		//Determine the handler
		= case selectHandler rstate.HttpReqState.request requestProcessHandlers of
			Nothing
				= (Ok connState, Nothing, ["HTTP/1.1 404 Not Found\r\n\r\n"], True, env)
			Just (_,completeRequest,newReqHandler,procReqHandler,_)
				//Process a completed request, or as soon as the headers are done if the handler indicates so
				| rstate.HttpReqState.data_done || (not completeRequest)
					# request	= if completeRequest (http_parseArguments rstate.HttpReqState.request) rstate.HttpReqState.request
					//Determine if a  persistent connection was requested
					# keepalive	= isKeepAlive request
					// Create a response
					# (response,mbLocalState,mbW,env)	= newReqHandler request r env
					//Add keep alive header if necessary
					# response	= if keepalive {response & rsp_headers = [("Connection","Keep-Alive"):response.rsp_headers]} response
					// Encode the response to the HTTP protocol format
					= case mbLocalState of
						Nothing	
							# reply		= encodeResponse True response
							| keepalive
                                # env=:{IWorld|world} = env
								# (now,world)       = time world
								= (Ok (NTIdle rstate.HttpReqState.request.client_name now), mbW, [reply], False, {IWorld|env & world=world})
							| otherwise
								= (Ok connState, mbW, [reply], True, env)
						Just localState	
							= (Ok (NTProcessingRequest request localState), mbW, [(encodeResponse False response)], False, env)
				| otherwise
					= (Ok (NTReadingRequest rstate), Nothing, [], False, env)		

	//Close idle connections if the keepalive time has passed
	whileConnected Nothing connState=:(NTIdle ip (Timestamp t)) r iworld=:{IWorld|world}
		# (Timestamp now,world)	= time world//TODO: Do we really need to do this for every connection all the time?
		= (Ok connState, Nothing, [], now >= t + keepAliveTime, {IWorld|iworld & world = world})

	//Do nothing if no data arrives for now
	whileConnected Nothing connState r env = (Ok connState,Nothing,[],False,env)

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
		| pred req.req_path	= Just h
							= selectHandler req hs

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

