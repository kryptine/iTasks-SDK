implementation module iTasks.Framework.WebService

import StdList, StdBool, StdTuple, StdArray
import System.Time, Text, Text.JSON, Data.Map, Internet.HTTP, Data.Error
import iTasks.Framework.Task, iTasks.Framework.TaskState, iTasks.Framework.TaskEval, iTasks.Framework.TaskStore
import iTasks.Framework.UIDiff, iTasks.Framework.Util, iTasks.Framework.HtmlUtil, iTasks.Framework.Engine, iTasks.Framework.IWorld
import iTasks.API.Core.SystemTypes
import Crypto.Hash.SHA1, Text.Encodings.Base64

DEFAULT_THEME :== "gray"

//The representation of the JSON service
:: ServiceResponse :== [ServiceResponsePart]
:: ServiceResponsePart =
	{ taskId	:: !String
	, value		:: !JSONNode
	, actions	:: ![String]
	}
	
derive JSONEncode ServiceResponsePart

import StdDebug
//TODO: The upload and download mechanism used here is inherently insecure!!!
// A smarter scheme that checks up and downloads, based on the current session/task is needed to prevent
// unauthorized downloading of documents and DDOS uploading.
webService :: !String !(HTTPRequest -> Task a) !ServiceFormat ->
						 (!(String -> Bool)
                         ,!(HTTPRequest *IWorld -> (!HTTPResponse,!Maybe InstanceNo, !*IWorld))
						 ,!(HTTPRequest (Maybe {#Char}) InstanceNo *IWorld -> (!Maybe {#Char}, !Bool, !InstanceNo, !*IWorld))
						 ,!(HTTPRequest InstanceNo *IWorld -> *IWorld)
						 ) | iTask a
webService url task defaultFormat = (matchFun url,reqFun` url task defaultFormat,dataFun,disconnectFun)
where
    matchFun :: String String -> Bool
    matchFun matchUrl reqUrl = startsWith matchUrl reqUrl && isTaskUrl (reqUrl % (size matchUrl,size reqUrl))
    where
        isTaskUrl "" = True
        isTaskUrl s = case dropWhile ((==)"") (split "/" s) of //Ignore slashes at the beginning of the path
            [instanceNo,_]      = toInt instanceNo > 0 // {instanceNo}/{instanceKey}
            [instanceNo,_,_]    = toInt instanceNo > 0 // {instanceNo}/{instanceKey}/{view}
            _                   = False

	reqFun` :: !String !(HTTPRequest -> Task a) !ServiceFormat HTTPRequest !*IWorld -> (!HTTPResponse,!Maybe InstanceNo, !*IWorld) | iTask a
	reqFun` url task defaultFormat req iworld 
		# server_url = "//" +++ req.server_name +++ ":" +++ toString req.server_port
		= reqFun url task defaultFormat req {IWorld|iworld & serverURL = server_url}

	reqFun :: !String !(HTTPRequest -> Task a) !ServiceFormat HTTPRequest !*IWorld -> (!HTTPResponse,!Maybe InstanceNo, !*IWorld) | iTask a
	reqFun url task defaultFormat req iworld=:{IWorld|application,config}
		//Check for uploads
		| hasParam "upload" req
			# uploads = toList req.arg_uploads
			| length uploads == 0
				= (jsonResponse (JSONArray []),Nothing,iworld)
			# (documents, iworld) = createDocumentsFromUploads uploads iworld
			// response of uploads must use content-type "text/html" or else iframe upload of extjs does not work
			# rsp = jsonResponse (toJSON documents)
			= ({rsp & rsp_headers = put "Content-Type" "text/html" rsp.rsp_headers},Nothing,iworld)
		//Check for downloads
		| hasParam "download" req
			# (mbContent, iworld)	= loadDocumentContent downloadParam iworld
			# (mbMeta, iworld)		= loadDocumentMeta downloadParam iworld
			= case (mbContent,mbMeta) of
				(Just content,Just {Document|name,mime,size})
					# headers	= [("Status","200 OK"),("Content-Type", mime),("Content-Length", toString size),("Content-Disposition","attachment;filename=\"" +++ name +++ "\"")]
					= ({HTTPResponse|rsp_headers = fromList headers, rsp_data = content},Nothing,iworld)
				_
					= (notFoundResponse req,Nothing,iworld)
        //Check for WebSocket upgrade headers
        | (get "Upgrade" req.req_headers) =:(Just "websocket") && isJust (get "Sec-WebSocket-Key" req.req_headers)
            # secWebSocketKey       = fromJust (get "Sec-WebSocket-Key" req.req_headers)
            # secWebSocketAccept    = webSocketHandShake secWebSocketKey
            //Create handshake response
            # headers = [("Status","101 Switching Protocols"),("Upgrade","websocket"), ("Connection","Upgrade")
                        ,("Sec-WebSocket-Accept",secWebSocketAccept),("Sec-WebSocket-Protocol","itwc")]
            = ({HTTPResponse|rsp_headers = fromList headers, rsp_data = ""}, Just 0,iworld)
        | urlSpec == ""
            = case defaultFormat of
			    (WebApp	opts)
                    = case createUnevaluatedTaskInstance (task req) iworld of
                        (Error err, iworld)
				            = (errorResponse err, Nothing, iworld)
                        (Ok (instanceNo,instanceKey),iworld)
				            = (itwcStartResponse url instanceNo instanceKey  (theme opts) application, Nothing, iworld)
                JSONPlain
                    # (mbResult,iworld) = createSessionTaskInstance (task req) event iworld
				    = case mbResult of
					    Ok (ExceptionResult _ err,_,_,_,_)
						    = (errorResponse err, Nothing, iworld)
					    Ok (ValueResult (Value val _) _ _ _,_,_,_,_)
						    = (jsonResponse val, Nothing, iworld)
					    _
						    = (errorResponse "Requested service format not available for this task", Nothing, iworld)
			    _
				    = (jsonResponse (JSONString "Unknown service format"), Nothing, iworld)
        | otherwise
            = case dropWhile ((==)"") (split "/" urlSpec) of
                [instanceNo,instanceKey]
                    = (itwcStartResponse url instanceNo instanceKey (theme []) application, Nothing, iworld)
                [instanceNo,instanceKey,"gui"]
                    //Load task instance and edit / evaluate
                    # (mbResult, iworld)    = evalSessionTaskInstance (toInt instanceNo) event iworld
                    # (json, iworld) = case mbResult of
                        Error err
					        = (JSONObject [("success",JSONBool False),("error",JSONString err)],iworld)
                        Ok (ExceptionResult _ err,_,_,_)
                            = (JSONObject [("success",JSONBool False),("error",JSONString err)], iworld)
                        Ok (ValueResult (Value _ True) _ _ _,_,_,_)
                            = (JSONObject ([("success",JSONBool True),("done",JSONBool True)]), iworld)
                        Ok (ValueResult _ info curRep context,instanceNo,{SessionInfo|lastEvent},updates)
                            //Determine expiry date	
                            # (expiresIn,iworld) = getResponseExpiry instanceNo iworld
                            # json	= JSONObject [("success",JSONBool True)
                                                 ,("expiresIn",toJSON expiresIn)
                                                 ,("lastEvent",JSONInt lastEvent)
                                                 ,("updates", encodeUIUpdates updates)
                                                 ]
                            = (json,iworld)
                        _
					        = (JSONObject [("success",JSONBool False),("error",JSONString  "Unknown exception")],iworld)
                    = (jsonResponse json, Nothing, iworld)
                //Stream messages
                [instanceNo,instanceKey,"gui-stream"]
                    # (messages,iworld)	= getUIMessages (toInt instanceNo) iworld	
                    = (eventsResponse messages, Just (toInt instanceNo), iworld)
                //Just send the value encoded as JSON
                [instanceNo,instanceKey,"value"]
                    # (mbResult, iworld)    = evalSessionTaskInstance (toInt instanceNo) event iworld
                    = case mbResult of
                        Ok (ExceptionResult _ err,_,_,_)
                            = (errorResponse err, Nothing, iworld)
                        Ok (ValueResult (Value val _) _ _ _,_,_,_)
                            = (jsonResponse val, Nothing, iworld)
                        _
                            = (errorResponse "No value available", Nothing, iworld)
                _
				    = (errorResponse "Requested service format not available for this task", Nothing, iworld)
	    where
            urlSpec             = req.req_path % (size url,size req.req_path)

		    downloadParam		= paramValue "download" req
		    uploadParam			= paramValue "upload" req


		    eventNoParam		= paramValue "eventNo" req
		    eventNo				= toInt eventNoParam

		    editEventParam		= paramValue "editEvent" req
		    actionEventParam	= paramValue "actionEvent" req
		    focusEventParam		= paramValue "focusEvent" req

            theme [Theme name:_] = name
            theme _              = DEFAULT_THEME

		    event = case (fromJSON (fromString editEventParam)) of
			    Just (taskId,name,value)	= EditEvent eventNo (fromString taskId) name value
			    _	= case (fromJSON (fromString actionEventParam)) of
				    Just (taskId,actionId)	= ActionEvent eventNo (fromString taskId) actionId
				    _	= case (fromJSON (fromString focusEventParam)) of
					    Just taskId			= FocusEvent eventNo (fromString taskId)
					    _					= RefreshEvent (Just eventNo)

        webSocketHandShake key = base64Encode digest
        where
            (SHA1Digest digest) = sha1StringDigest (key+++"258EAFA5-E914-47DA-95CA-C5AB0DC85B11")

	dataFun :: HTTPRequest (Maybe {#Char}) !InstanceNo !*IWorld -> (!Maybe {#Char}, !Bool, !InstanceNo, !*IWorld)
    dataFun req mbData 0 iworld
        = (mbData,False,0,iworld) //TEMPORARY ECHO WEBSOCKET
	dataFun req _ instanceNo iworld
        # (messages,iworld)	= getUIMessages instanceNo iworld	
		= case messages of
			[]	= (Nothing,False,instanceNo,iworld)
			_	= (Just (formatMessageEvents messages),False,instanceNo,iworld)

    disconnectFun :: HTTPRequest InstanceNo !*IWorld -> *IWorld
	disconnectFun _ _ iworld = iworld

	jsonResponse json
		= {HTTPResponse | rsp_headers = fromList [("Content-Type","text/json"),("Access-Control-Allow-Origin","*")], rsp_data = toString json}
	errorResponse msg
		= {HTTPResponse | rsp_headers = fromList [("Status", "500 Internal Server Error")], rsp_data = msg}
			
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
		= {HTTPResponse | rsp_headers = fromList [("Content-Type","text/event-stream"),("Cache-Control","no-cache")]
                        , rsp_data = formatMessageEvents messages}
	
	formatMessageEvents messages = join "" (map format messages)
    where
        format (UIUpdates updates) = "data: " +++ toString (encodeUIUpdates updates) +++ "\n\n"
        format (UIReset text) = "event: reset\ndata: " +++ text +++ "\n\n"

	itwcStartResponse path instanceNo instanceKey theme appName = {newHTTPResponse & rsp_data = toString itwcStartPage}
	where
		itwcStartPage = HtmlTag [] [head,body]
		head = HeadTag [] [MetaTag [CharsetAttr "UTF-8"] []
                          ,MetaTag [NameAttr "viewport",ContentAttr "width=device-width"] []
                          ,MetaTag [NameAttr "mobile-web-app-capable",ContentAttr "yes"] []
                          ,TitleTag [] [Text appName], startUrlScript : styles ++ scripts]
		body = BodyTag [] []

        startUrlScript = ScriptTag [TypeAttr "text/javascript"]
            [RawText "window.itwc = {};"
            ,RawText ("itwc.START_INSTANCE_NO = "+++toString instanceNo+++";")
            ,RawText ("itwc.START_INSTANCE_KEY = \""+++instanceKey+++"\";")
            ,RawText ("itwc.START_INSTANCE_URL = \""+++startUrl path instanceNo instanceKey+++"\"; ")
            ]
	
        startUrl path instanceNo instanceKey //Check if the requested path ended with a slash
            | path.[size path - 1] == '/'
                = path +++ toString instanceNo +++ "/" +++ instanceKey
                = path +++ "/" +++ toString instanceNo +++ "/" +++ instanceKey

		styles = [LinkTag [RelAttr "stylesheet", HrefAttr file, TypeAttr "text/css"] [] \\ file <- stylefiles]
		scripts = [ScriptTag [SrcAttr file, TypeAttr "text/javascript"] [] \\ file <- scriptfiles]
		
		stylefiles =
            ["itwc-theme-"+++theme+++"/itwc-theme.css"
			,"css/icons.css"
			,"css/app.css"
			,appName +++ ".css"]

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
