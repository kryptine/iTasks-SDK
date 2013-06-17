implementation module iTasks.Framework.WebService

import StdList, StdBool, StdTuple
import System.Time, Text.JSON, Data.Map, Internet.HTTP, Data.Error
import iTasks.Framework.Task, iTasks.Framework.TaskState, iTasks.Framework.TaskEval, iTasks.Framework.TaskStore
import iTasks.Framework.UIDiff, iTasks.Framework.Util, iTasks.Framework.HtmlUtil, iTasks.Framework.Engine, iTasks.Framework.IWorld
import iTasks.API.Core.SystemTypes

//Flag for disabling use of the compiled version of the client javascript
//only useful when doing work on the client framework
IF_CLIENT_DEV yes no	:== yes

//The representation of the JSON service
:: ServiceResponse :== [ServiceResponsePart]
:: ServiceResponsePart =
	{ taskId	:: !String
	, value		:: !JSONNode
	, actions	:: ![String]
	}
	
derive JSONEncode ServiceResponsePart

//TODO: The upload and download mechanism used here is inherently insecure!!!
// A smarter scheme that checks up and downloads, based on the current session/task is needed to prevent
// unauthorized downloading of documents and DDOS uploading.

webService :: !(HTTPRequest -> Task a) !ServiceFormat ->
						 (!(HTTPRequest *IWorld -> (!HTTPResponse,!Maybe SessionId, !*IWorld))
						 ,!(HTTPRequest (Maybe {#Char}) SessionId *IWorld -> (!Maybe {#Char}, !Bool, !SessionId, !*IWorld))
						 ,!(HTTPRequest SessionId *IWorld -> *IWorld)
						 ) | iTask a
webService task defaultFormat = (reqFun task defaultFormat,dataFun,disconnectFun)
where
	reqFun ::!(HTTPRequest -> Task a) !ServiceFormat HTTPRequest !*IWorld -> (!HTTPResponse,!Maybe SessionId, !*IWorld) | iTask a
	reqFun task defaultFormat req iworld=:{IWorld|application}
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
		= case format req of
			//Serve start page
			WebApp	
				= (appStartResponse application, Nothing, iworld)
			//Serve the user interface representation once, or if possible the diff between the current task GUI and a previous version
			JSONGui
				//Load or create session context and edit / evaluate
				# (mbResult, iworld)	= case sessionParam of
					""			= createSessionTaskInstance (task req) RefreshEvent iworld
					sessionId	= evalSessionTaskInstance sessionId event iworld
				# (json, iworld) 		= case mbResult of
					Error err
						= (JSONObject [("success",JSONBool False),("error",JSONString err)],iworld)
					Ok (ExceptionResult _ err,_,_,_)
						= (JSONObject [("success",JSONBool False),("error",JSONString err)], iworld)
					Ok (ValueResult (Value _ True) _ _ _,_,_,_)
						= (JSONObject ([("success",JSONBool True),("done",JSONBool True)]), iworld)
					Ok (ValueResult _ info curRep context,instanceNo,sessionId,updates)
						//Determine expiry date	
						# (expiresIn,iworld)	= getResponseExpiry instanceNo iworld
						# json	= JSONObject [("success",JSONBool True)
											 ,("session",JSONString sessionId)
											 ,("expiresIn",toJSON expiresIn)
											 ,("updates", encodeUIUpdates updates)
											 ,("version",toJSON guiVersion)]
						= (json,iworld)
					_
						= (JSONObject [("success",JSONBool False),("error",JSONString  "Unknown exception")],iworld)
				= (jsonResponse json, Nothing, iworld)
			//Serve the task representation as a continuous stream of GUI update events.
			JSONGuiEventStream
				# (mbResult, iworld)	= case sessionParam of
					""			= createSessionTaskInstance (task req) RefreshEvent iworld
					sessionId	= evalSessionTaskInstance sessionId event iworld
				= case mbResult of
					Ok (ValueResult _ _ _ _,instanceNo,sessionId,updates)
						= (eventsResponse sessionId updates, Just sessionId, iworld)	
					_
						= (errorResponse "Failed to initialize event stream", Nothing, iworld)
			//Serve the task in easily accessable JSON representation
			JSONService
				# (mbResult,iworld)	= case sessionParam of
					""			= createSessionTaskInstance (task req) RefreshEvent iworld
					sessionId	= evalSessionTaskInstance sessionId RefreshEvent iworld
				= case mbResult of
					Ok (ExceptionResult _ err,_,_,_)
						= (errorResponse err, Nothing, iworld)
					Ok (ValueResult (Value val True) _ _ _,_,_,_)
						= (jsonResponse (serviceDoneResponse val), Nothing, iworld)
					Ok (ValueResult _ _ (TaskRep def rep) _,_,_,_)
						= (jsonResponse (serviceBusyResponse rep (uiDefActions def) (toList (uiDefAttributes def))), Nothing, iworld)
			//Serve the task in a minimal JSON representation (only possible for non-parallel instantly completing tasks)
			JSONPlain
				# (mbResult,iworld) = createSessionTaskInstance (task req) RefreshEvent iworld
				= case mbResult of
					Ok (ExceptionResult _ err,_,_,_)
						= (errorResponse err, Nothing, iworld)
					Ok (ValueResult (Value val _) _ _ _,_,_,_)
						= (jsonResponse val, Nothing, iworld)
					_
						= (errorResponse "Requested service format not available for this task", Nothing, iworld)
			//Error unimplemented type
			_
				= (jsonResponse (JSONString "Unknown service format"), Nothing, iworld)
	where
		sessionParam		= paramValue "session" req
		downloadParam		= paramValue "download" req
		uploadParam			= paramValue "upload" req

		versionParam		= paramValue "version" req

		editEventParam		= paramValue "editEvent" req
		actionEventParam	= paramValue "actionEvent" req
		focusEventParam		= paramValue "focusEvent" req
	
		event = case (fromJSON (fromString editEventParam)) of
			Just (taskId,name,value)	= EditEvent (fromString taskId) name value
			_	= case (fromJSON (fromString actionEventParam)) of
				Just (taskId,actionId)	= ActionEvent (fromString taskId) actionId
				_	= case (fromJSON (fromString focusEventParam)) of
					Just taskId			= FocusEvent (fromString taskId)
					_					= RefreshEvent

		guiVersion			= toInt versionParam

	dataFun :: HTTPRequest (Maybe {#Char}) !SessionId !*IWorld -> (!Maybe {#Char}, !Bool, !SessionId, !*IWorld)
	dataFun req _ sessionId iworld
		= case format req of
			JSONGuiEventStream
				# (updates,iworld)	= getUIUpdates sessionId iworld	
				= case updates of
					[]	= (Nothing,False,sessionId,iworld)
					_	= (Just (formatUpdateEvents updates),False,sessionId,iworld)
			_
				= (Nothing,True,sessionId,iworld)	

	disconnectFun :: HTTPRequest SessionId !*IWorld -> *IWorld
	disconnectFun _ _ iworld = iworld

	//Util functions
	format req = case paramValue "format" req of
		"webapp"			= WebApp
		"json-gui"			= JSONGui
		"json-gui-events"	= JSONGuiEventStream
		"json-service"		= JSONService
		"json-plain"		= JSONPlain
		_					= defaultFormat

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

	eventsResponse sessionId events
		= {HTTPResponse | rsp_headers = fromList [("Content-Type","text/event-stream"),("Cache-Control","no-cache")], rsp_data = formatSessionEvent sessionId +++ formatUpdateEvents events}
	
	formatSessionEvent sessionId = "event: session\ndata: " +++ sessionId +++ "\n\n"
	formatUpdateEvents events = "data: " +++ toString (encodeUIUpdates events) +++ "\n\n"

	appStartResponse appName = {newHTTPResponse & rsp_data = toString (appStartPage appName)}
	where
		appStartPage appName = HtmlTag [] [head,body]

		head = HeadTag [] [TitleTag [] [Text appName]: styles ++ scripts]
		body = BodyTag [] []
	
		styles = [LinkTag [RelAttr "stylesheet", HrefAttr file, TypeAttr "text/css"] [] \\ file <- stylefiles]
		scripts = [ScriptTag [SrcAttr file, TypeAttr "text/javascript"] [] \\ file <- scriptfiles]
		
		stylefiles =
			[IF_CLIENT_DEV "bootstrap.css" "build/itwc/production/resources/itwc-all.css"
			 ,"lib/codemirror-2.36/codemirror.css"
			 ,"css/icons.css"
			 ,"css/app.css"
			 ,appName +++ ".css"]

		scriptfiles = (IF_CLIENT_DEV ["ext/ext-debug.js"] [])
			++  ["app/taskeval/utils.js","app/taskeval/itask.js" //UGLY INCLUSION, MUST BE MERGED INTO ITWC FRAMEWORK
				,"app/taskeval/builtin.js","app/taskeval/sapl.js"
				,"app/taskeval/editlet.js"
				,"app/taskeval/db.js", "app/taskeval/debug.js"
				,"app/taskeval/editlet.js"
				,"lib/codemirror-2.36/codemirror.js"
				]
			++ (IF_CLIENT_DEV ["app/app.js"] ["build/itwc/production/all-classes.js"])

	createDocumentsFromUploads [] iworld = ([],iworld)
	createDocumentsFromUploads [(n,u):us] iworld
		# (mbD,iworld)	= createDocument u.upl_filename u.upl_mimetype u.upl_content iworld
		| isError mbD	= createDocumentsFromUploads us iworld
		# (ds,iworld)	= createDocumentsFromUploads us iworld
		= ([fromOk mbD:ds],iworld)
