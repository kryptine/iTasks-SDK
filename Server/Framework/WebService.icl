implementation module WebService

import StdList, StdBool
import Time, JSON
import SystemTypes, Task, TaskContext, TaskEval, TaskStore, TUIDiff, TUIEncode, Util, HtmlUtil, Map
import Engine, IWorld

//The representation of the JSON service
:: ServiceResponse :== [ServiceResponsePart]
:: ServiceResponsePart =
	{ taskId	:: !String
	, partId	:: !Int
	, value		:: !JSONNode
	, actions	:: ![String]
	}
	
derive JSONEncode ServiceResponsePart

webService :: !(Task a) !ServiceFormat !HTTPRequest !*IWorld -> (!HTTPResponse, !*IWorld) | iTask a
webService task defaultFormat req iworld=:{IWorld|timestamp,application}
	= case format of
		//Serve start page
		WebApp	
			=  (appStartResponse application, iworld)
		//Serve the user interface representations
		JSONGui
			//Load or create session context and edit / evaluate
			# (mbResult, mbPrevTui, iworld)	= case sessionParam of
				""	
					# (mbResult, iworld) = createSessionInstance task Nothing Nothing True iworld
					= (mbResult, Error "Fresh session, no previous user interface", iworld)
				sessionId
					//Check if there is a previous tui definition and check if it is still current
					# (mbPreviousTui,iworld)	= loadTaskTUI (SessionProcess sessionId) iworld
					//Check if the version of the user interface the client has is still fresh
					# outdated = case mbPreviousTui of
						Ok (_,prevGuiVersion)		= guiVersion < prevGuiVersion
						Error _						= False
					| outdated
						# (mbResult, iworld) = evalSessionInstance (SessionProcess sessionId) Nothing Nothing True iworld
						= (mbResult,mbPreviousTui,iworld)
					| otherwise
						# (mbResult, iworld) = evalSessionInstance (SessionProcess sessionId) editEvent commitEvent True iworld
						= (mbResult,mbPreviousTui,iworld)
			# (json, iworld) = case mbResult of
					Error err
						= (JSONObject [("success",JSONBool False),("error",JSONString err)],iworld)
					Ok (TaskException _ err,_)
						= (JSONObject [("success",JSONBool False),("error",JSONString err)], iworld)
					Ok (TaskStable _ _ _,_)
						= (JSONObject ([("success",JSONBool True),("done",JSONBool True)]), iworld)
					Ok (TaskInstable _ (mbCurrentTui,actions) context,SessionProcess sessionId)
						# json = case (mbPrevTui,mbCurrentTui) of
							(Ok (previousTui,prevGuiVersion),TUIRep currentTui)
								| prevGuiVersion == guiVersion - 1 //The stored version, is exactly one less then the current version 
									= JSONObject [("success",JSONBool True)
												 ,("session",JSONString sessionId)
												 ,("updates", encodeTUIUpdates (diffTUIDefinitions previousTui currentTui))
												 ,("timestamp",toJSON timestamp)]
								| otherwise
									= JSONObject [("success",JSONBool True)
												 ,("session",JSONString sessionId)
												 ,("content",encodeTUIDefinition currentTui)
												 ,("warning",JSONString "The client is out of sync. The user interface was refreshed with the most recent value.")
												 ,("timestamp",toJSON timestamp)]
							(_, TUIRep currentTui)
								= JSONObject [("success",JSONBool True)
											 ,("session",JSONString sessionId)
											 ,("content", encodeTUIDefinition currentTui)
											 ,("timestamp",toJSON timestamp)]
							_
								= JSONObject [("success",JSONBool True),("done",JSONBool True)]
						//Store gui for later incremental requests
						# iworld = case mbCurrentTui of
							TUIRep currentTui	= storeTaskTUI (SessionProcess sessionId) currentTui guiVersion iworld
							_					= iworld
						= (json,iworld)
					_
						= (JSONObject [("success",JSONBool False),("error",JSONString  "Unknown exception")],iworld)
			= (jsonResponse json, iworld)
		//Serve the task in easily accessable JSON representation
		JSONService
			# (mbResult,iworld)	= case sessionParam of
				""	= createSessionInstance task Nothing Nothing False iworld
				sessionId
					= evalSessionInstance (SessionProcess sessionId) Nothing Nothing False iworld
			= case mbResult of
				Ok (TaskException _ err,_)
					= (errorResponse err, iworld)
				Ok (TaskStable val _ _,_)
					= (jsonResponse (serviceDoneResponse val), iworld)
				Ok (TaskInstable _ (ServiceRep rep,actions) _,_)
					= (jsonResponse (serviceBusyResponse rep actions), iworld)
				Ok (TaskInstable _ _ _,_)
					= (errorResponse "Requested service format not available for this task", iworld)
		//Serve the task in a minimal JSON representation (only possible for non-parallel instantly completing tasks)
		JSONPlain
			//HACK: REALLY REALLY REALLY UGLY THAT IT IS NECCESARY TO EVAL TWICE
			# (mbResult,iworld) = createSessionInstance task Nothing Nothing False iworld
			# (mbResult,iworld) = case mbResult of
				(Ok (_,sessionId))	= evalSessionInstance sessionId luckyEdit luckyCommit False iworld
				(Error e)			= (Error e,iworld)
			= case mbResult of
				Ok (TaskException _ err,_)
					= (errorResponse err, iworld)
				Ok (TaskStable val _ _,_)
					= (plainDoneResponse val, iworld)
				_
					= (errorResponse "Requested service format not available for this task", iworld)
		//Error unimplemented type
		_
			= (jsonResponse (JSONString "Unknown service format"), iworld)	
where
	format			= case formatParam of
		"webapp"			= WebApp
		"json-gui"			= JSONGui
		"json-service"		= JSONService
		"json-plain"		= JSONPlain
		_					= defaultFormat
		 
	formatParam			= paramValue "format" req
	sessionParam		= paramValue "session" req
	downloadParam		= paramValue "download" req
	uploadParam			= paramValue "upload" req
	versionParam		= paramValue "version" req
	editEventParam		= paramValue "editEvent" req
	editEvent			= case (fromJSON (fromString editEventParam)) of
		Just (target,path,value)	= Just (ProcessEvent (reverse (taskNrFromString target)) (path,value))
		_							= Nothing
	commitEventParam	= paramValue "commitEvent" req
	commitEvent			= case (fromJSON (fromString commitEventParam)) of
		Just (target,action)		= Just (ProcessEvent (reverse (taskNrFromString target)) action)
		_							= Nothing

	guiVersion			= toInt versionParam
	
	//Parse the body of the request as JSON message
	(luckyEdit,luckyCommit) = if(req.req_data == "")
		(Nothing,Nothing)	
		(Just (LuckyEvent ("0",fromString req.req_data)), Just (LuckyEvent ""))
	
	jsonResponse json
		= {HTTPResponse | rsp_headers = fromList [("Content-Type","text/json")], rsp_data = toString json}
	errorResponse msg
		= {HTTPResponse | rsp_headers = fromList [("Status", "500 Internal Server Error")], rsp_data = msg}
			
	serviceBusyResponse rep actions
		= JSONObject [("status",JSONString "busy"),("parts",parts)]
	where
		parts = toJSON [{ServiceResponsePart|taskId = taskId, partId = partId, value = value, actions = findActions taskId actions} \\ (taskId,partId,value) <- rep]
		findActions taskId actions
			= [actionName action \\ (task,action,enabled) <- actions | enabled && task == taskId]
	serviceDoneResponse (Container val :: Container a a)
		= JSONObject [("status",JSONString "complete"),("value",toJSON val)]
	serviceDoneResponse _
		= serviceErrorResponse "Corrupt result value"
	serviceErrorResponse e
		= JSONObject [("status",JSONString "error"),("error",JSONString e)]
		
	plainDoneResponse (Container val :: Container a a)
		= jsonResponse (toJSON val)
	plainDoneResponse _
		= errorResponse "Corrupt result value"

	appStartResponse appName = {newHTTPResponse & rsp_data = toString (appStartPage appName)}

	appStartPage appName = HtmlTag [] [head,body]
	where
		head = HeadTag [] [TitleTag [] [Text "Loading..."]: styles ++ scripts]
		body = BodyTag [] []
	
		styles = [LinkTag [RelAttr "stylesheet", HrefAttr file, TypeAttr "text/css"] [] \\ file <- stylefiles]
		scripts = [ScriptTag [SrcAttr file, TypeAttr "text/javascript"] [] \\ file <- scriptfiles]
		
		stylefiles = ["/lib/ext-4.0.2a/resources/css/ext-all-gray.css"
					 ,"/css/main.css"
					 ,appName +++ ".css"]
		//scriptfiles = ["/lib/ext-4.0.2a/ext-debug.js","/app.js"]
		scriptfiles = ["/lib/ext-4.0.2a/ext.js","/app-all.js"]
		
