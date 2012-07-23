implementation module WebService

import StdList, StdBool
import Time, JSON_NG
import SystemTypes, Task, TaskState, TaskEval, TaskStore, UIDiff, Util, HtmlUtil, Map
import Engine, IWorld

//The representation of the JSON service
:: ServiceResponse :== [ServiceResponsePart]
:: ServiceResponsePart =
	{ taskId	:: !String
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
					# (mbResult, iworld) = createSessionInstance task Nothing Nothing iworld
					= (mbResult, Nothing, iworld)
				sessionId
					//Check if there is a previous tui definition and check if it is still current
					# (mbPreviousTui,iworld)	= loadPrevUI sessionId guiVersion iworld
					# (mbResult, iworld) 		= evalSessionInstance sessionId editEvent commitEvent iworld
					= (mbResult,mbPreviousTui,iworld)
			# (json, iworld) = case mbResult of
					Error err
						= (JSONObject [("success",JSONBool False),("error",JSONString err)],iworld)
					Ok (ExceptionResult _ err,_,_)
						= (JSONObject [("success",JSONBool False),("error",JSONString err)], iworld)
					Ok (ValueResult (Value _ Stable) _ _ _,_,_)
						= (JSONObject ([("success",JSONBool True),("done",JSONBool True)]), iworld)
					Ok (ValueResult _ _ mbCurrentTui context,_,sessionId)
						# json = case (mbPrevTui,mbCurrentTui) of
							(Just previousTui,TaskRep (_,Just currentTui,actions,attributes) _)
									= JSONObject [("success",JSONBool True)
												 ,("session",JSONString sessionId)
												 ,("updates", encodeUIUpdates (diffUIDefinitions previousTui currentTui editEvent))
												 ,("timestamp",toJSON timestamp)]
							(_, TaskRep (_,Just currentTui,actions,attributes) _)
								= JSONObject [("success",JSONBool True)
											 ,("session",JSONString sessionId)
											 ,("content", encodeUIDefinition currentTui)
											 ,("timestamp",toJSON timestamp)]
							_
								= JSONObject [("success",JSONBool True),("done",JSONBool True)]
						//Store gui for later incremental requests
						# iworld = case mbCurrentTui of
							TaskRep (_,Just currentTui,_,_) _	= storeCurUI sessionId guiVersion currentTui iworld
							_									= iworld
						= (json,iworld)
					_
						= (JSONObject [("success",JSONBool False),("error",JSONString  "Unknown exception")],iworld)
			= (jsonResponse json, iworld)
		//Serve the task in easily accessable JSON representation
		JSONService
			# (mbResult,iworld)	= case sessionParam of
				""	= createSessionInstance task Nothing Nothing iworld
				sessionId
					= evalSessionInstance sessionId Nothing Nothing iworld
			= case mbResult of
				Ok (ExceptionResult _ err,_,_)
					= (errorResponse err, iworld)
				Ok (ValueResult (Value val Stable) _ _ _,_,_)
					= (jsonResponse (serviceDoneResponse val), iworld)
				Ok (ValueResult _ _ (TaskRep (_,_,actions,attributes) rep) _,_,_)
					= (jsonResponse (serviceBusyResponse rep actions attributes), iworld)
		//Serve the task in a minimal JSON representation (only possible for non-parallel instantly completing tasks)
		JSONPlain
			//HACK: REALLY REALLY REALLY UGLY THAT IT IS NECCESARY TO EVAL TWICE
			# (mbResult,iworld) = createSessionInstance task Nothing Nothing iworld
			# (mbResult,iworld) = case mbResult of
				(Ok (_,instanceId,sessionId))	
					# (luckyEdit,luckyCommit) = if (req.req_data == "")
						(Nothing,Nothing)	
						(Just (LuckyEvent instanceId ("",fromString req.req_data)), Just (LuckyEvent instanceId ""))
					= evalSessionInstance sessionId luckyEdit luckyCommit iworld
				(Error e)			= (Error e,iworld)
			= case mbResult of
				Ok (ExceptionResult _ err,_,_)
					= (errorResponse err, iworld)
				Ok (ValueResult (Value val Stable) _ _ _,_,_)
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
//	downloadParam		= paramValue "download" req
//	uploadParam			= paramValue "upload" req
	versionParam		= paramValue "version" req
	editEventParam		= paramValue "editEvent" req
	editEvent			= case (fromJSON (fromString editEventParam)) of
		Just (task,path,value)	= Just (TaskEvent (fromString task) (path,value))
		_						= Nothing
	commitEventParam	= paramValue "commitEvent" req
	commitEvent			= case (fromJSON (fromString commitEventParam)) of
		Just (task,action)		= Just (TaskEvent (fromString task) action)
		_						= Nothing

	guiVersion			= toInt versionParam

	jsonResponse json
		= {HTTPResponse | rsp_headers = fromList [("Content-Type","text/json")], rsp_data = toString json}
	errorResponse msg
		= {HTTPResponse | rsp_headers = fromList [("Status", "500 Internal Server Error")], rsp_data = msg}
			
	serviceBusyResponse rep actions attributes
		= JSONObject [("status",JSONString "busy"),("parts",parts),("attributes",JSONObject [(k,JSONString v) \\ (k,v) <- attributes])]
	where
		parts = toJSON [{ServiceResponsePart|taskId = toString taskId, value = value, actions = findActions taskId actions} \\ (taskId,value) <- rep]
		findActions taskId actions
			= [actionName action \\ (task,action,enabled) <- actions | enabled && task == taskId]
	
	serviceDoneResponse val
		= JSONObject [("status",JSONString "complete"),("value",val)]
	serviceErrorResponse e
		= JSONObject [("status",JSONString "error"),("error",JSONString e)]

	plainDoneResponse val = jsonResponse val
	
	appStartResponse appName = {newHTTPResponse & rsp_data = toString (appStartPage appName)}

	appStartPage appName = HtmlTag [] [head,body]
	where
		head = HeadTag [] [TitleTag [] [Text appName]: styles ++ scripts]
		body = BodyTag [] []
	
		styles = [LinkTag [RelAttr "stylesheet", HrefAttr file, TypeAttr "text/css"] [] \\ file <- stylefiles]
		scripts = [ScriptTag [SrcAttr file, TypeAttr "text/javascript"] [] \\ file <- scriptfiles]
		
		stylefiles = ["lib/extjs-4.1.0/resources/css/ext-all-gray.css"
					 ,"css/app.css"
					 ,appName +++ ".css"]
		scriptfiles = ["lib/extjs-4.1.0/ext-debug.js",
					   "app/taskeval/utils.js","app/taskeval/itask.js", //UGLY INCLUSION, MUST BE MERGED INTO ITWC FRAMEWORK
					   "app/taskeval/builtin.js","app/taskeval/sapl.js",
					   "app/taskeval/db.js", "app/taskeval/debug.js",				   
					   "app.js"]
		//scriptfiles = ["/lib/ext-4.1.0/ext.js","/app-all.js"]
		