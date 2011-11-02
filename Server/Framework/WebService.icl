implementation module WebService

import StdList, StdBool
import Time, JSON
import SystemTypes, Task, TaskContext, TaskEval, TaskStore, TUIDiff, TUIEncode, Util, HtmlUtil, Map
import Engine, IWorld

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
					# (mbResult, iworld) = createSessionInstance task True iworld
					= (mbResult, Error "Fresh session, no previous user interface", iworld)
				sessionId
					//Check if there is a previous tui definition and check if it is still current
					# (mbPreviousTui,iworld)	= loadTaskTUI (SessionProcess sessionId) iworld
					//Check if the version of the user interface the client has is still fresh
					# outdated = case mbPreviousTui of
						Ok (_,previousTimestamp)	= timestampParam <> "" && Timestamp (toInt timestampParam) < previousTimestamp
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
					Ok (TaskFinished _,_)
						= (JSONObject ([("success",JSONBool True),("done",JSONBool True)]), iworld)
					Ok (TaskBusy mbCurrentTui actions context,SessionProcess sessionId)
						# json = case (mbPrevTui,mbCurrentTui) of
							(Ok (previousTui,previousTimestamp),TUIRep currentTui)
								| previousTimestamp == Timestamp (toInt timestampParam) 
									= JSONObject [("success",JSONBool True)
												 ,("session",JSONString sessionId)
												 ,("updates", encodeTUIUpdates (diffTUIDefinitions previousTui currentTui))
												 ,("timestamp",toJSON timestamp)]
								| otherwise
									= JSONObject [("success",JSONBool True)
												 ,("session",JSONString sessionId)
												 ,("content",encodeTUIDefinition currentTui)
												 ,("warning",JSONString "The client is outdated. The user interface was refreshed with the most recent value.")
												 ,("timestamp",toJSON timestamp)]
							(_, TUIRep currentTui)
								= JSONObject [("success",JSONBool True)
											 ,("session",JSONString sessionId)
											 ,("content", encodeTUIDefinition currentTui)
											 ,("timestamp",toJSON timestamp)]
							_
								= JSONObject [("success",JSONBool True),("done",JSONBool True)]
						//Store tui for later incremental requests
						# iworld = case mbCurrentTui of
							TUIRep currentTui	= storeTaskTUI (SessionProcess sessionId) currentTui iworld
							_					= iworld
						= (json,iworld)
					_
						= (JSONObject [("success",JSONBool False),("error",JSONString  "Unknown exception")],iworld)
			= (jsonResponse json, iworld)
		//Serve the task as an easily accessable JSON service
		JSONService
			# (mbResult,iworld)	= case sessionParam of
				""	= createSessionInstance task False iworld
				sessionId
					= evalSessionInstance (SessionProcess sessionId) Nothing Nothing False iworld
			= case mbResult of
				Ok (TaskException _ err,_)
					= (errorResponse err, iworld)
				Ok (TaskFinished val,_)
					= (errorResponse "TODO: yield value when done", iworld)
				Ok (TaskBusy (ServiceRep rep) actions _,_)
					= (errorResponse "TODO: return useful service representation", iworld)
				Ok (TaskBusy _ _ _,_)
					= (errorResponse "Requested service format not available", iworld)
				_
					= (errorResponse "Not implemented", iworld)
		//Serve the task in a minimal JSON representation
		JSONServiceRaw
			= (jsonResponse (JSONString "Not implemented"), iworld)
		//Error unimplemented type
		_
			= (jsonResponse (JSONString "Unknown service format"), iworld)
		
where
	format			= case formatParam of
		"webapp"			= WebApp
		"json-gui"			= JSONGui
		"json-service"		= JSONService
		"json-service-raw"	= JSONServiceRaw
		_					= defaultFormat
		 
	formatParam			= paramValue "format" req
	sessionParam		= paramValue "session" req
	downloadParam		= paramValue "download" req
	uploadParam			= paramValue "upload" req
	timestampParam		= paramValue "timestamp" req
	editEventParam		= paramValue "editEvent" req
	editEvent			= case (fromJSON (fromString editEventParam)) of
		Just (target,path,value)	= Just (ProcessEvent (reverse (taskNrFromString target)) (path,value))
		_							= Nothing
	commitEventParam	= paramValue "commitEvent" req
	commitEvent			= case (fromJSON (fromString commitEventParam)) of
		Just (target,action)		= Just (ProcessEvent (reverse (taskNrFromString target)) action)
		_							= Nothing
	jsonResponse json
		= {HTTPResponse | rsp_headers = fromList [("Content-Type","text/json")], rsp_data = toString json}
	
	errorResponse msg
		= {HTTPResponse | rsp_headers = fromList [("Status", "500 Internal Server Error")], rsp_data = msg}
	