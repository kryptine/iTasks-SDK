implementation module WebService

import StdList, StdBool
import Time, JSON
import SystemTypes, Task, TaskContext, TaskEval, TaskStore, TUIDiff, TUIEncode, Util, HtmlUtil, Map
import IWorld

webService :: !(Task a) !HTTPRequest !*IWorld -> (!HTTPResponse, !*IWorld) | iTask a
webService task req iworld=:{IWorld|timestamp,application}
	= case showParam of
	//Serve the gui
	"gui"
		| sessionParam == ""
			//Create and evaluate a new session context
			# (mbResult,iworld) = createSessionInstance task iworld
			= case mbResult of
				Error err
					= (response (JSONObject [("success",JSONBool False),("error",JSONString err)]), iworld)
				Ok (TaskException _ err,_)
					= (response (JSONObject [("success",JSONBool False),("error",JSONString err)]), iworld)
				Ok (TaskFinished _, sessionId)
					= (response (JSONObject [("success",JSONBool False),("error",JSONString "Task completed instantly")]), iworld)
				Ok (TaskBusy Nothing _ tree, sessionId)
					= (response (JSONObject [("success",JSONBool False),("error",JSONString "No tui definition available")]), iworld)
				Ok (TaskBusy (Just tui) actions _ , sessionId =: SessionProcess session)
					//Save user interface to enable incremental updates in later requests
					# iworld		= storeTaskTUI sessionId tui iworld
					//Output user interface
					# json = JSONObject [("content",encodeTUIDefinition tui)
										,("session",JSONString session)
										,("timestamp",JSONInt (toInt timestamp))]
					= (response json, iworld)
		| otherwise
			# sessionId					= SessionProcess sessionParam
			//Load previous user interface to enable incremental updates
			# (mbPreviousTui,iworld)	= loadTaskTUI sessionId iworld
			//Check if the version of the user interface the client has is still fresh
			# outdated	= case mbPreviousTui of
				Ok (_,previousTimestamp)	= timestampParam <> "" && Timestamp (toInt timestampParam) < previousTimestamp
				Error _						= False
			//Determine possible edit and commit events
			# editEvent = case fromJSON (fromString editEventParam) of
				Just (target,path,value)
					// ignore edit events of outdated clients
					| not outdated || timestampParam == ""  
						= Just (ProcessEvent (reverse (taskNrFromString target)) (path,value))
					| otherwise
						= Nothing
				Nothing = Nothing			
			# commitEvent = case fromJSON (fromString commitEventParam) of
				Just (target,action)
					// ignore commit events of outdated clients
					| not outdated || timestampParam == "" 
						= Just (ProcessEvent (reverse (taskNrFromString target)) action)
					| otherwise
						= Nothing
				Nothing	= Nothing
			//Evaluate existing session context
			# (mbResult,iworld) = evalSessionInstance sessionId editEvent commitEvent iworld
			# (json,iworld) = case mbResult of
				Error err
					= (JSONObject [("success",JSONBool False),("error",JSONString err)],iworld)
				Ok (TaskException _ err)
					= (JSONObject [("success",JSONBool False),("error",JSONString err)], iworld)
				Ok (TaskFinished _)
					= (JSONObject ([("success",JSONBool True),("done",JSONBool True)]), iworld)
				Ok (TaskBusy mbCurrentTui actions context)
					# json = case (mbPreviousTui,mbCurrentTui) of
						(Ok (previousTui,previousTimestamp),Just currentTui)
							| previousTimestamp == Timestamp (toInt timestampParam) 
								= JSONObject [("success",JSONBool True)
											 ,("updates", encodeTUIUpdates (diffTUIDefinitions previousTui currentTui))
											 ,("timestamp",toJSON timestamp)]
							| otherwise
								= JSONObject [("success",JSONBool True)
											 ,("content",encodeTUIDefinition currentTui)
											 ,("warning",JSONString "The client is outdated. The user interface was refreshed with the most recent value.")
											 ,("timestamp",toJSON timestamp)]
						(_, Just currentTui)
							= JSONObject [("success",JSONBool True)
										 ,("content", encodeTUIDefinition currentTui)
										 ,("timestamp",toJSON timestamp)]
						_
							= JSONObject [("success",JSONBool True),("done",JSONBool True)]
					//Store tui for later incremental requests
					# iworld = case mbCurrentTui of
						Just currentTui	= storeTaskTUI sessionId currentTui iworld
						Nothing			= iworld
					= (json,iworld)
				_
					= (JSONObject [("success",JSONBool False),("error",JSONString  "Unknown exception")],iworld)
			= (response json, iworld)
	
	_	//Serve start page
		=  (appStartResponse application, iworld)
where
	showParam			= paramValue "show" req
	sessionParam		= paramValue "session" req
	downloadParam		= paramValue "download" req
	uploadParam			= paramValue "upload" req
	timestampParam		= paramValue "timestamp" req
	editEventParam		= paramValue "editEvent" req
	commitEventParam	= paramValue "commitEvent" req
	
	response json
		= {HTTPResponse | rsp_headers = fromList [("Content-Type","text/json")], rsp_data = toString json}
	