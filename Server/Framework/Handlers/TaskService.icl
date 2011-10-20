implementation module TaskService

import StdList, StdBool
import Time, JSON
import SystemTypes, Task, TaskContext, TaskEval,  TUIDiff, TUIEncode, Util, HtmlUtil, Map
import IWorld

from ProcessDB	import qualified class ProcessDB(..), instance ProcessDB IWorld
from SessionDB	import qualified class SessionDB(..), instance SessionDB IWorld
from WorkflowDB import qualified class WorkflowDB(..), instance WorkflowDB IWorld

derive bimap Maybe, (,)

derive JSONEncode TUIDef, TUIDefContent, TUIUpdate, TUIIcon, TUIButton, TUIMenuButton, TUIMenu, TUIMenuItem, Hotkey
derive JSONEncode TUIControlType
derive JSONEncode TUIButtonControl, TUIListItem
derive JSONEncode TUIContainer, TUIPanel, TUITabContainer, TUITabItem, TUIBorderContainer, TUIBorderItem, TUIListContainer, TUIGridControl, TUITree, TUIEditControl, TUIShowControl, TUIRadioChoice, TUICheckChoice, TUISize, TUIVAlign, TUIHAlign, TUIDirection, TUIMinSize, TUIMargins

derive JSONDecode TUIDef, TUIDefContent, TUIUpdate, TUIIcon, TUIButton, TUIMenuButton, TUIMenu, TUIMenuItem, Hotkey
derive JSONDecode TUIControlType
derive JSONDecode TUIButtonControl, TUIListItem
derive JSONDecode TUIContainer, TUIPanel, TUITabContainer, TUITabItem, TUIBorderContainer, TUIBorderItem, TUIListContainer, TUIGridControl, TUITree, TUIEditControl, TUIShowControl, TUIRadioChoice, TUICheckChoice, TUISize, TUIVAlign, TUIHAlign, TUIDirection, TUIMinSize, TUIMargins
	
JSONEncode{|HtmlTag|} htm = [JSONString (toString htm)]

taskService :: !(Task a) !HTTPRequest !*IWorld -> (!HTTPResponse, !*IWorld) | iTask a
taskService task req iworld=:{IWorld|timestamp,application} = case showParam of
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
					# tuiStoreId	= toString sessionId +++ "-gui"
					# iworld	 	= storeValue tuiStoreId tui iworld
					//Output user interface
					# json = JSONObject [("content",encodeTUIDefinition tui)
										,("session",JSONString session)
										,("timestamp",JSONInt (toInt timestamp))]
					= (response json, iworld)
		| otherwise
			# sessionId					= SessionProcess sessionParam
			//Load previous user interface to enable incremental updates
			# tuiStoreId				= toString sessionId +++ "-gui"
			# (mbPreviousTui,iworld)	= loadValueAndTimestamp tuiStoreId iworld
			//Check if the version of the user interface the client has is still fresh
			# outdated	= case mbPreviousTui of
				Just (_,previousTimestamp)	= timestampParam <> "" && Timestamp (toInt timestampParam) < previousTimestamp
				Nothing						= False
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
						(Just (previousTui,previousTimestamp),Just currentTui)
							| previousTimestamp == Timestamp (toInt timestampParam) 
								= JSONObject [("success",JSONBool True)
											 ,("updates", encodeTUIUpdates (diffTUIDefinitions previousTui currentTui))
											 ,("timestamp",toJSON timestamp)]
							| otherwise
								= JSONObject [("success",JSONBool True)
											 ,("content",encodeTUIDefinition currentTui)
											 ,("warning",JSONString "The client is outdated. The user interface was refreshed with the most recent value.")
											 ,("timestamp",toJSON timestamp)]
						(Nothing, Just currentTui)
							= JSONObject [("success",JSONBool True)
										 ,("content", encodeTUIDefinition currentTui)
										 ,("timestamp",toJSON timestamp)]
						_
							= JSONObject [("success",JSONBool True),("done",JSONBool True)]
					//Store tui for later incremental requests
					# iworld = case mbCurrentTui of
						Just currentTui	= storeValue tuiStoreId currentTui iworld
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
	timestampParam		= paramValue "timestamp" req
	editEventParam		= paramValue "editEvent" req
	commitEventParam	= paramValue "commitEvent" req
	
	response json
		= {HTTPResponse | rsp_headers = fromList [("Content-Type","text/json")], rsp_data = toString json}
	
		
taskServiceOld :: !String !String ![String] !HTTPRequest !*IWorld -> (!HTTPResponse, !*IWorld)
taskServiceOld url format path req iworld=:{IWorld|timestamp}
	# html					= format == "html"
	# (mbSession,iworld)	= 'SessionDB'.restoreSession sessionParam iworld
	= case path of
		//List tasks
		[]
			| isError mbSession
				= (serviceResponse html "Task list" listDescription url listParams (jsonSessionErr mbSession), iworld)					
			# (processes,iworld)	= case (fromOk mbSession).user of
				RootUser
					| userParam == ""	= 'ProcessDB'.getProcessesForUser RootUser [Running] iworld
					| otherwise			= 'ProcessDB'.getProcessesForUser (NamedUser userParam) [Running] iworld
				user			= 'ProcessDB'.getProcessesForUser (fromOk mbSession).user [Running] iworld		
			# items				= taskItems processes
			# json				= JSONObject [("success",JSONBool True),("tasks",JSONArray items)]
			= (serviceResponse html "Task list" listDescription url listParams json, iworld)
		//For debugging, list all tasks in the process table
		["debug"]
			| isError mbSession
				= (serviceResponse html "Task debug list" listDebugDescription url debugParams (jsonSessionErr mbSession), iworld)	
			# (processes,iworld)	= 'ProcessDB'.getProcesses [Running,Finished,Excepted,Deleted] iworld
			# json					= JSONObject [("success",JSONBool True),("tasks",toJSON processes)]
			= (serviceResponse html "Task debug list" listDebugDescription url debugParams json, iworld)
		//Start a new task (create a process)
		["create"]
			| isError mbSession
				= (serviceResponse html "Create task" createDescription url createParams (jsonSessionErr mbSession), iworld)	 
			# (mbResult,iworld)	= createTopInstance (toInt workflowParam) (fromOk mbSession).user (if (paramParam == "") Nothing (Just (fromString paramParam))) iworld
			# json = case mbResult of
				Error err
					= JSONObject [("success",JSONBool False),("error",JSONString err)]
				Ok (TaskException _ err,_)
					= JSONObject [("success",JSONBool False),("error",JSONString err)]
				Ok (_,taskNr)
					= JSONObject [("success",JSONBool True),("taskId",JSONInt (last taskNr))]
			= (serviceResponse html "Create task" createDescription url createParams json, iworld)
		//Show properties of an individual task without further evaluating it.	
		[taskId]
			| isError mbSession
				= (serviceResponse html "Task details" detailsDescription url detailsParams (jsonSessionErr mbSession), iworld)
			# (mbProcess, iworld)	= 'ProcessDB'.getProcess (processOf taskId) iworld
			= case mbProcess of
				Nothing
					= (notFoundResponse req, iworld)
				Just proc
					# json = JSONObject [("success",JSONBool True),("task",toJSON proc.Process.properties)]
					= (serviceResponse html "Task details" detailsDescription url detailsParams json, iworld)
		//Evaluates a workflow instance (does not require being logged in) 
		[taskId,"refresh"]
			//Evaluate with
			//# (mbResult,iworld)	= evaluateWorkflowInstance (processOf taskId) Nothing Nothing [] iworld
			# (mbResult,iworld)		= evalTopInstance (taskNrFromString taskId) (fromOk mbSession).user Nothing Nothing iworld
			# json = case mbResult of
				Error err
					= JSONObject [("success",JSONBool False),("error",JSONString err)]
				Ok (TaskException _ err)
					= JSONObject [("success",JSONBool False),("error",JSONString err)]	
				_
					= JSONObject [("success",JSONBool True)]
			= (serviceResponse html "Task details" refreshDescription url [] json, iworld)
		//Dump the raw task context datastructure
		[taskId,"debug"]
			| isError mbSession
				= (serviceResponse html "Task debug" taskDebugDescription url debugParams (jsonSessionErr mbSession), iworld)
			//# (mbResult,iworld)	= evaluateWorkflowInstance (processOf taskId) Nothing Nothing [] iworld
			# (mbResult,iworld)		= evalTopInstance (taskNrFromString taskId) (fromOk mbSession).user Nothing Nothing iworld
			# json = case mbResult of
				Error err
					= JSONObject [("success",JSONBool False),("error",JSONString err)]
				Ok (TaskBusy _ _ context)
					= JSONObject [("success",JSONBool False),("tree",toJSON context)]
				Ok (TaskFinished _)
					= JSONObject [("success",JSONBool True),("result",JSONString "finished")]
				Ok (TaskException _ err)
					= JSONObject [("success",JSONBool False),("error",JSONString err)]
			= (serviceResponse html "Task debug" taskDebugDescription url debugParams json, iworld)	
		//Show / Update task user interface definition
		[taskId,"tui"]
			| isError mbSession
				= (serviceResponse html "Task user interface" tuiDescription url tuiParams (jsonSessionErr mbSession), iworld)
			//Load previous user interface to enable incremental updates
			# tuiStoreId				= "Process-" +++ taskId +++ "-tui"
			# (mbPreviousTui,iworld)	= loadValueAndTimestamp tuiStoreId iworld
			//Check if the version of the user interface the client has is still fresh
			# outdated	= case mbPreviousTui of
				Just (_,previousTimestamp)	= timestampParam <> "" && Timestamp (toInt timestampParam) < previousTimestamp
				Nothing					= False
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
			//We need the current timestamp to include in the response
			# (timestamp,iworld)	= getTimestamp iworld
			//Evaluate the workflow instance
			//# tuiTaskNr			= taskNrFromString taskId
			//# (mbResult,iworld)	= evaluateWorkflowInstance (processOf taskId) editEvent commitEvent tuiTaskNr iworld
			# (mbResult,iworld)	= evalTopInstance (taskNrFromString taskId) (fromOk mbSession).user editEvent commitEvent iworld
			# (json,iworld) = case mbResult of
				Error err
					= (JSONObject [("success",JSONBool False),("error",JSONString err)],iworld)
								
				Ok (TaskBusy mbCurrentTui actions context)
					//Determine content or updates
					# tui = case (mbPreviousTui,mbCurrentTui) of
						(Just (previousTui,previousTimestamp),Just currentTui)
							| previousTimestamp == Timestamp (toInt timestampParam) 
								= JSONObject [("updates",encodeTUIUpdates (diffTUIDefinitions previousTui currentTui))]
							| otherwise
								= JSONObject [("content",encodeTUIDefinition currentTui)]
						(Nothing, Just currentTui)
							= JSONObject [("content", encodeTUIDefinition currentTui)]
						_
							= JSONString "done"
							
					//Store tui for later incremental requests
					# iworld = case mbCurrentTui of
						Just currentTui	= storeValue tuiStoreId currentTui iworld
						Nothing			= iworld
						
					//Build output structure
					# json	= JSONObject [("success",JSONBool True)
										 ,("timestamp",toJSON timestamp)
								 		 ,("tui",tui)
								 		 :if outdated [("warning",JSONString "The client is outdated. The user interface was refreshed with the most recent value.")] []
								 		 ]
					= (json, iworld)
					
				Ok (TaskFinished _)
					= (JSONObject ([("success",JSONBool True),("timestamp",toJSON timestamp),("tui",JSONString "done")]), iworld)
				Ok (TaskException _ err)
					= (JSONObject [("success",JSONBool False),("error",JSONString err)], iworld)
				_
					= (JSONObject [("success",JSONBool False),("error",JSONString  "Unknown exception")],iworld)
					 
			= (serviceResponse html "Task user interface" tuiDescription url tuiParams json,iworld) 
	
		//Cancel / Abort / Delete the current task
		[taskId,"cancel"]
			| isError mbSession
				= (serviceResponse html "Cancel task" cancelDescription url detailsParams (jsonSessionErr mbSession), iworld)
			| length (taskNrFromString taskId) > 1 // don't delete detached
				= (serviceResponse html "Cancel task" cancelDescription url detailsParams (JSONObject [("success",JSONBool False),("error",JSONString  "Cannot delete detached process")]), iworld)
			# (iworld) = 'ProcessDB'.deleteProcess (processOf taskId) iworld
			= (serviceResponse html "Cancel task" cancelDescription url detailsParams (JSONObject [("success",JSONBool True),("message",JSONString "Task deleted")]), iworld)	
		_
			= (notFoundResponse req, iworld)
where
	showParam			= paramValue "show" req
	sessionParam		= paramValue "session" req
	userParam			= paramValue "user" req
	timestampParam		= paramValue "timestamp" req
	
	createParams		= [("session",sessionParam,True),("workflow",workflowParam,True),("parameter",paramParam,False)]
	workflowParam		= paramValue "workflow" req
	paramParam			= paramValue "parameter" req
	
	listParams			= [("session",sessionParam,True),("user",userParam,False)]
	
	debugParams			= [("session",sessionParam,True),("type",typeParam,False)]
	typeParam			= paramValue "type" req
	
	detailsParams		= [("session",sessionParam,True),("editEvent",editEventParam,False),("commitEvent",commitEventParam,False)]
	editEventParam		= paramValue "editEvent" req
	commitEventParam	= paramValue "commitEvent" req
	
	tuiParams			= [("session",sessionParam,True),("editEvent",editEventParam,False),("commitEvent",commitEventParam,False),("since",sinceParam,False)]
	sinceParam			= paramValue "since" req
	
	propParams			= [("session",sessionParam,True),("update",updateParam,False)]
	updateParam			= paramValue "update" req
	
	jsonSessionErr (Error error)
						= JSONObject [("success",JSONBool False),("error", JSONString error)]
	
	//TODO: Refactor this mechanism.
	//It is a bit of a shame that the tree structure is flattened here and
	//then reconstructed again on the client...
	//And very ugly and inefficient :)
	taskItems processes = items Nothing processes
	where
		items parent processes
			= flatten (map (item parent) processes)
		item parent process
			# json = case parent of
				Nothing = toJSON process.Process.properties
				Just taskId
					# json = toJSON process.Process.properties
					= addParent taskId json
			= [json: items (Just process.Process.properties.systemProperties.SystemProperties.taskId) process.Process.subprocesses]
	
		addParent taskId (JSONObject [("taskProperties",taskProperties),("managerProperties",managerProperties),("systemProperties",JSONObject systemProperties)])
			= JSONObject [("taskProperties",taskProperties),("managerProperties",managerProperties),("systemProperties",JSONObject [("parent",JSONString taskId):systemProperties])]
	
	
	processOf taskId	= case taskNrFromString taskId of
		[]		= WorkflowProcess 0
		taskNr	= WorkflowProcess (last taskNr)
	
	taskProperties :: Process -> [(String,JSONNode)]
	taskProperties proc = case (toJSON proc.Process.properties) of (JSONObject fields) = fields

	getTimestamp :: !*IWorld -> (!Timestamp,!*IWorld)
	getTimestamp iworld=:{IWorld|timestamp} = (timestamp,iworld)

	
listDescription			:== "This service lists all tasks for the user of the provided session."
listDebugDescription	:== "This service dumps all information currently in the process database of running instances."
detailsDescription		:== "This service provides all meta-properties of a running task instance."
createDescription		:== "This service let's you create new instances of a workflow.<br />"
						+++ "The 'workflow' parameter is the path of a workflow (separated by slashes) as listed by the workflow directory service. "
						+++ "E.g. Foo/Bar/Baz"
taskDebugDescription	:== "This service dumps all information about a running task instance. Both its meta-properties and its task tree."
cancelDescription		:== "This service let's you cancel (delete) a running task instance."
tuiDescription 			:== "This yields an abstract user interface description for the current task."
refreshDescription		:== "This service recalculates the task tree of a running task instance."