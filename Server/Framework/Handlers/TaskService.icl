implementation module TaskService

import StdList, StdBool
import Time, JSON
import SystemTypes, Task, TaskInstance, TaskContext, TUIDiff, TUIEncode, Util, HtmlUtil
import IWorld

from ProcessDB	import qualified class ProcessDB(..), instance ProcessDB IWorld
from SessionDB	import qualified class SessionDB(..), instance SessionDB IWorld
from WorkflowDB import qualified class WorkflowDB(..), instance WorkflowDB IWorld

derive bimap Maybe, (,)

derive JSONEncode TUIDef, TUIDefContent, TUIButton, TUIUpdate, TUIMenuButton, TUIMenu, TUIMenuItem, Hotkey
derive JSONEncode TUIControlType, TUIConstructorControl
derive JSONEncode TUIButtonControl, TUIListItem, TUIChoiceControl
derive JSONEncode TUILayoutContainer, TUITabContainer, TUITab, TUIMainContainer, TUIListContainer, TUIGridControl, TUITree, TUIControl, TUISize, TUIVGravity, TUIHGravity, TUIOrientation, TUIMinSize, TUIMargins

derive JSONDecode TUIDef, TUIDefContent, TUIButton, TUIUpdate, TUIMenuButton, TUIMenu, TUIMenuItem, Hotkey
derive JSONDecode TUIControlType, TUIConstructorControl
derive JSONDecode TUIButtonControl, TUIListItem, TUIChoiceControl
derive JSONDecode TUILayoutContainer, TUITabContainer, TUITab, TUIMainContainer, TUIListContainer, TUIGridControl, TUITree, TUIControl, TUISize, TUIVGravity, TUIHGravity, TUIOrientation, TUIMinSize, TUIMargins
	
JSONEncode{|HtmlTag|} htm = [JSONString (toString htm)]

taskService :: !String !String ![String] !HTTPRequest !*IWorld -> (!HTTPResponse, !*IWorld)
taskService url format path req iworld
	# html					= format == "html"
	# (mbSession,iworld)	= 'SessionDB'.restoreSession sessionParam iworld
	= case path of
		//List tasks
		[]
			| isError mbSession
				= (serviceResponse html "Task list" listDescription url listParams (jsonSessionErr mbSession), iworld)					
			# (processes,iworld)	= case (fromOk mbSession).user of
				RootUser
					| userParam == ""	= 'ProcessDB'.getProcessesForUser RootUser [Running] [Active] iworld
					| otherwise			= 'ProcessDB'.getProcessesForUser (NamedUser userParam) [Running] [Active] iworld
				user			= 'ProcessDB'.getProcessesForUser (fromOk mbSession).user [Running] [Active] iworld		
			# items				= taskItems processes
			# json				= JSONObject [("success",JSONBool True),("tasks",JSONArray items)]
			= (serviceResponse html "Task list" listDescription url listParams json, iworld)
		//For debugging, list all tasks in the process table
		["debug"]
			| isError mbSession
				= (serviceResponse html "Task debug list" listDebugDescription url debugParams (jsonSessionErr mbSession), iworld)	
			# (processes,iworld)	= 'ProcessDB'.getProcesses [Running,Finished,Excepted,Deleted] [Active,Suspended] iworld
			# json					= JSONObject [("success",JSONBool True),("tasks",toJSON processes)]
			= (serviceResponse html "Task debug list" listDebugDescription url debugParams json, iworld)
		//Start a new task (create a process)
		["create"]
			| isError mbSession
				= (serviceResponse html "Create task" createDescription url createParams (jsonSessionErr mbSession), iworld)	 
			# (mbResult,iworld)	= createWorkflowInstance (toInt workflowParam) (fromOk mbSession).user (if (paramParam == "") Nothing (Just (fromString paramParam))) iworld
			# json = case mbResult of
				Error err
					= JSONObject [("success",JSONBool False),("error",JSONString err)]
				Ok (TaskException _ err,_)
					= JSONObject [("success",JSONBool False),("error",JSONString err)]
				Ok (_,properties)
					= JSONObject [("success",JSONBool True),("taskId",JSONInt (toInt properties.systemProperties.SystemProperties.taskId))]
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
			# (mbResult,iworld)	= evaluateWorkflowInstance (processOf taskId) Nothing Nothing [] iworld
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
			# (mbResult,iworld)	= evaluateWorkflowInstance (processOf taskId) Nothing Nothing [] iworld
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
						= Just (reverse (taskNrFromString target),path,value)
					| otherwise
						= Nothing
				Nothing = Nothing			
			# commitEvent = case fromJSON (fromString commitEventParam) of
				Just (target,action)
					// ignore commit events of outdated clients
					| not outdated || timestampParam == "" 
						= Just (reverse (taskNrFromString target),action)
					| otherwise
						= Nothing
				Nothing	= Nothing
			//We need the current timestamp to include in the response
			# (timestamp,iworld)	= getTimestamp iworld
			//Evaluate the workflow instance
			# tuiTaskNr			= taskNrFromString taskId
			# (mbResult,iworld)	= evaluateWorkflowInstance (processOf taskId) editEvent commitEvent tuiTaskNr iworld
			# (json,iworld) = case mbResult of
				Error err
					= (JSONObject [("succes",JSONBool False),("error",JSONString err)],iworld)
								
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
					= (JSONObject [("succes",JSONBool False),("error",JSONString err)], iworld)
				_
					= (JSONObject [("succes",JSONBool False),("error",JSONString  "Unknown exception")],iworld)
					 
			= (serviceResponse html "Task user interface" tuiDescription url tuiParams json,iworld) 
	
		//Cancel / Abort / Delete the current task
		[taskId,"cancel"]
			| isError mbSession
				= (serviceResponse html "Cancel task" cancelDescription url detailsParams (jsonSessionErr mbSession), iworld)
			| length (taskNrFromString taskId) > 1 // don't delete detached
				= (serviceResponse html "Cancel task" cancelDescription url detailsParams (JSONObject [("succes",JSONBool False),("error",JSONString  "Cannot delete detached process")]), iworld)
			# (_,iworld) = 'ProcessDB'.deleteProcess (processOf taskId) iworld
			= (serviceResponse html "Cancel task" cancelDescription url detailsParams (JSONObject [("success",JSONBool True),("message",JSONString "Task deleted")]), iworld)	
		_
			= (notFoundResponse req, iworld)
where
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
		[]		= 0
		taskNr	= last taskNr
	
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