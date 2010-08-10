implementation module TaskService
import Http, TSt
import HtmlUtil, Text
import JSON
import StdList

import ProcessDB
import TaskPanel

derive bimap (,), Maybe

derive JSONEncode Menu, MenuItem, Hotkey, Key, Action, User, UserDetails, Password
derive JSONDecode ManagerProperties, TaskPriority, User, UserDetails, Password

//Additional derives for debugging
derive JSONEncode Process, TaskParallelType, TaskParallelInfo, TaskInfo
derive JSONEncode GroupActionsBehaviour, GroupedBehaviour
derive JSONEncode HtmlTag, HtmlAttr

//Can't derive TaskTree serialization because the damn thing contains functions
//on unique states @!#$%!!
JSONEncode{|TaskTree|} (TTMainTask a0 a1 a2 a3 a4)
	= [JSONArray [JSONString "TTMainTask":JSONEncode{|*|} a0 ++ JSONEncode{|*|} a1 ++ JSONEncode{|*|} a2 ++ JSONEncode{|*|} a3 ++ JSONEncode{|*|} a4]]
JSONEncode{|TaskTree|} (TTInteractiveTask a0 a1)
	= [JSONArray [JSONString "TTInteractiveTask":JSONEncode{|*|} a0]] //DOES NOT INCLUDE a1
JSONEncode{|TaskTree|} (TTMonitorTask a0 a1)
	= [JSONArray [JSONString "TTMonitorTask":JSONEncode{|*|} a0 ++ JSONEncode{|*|} a1]]
JSONEncode{|TaskTree|} (TTInstructionTask a0 a1 a2)
	= [JSONArray [JSONString "TTInstructionTask":JSONEncode{|*|} a0 ++ JSONEncode{|*|} a1 ++ JSONEncode{|*|} a2]]
JSONEncode{|TaskTree|} (TTRpcTask a0 a1)
	= [JSONArray [JSONString "TTRpcTask":JSONEncode{|*|} a0 ++ JSONEncode{|*|} a1]]
JSONEncode{|TaskTree|} (TTSequenceTask a0 a1)
	= [JSONArray [JSONString "TTSequenceTask":JSONEncode{|*|} a0 ++ JSONEncode{|*|} a1]]
JSONEncode{|TaskTree|} (TTParallelTask a0 a1 a2)
	= [JSONArray [JSONString "TTParallelTask":JSONEncode{|*|} a0 ++ JSONEncode{|*|} a1 ++ JSONEncode{|*|} a2]]
JSONEncode{|TaskTree|} (TTGroupedTask a0 a1 a2 a3)
	= [JSONArray [JSONString "TTGroupedTask":JSONEncode{|*|} a0 ++ JSONEncode{|*|} a1 ++ JSONEncode{|*|} a3]] //DOES NOT INCLUDE a2	
JSONEncode{|TaskTree|} (TTFinishedTask a0 a1)
	= [JSONArray [JSONString "TTFinishedTask":JSONEncode{|*|} a0 ++ JSONEncode{|*|} a1]]

JSONEncode{|Timestamp|}	(Timestamp x)	= JSONEncode{|*|} x
JSONDecode{|Timestamp|} [JSONInt x:c]	= (Just (Timestamp x),c)

taskService :: !String !Bool ![String] !HTTPRequest *TSt -> (!HTTPResponse, !*TSt)
taskService url html path req tst
	# (mbSessionErr,tst)	= initSession sessionParam tst
	# (session,tst)			= getCurrentSession tst
	= case path of
		//List tasks
		[]
			| isJust mbSessionErr
				= (serviceResponse html "tasks" url listParams (jsonSessionErr mbSessionErr), tst)	
						
			# (processes,tst)	= case session.Session.user of
				RootUser
					| userParam == ""	= getProcessesForUser RootUser [Active] tst
					| otherwise			= getProcessesForUser (NamedUser userParam) [Active] tst
				user			= getProcessesForUser session.Session.user [Active] tst		
			# items				= taskItems processes
			# json				= JSONObject [("success",JSONBool True),("tasks",toJSON items)]
			= (serviceResponse html "tasks" url listParams json, tst)
		//For debugging, list all tasks in the process table
		["debug"]
			| isJust mbSessionErr
				= (serviceResponse html "tasks debug" url debugParams (jsonSessionErr mbSessionErr), tst)	
			# (processes,tst)	= getProcesses [Active,Suspended,Finished,Excepted,Deleted] tst
			# json				= JSONObject [("success",JSONBool True),("tasks",toJSON processes)]
			= (serviceResponse html "tasks debug" url debugParams json, tst)
		//Start a new task (create a process)
		["create"]
			| isJust mbSessionErr
				= (serviceResponse html "create task" url createParams (jsonSessionErr mbSessionErr), tst)	
			
			# (mbWorkflow, tst)	= getWorkflowByName workflowParam tst
			# (json,tst) = case mbWorkflow of
				Nothing
					= (JSONObject [("success",JSONBool False),("error",JSONString "No such workflow")], tst)
				Just workflow
					# (taskId,_,_,tst) = createTaskInstance workflow.Workflow.thread True Nothing True True tst
					= (JSONObject [("success",JSONBool True),("taskId",JSONString taskId)], tst)		
			= (serviceResponse html "create task" url createParams json, tst)
		//Show task details of an individual task
		[taskId]
			| isJust mbSessionErr
				= (serviceResponse html "task details" url detailsParams (jsonSessionErr mbSessionErr), tst)	
			# (mbProcess, tst)	= case session.Session.user of
				RootUser
								= getProcess taskId tst
				user			= getProcessForUser user taskId tst
			= case mbProcess of
				Nothing
					= (notFoundResponse req, tst)
				Just proc
					# json = JSONObject [("success",JSONBool True),("task",toJSON (taskItem proc))]
					= (serviceResponse html "task details" url detailsParams json, tst)
		//Dump the raw tasktree datastructure
		[taskId,"debug"]
			| isJust mbSessionErr
				= (serviceResponse html "task debug" url debugParams (jsonSessionErr mbSessionErr), tst)
			# (mbProcess, tst)	= getProcess taskId tst
			= case mbProcess of
				Nothing
					= (notFoundResponse req, tst)
				Just proc
					# (tree,tst) = calculateTaskTree taskId [] tst
					# json		= JSONObject [("success",JSONBool True),("task",toJSON proc),("tree",toJSON tree)]
					= (serviceResponse html "task debug" url debugParams json, tst)
		//Show / Update task user interface definition
		[taskId,"tui"]
			| isJust mbSessionErr
				= (serviceResponse html "task user interface" url tuiParams (jsonSessionErr mbSessionErr), tst)	
			# (mbProcess, tst)	= case session.Session.user of
				RootUser
								= getProcess taskId tst
				user			= getProcessForUser user taskId tst
			= case mbProcess of
				Nothing
					= (notFoundResponse req, tst)
				Just proc
					# task			= taskItem proc
					//Updates are posted as a list of triplets
					# events		= case (fromJSON (fromString eventsParam)) of
						Just events		= events
						Nothing			= []
					//The menusChanged parameter is a global flag that is set when any task in the tree has
					//changed the menu and thus the menu needs to be replaced
					# (tree,tst=:{TSt|menusChanged}) 
									= calculateTaskTree taskId events tst
					= case tree of
						(TTMainTask ti properties menus _ content)
							# tui			= buildTaskPanel content menus menusChanged session.Session.user
							# json			= JSONObject [("success",JSONBool True),("task",toJSON task),("menu",toJSON menus),("tui",toJSON tui)]
							= (serviceResponse html "task user interface" url tuiParams json, {TSt|tst & menusChanged = menusChanged})
						_
							# json			= JSONObject [("success",JSONBool True),("task",toJSON task),("menu",JSONNull),("tui",JSONNull)]
							= (serviceResponse html "task user interface" url tuiParams json, {TSt|tst & menusChanged = menusChanged})
		//Cancel / Abort / Delete the current task
		[taskId,"cancel"]
			| isJust mbSessionErr
				= (serviceResponse html "task user interface" url tuiParams (jsonSessionErr mbSessionErr), tst)
			# (mbProcess, tst) = getProcessForTask taskId tst
			= case mbProcess of
				Nothing
					= (notFoundResponse req, tst)
				Just proc
					// check whether the current user is allowed to cancel the task (user == manager)
					# (mbUProcess, tst) = case session.Session.user of
						RootUser = (Just proc,tst)
						user	 = getProcessForManager user proc.Process.taskId tst
					| not (isJust mbUProcess) 
						= (serviceResponse html "cancel task" url tuiParams (JSONObject [("success",JSONBool False),("error",JSONString "You do not have permission to cancel this task")]), tst)		
					# tst = deleteTaskInstance proc.Process.taskId tst
					= (serviceResponse html "cancel task" url tuiParams (JSONObject [("success",JSONBool True),("message",JSONString "Task deleted")]), tst)	
					
		//Show / update Manager properties
		[taskId,"managerProperties"]
			| isJust mbSessionErr
				= (serviceResponse html "Manager properties" url propParams (jsonSessionErr mbSessionErr), tst)			
			# (mbProcess, tst)	= case session.Session.user of
				RootUser
								= getProcess taskId tst
				user			= getProcessForManager user taskId tst
			= case mbProcess of
				Nothing
					= (notFoundResponse req, tst)
				Just proc
					//Update properties if "update" is set
					# (json, tst) = case updateParam of
						""	= (JSONObject [("success",JSONBool True),("managerProperties",toJSON proc.Process.properties.managerProperties)], tst)
						update
							= case (fromJSON (fromString update)) of
								Just managerProperties
									# (ok,tst) = updateProcessProperties taskId (\p -> {p & managerProperties = managerProperties}) tst
									| ok	= (JSONObject [("success",JSONBool True),("managerProperties",toJSON proc.Process.properties.managerProperties)], tst)
											= (JSONObject [("success",JSONBool False),("error",JSONString "Failed to update properties")],tst)
								_
									= (JSONObject [("success",JSONBool False),("error",JSONString "Failed to parse update")],tst)
					= (serviceResponse html "Manager properties" url propParams json, tst)
		[taskId,"managerProperties",_]
			# param = last path
			| isJust mbSessionErr
				= (serviceResponse html ("Manager property: "+++param) url propParams (jsonSessionErr mbSessionErr), tst)
			# (mbProcess, tst) = case session.Session.user of
				RootUser 	= getProcess taskId tst
				user		= getProcessForManager user taskId tst
			= case mbProcess of
				Nothing = (notFoundResponse req, tst)
				Just proc
					# (json, tst) = case updateParam of
						"" 		= (getManagerProperty param proc.Process.properties.managerProperties,tst)
						update 	= updateManagerProperty param update proc tst
					# updateParam = "" //reset the update paramater
					= (serviceResponse html ("Manager property: "+++param) url propParams json, tst)
		
		//TODO: Worker properties & System properties
				
		//taskId/result -> show result of a finished in serialized form (to be implemented)
		//Show the result of a finished task as interface definition	
		//TODO: Prevent access in case of a faulty user
		[taskId,"result","tui"]
		| isJust mbSessionErr
				= (serviceResponse html "task result user interface" url tuiParams (jsonSessionErr mbSessionErr), tst)
		# (mbProcess, tst) = getProcessForTask taskId tst
		= case mbProcess of
			Nothing 
				= (notFoundResponse req, tst)
			Just proc
				# task			= taskItem proc
				# (tree,tst)	= calculateTaskResult taskId tst
				# tui			= buildResultPanel tree
				# json			= JSONObject [("success",JSONBool True),("task",toJSON task),("tui",toJSON tui)]
				= (serviceResponse html "task result user interface" url tuiParams json, tst)
		_
			= (notFoundResponse req, tst)		
where
	sessionParam	= paramValue "_session" req
	userParam		= paramValue "_user" req
	
	createParams	= [("_session",sessionParam,True),("workflow",workflowParam,True)]
	workflowParam	= paramValue "workflow" req
	
	listParams		= [("_session",sessionParam,True),("_user",userParam,False)]
	
	debugParams		= [("_session",sessionParam,True)]
	
	detailsParams	= [("_session",sessionParam,True)]
	tuiParams		= [("_session",sessionParam,True),("events",eventsParam,False)]
	eventsParam		= paramValue "events" req
	
	propParams		= [("_session",sessionParam,True),("update",updateParam,False)]
	updateParam		= paramValue "update" req
	
	jsonSessionErr (Just error)
					= JSONObject [("success",JSONBool False),("error", JSONString error)]
	
	taskItems processes = map taskItem processes
	taskItem process	= process.Process.properties	
		
	getManagerProperty :: !String !ManagerProperties -> JSONNode
	getManagerProperty param manProps = case param of
		"worker" 	= JSONObject [("success",JSONBool True),(param,toJSON manProps.ManagerProperties.worker)]
		"subject"	= JSONObject [("success",JSONBool True),(param,toJSON manProps.ManagerProperties.subject)]  
		"priority"	= JSONObject [("success",JSONBool True),(param,toJSON manProps.ManagerProperties.priority)]
		"deadline"	= JSONObject [("success",JSONBool True),(param,toJSON manProps.ManagerProperties.deadline)]
		"tags"		= JSONObject [("success",JSONBool True),(param,toJSON manProps.ManagerProperties.tags)]
		_		 	= JSONObject [("success",JSONBool False),("error",JSONString ("Property "+++param+++" does not exist"))]
		
	updateManagerProperty :: !String !String !Process !*TSt -> (JSONNode,*TSt)
	updateManagerProperty param update proc tst
		# manProps 		= proc.Process.properties.managerProperties
		# (ok,newProps) = case param of
			"worker" = case fromJSON(fromString update) of
				Nothing = (False,manProps)
				Just upd = (True,{ManagerProperties | manProps & worker = upd})
			"subject" = case fromJSON(fromString update) of
				Nothing = (False,manProps)
				Just upd = (True,{ManagerProperties | manProps & subject = upd})
			"priority" = case fromJSON(fromString update) of
				Nothing = (False,manProps)
				Just upd = (True,{ManagerProperties | manProps & priority = upd})
			"deadline" = case fromJSON(fromString update) of
				Nothing = (False,manProps)
				Just upd = (True,{ManagerProperties | manProps & deadline = upd})
			"tags" = case fromJSON(fromString update) of
				Nothing = (False,manProps)
				Just upd = (True,{ManagerProperties | manProps & tags = upd})
			_ = (False,manProps)
		= case ok of
			True
				# (ok,tst) = updateProcessProperties proc.Process.taskId (\p -> {p & managerProperties = newProps}) tst
				| ok	= (getManagerProperty param newProps, tst)
						= (JSONObject [("success",JSONBool False),("error",JSONString "Failed to update properties")],tst)
			False 
				= (JSONObject [("success", JSONBool False),("error", JSONString ("Cannot update '"+++param+++"' property"))],tst) 
