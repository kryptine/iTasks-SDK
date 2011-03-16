implementation module TaskService

import StdList, Util, HtmlUtil, JSON, TaskTree, ProcessDB, TaskPanel

//Additional derives for debugging
derive JSONEncode TaskTree, TaskInfo, Hotkey, Key, InteractiveTask, Menu, TTContainerType, TaskTreeContainer

JSONEncode{|MenuItem|} v = case v of
	MenuItem action mbHotkey	= [JSONArray [JSONString "MenuItem" : JSONEncode{|*|} (menuAction action) ++ JSONEncode{|*|} mbHotkey]]
	SubMenu label items			= [JSONArray [JSONString "SubMenu" : JSONEncode{|*|} label ++ JSONEncode{|*|} items]]
	MenuSeparator				= [JSONString "MenuSeparator"]
	
JSONEncode{|HtmlTag|} htm = [JSONString (toString htm)]

taskService :: !String !Bool ![String] !HTTPRequest *TSt -> (!HTTPResponse, !*TSt)
taskService url html path req tst
	# (mbSessionErr,tst)	= initSession sessionParam tst
	# (session,tst)			= getCurrentSession tst
	= case path of
		//List tasks
		[]
			| isJust mbSessionErr
				= (serviceResponse html "Task list" listDescription url listParams (jsonSessionErr mbSessionErr), tst)	
						
			# (processes,tst)	= case session.Session.user of
				RootUser
					| userParam == ""	= getProcessesForUser RootUser [Active] tst
					| otherwise			= getProcessesForUser (NamedUser userParam) [Active] tst
				user			= getProcessesForUser session.Session.user [Active] tst		
			# items				= taskItems processes
			# json				= JSONObject [("success",JSONBool True),("tasks",toJSON items)]
			= (serviceResponse html "Task list" listDescription url listParams json, tst)
		//For debugging, list all tasks in the process table
		["debug"]
			| isJust mbSessionErr
				= (serviceResponse html "Task debug list" listDebugDescription url debugParams (jsonSessionErr mbSessionErr), tst)	
			# (processes,tst)	= getProcesses [Active,Suspended,Finished,Excepted,Deleted] tst
			# json				= JSONObject [("success",JSONBool True),("tasks",toJSON processes)]
			= (serviceResponse html "Task debug list" listDebugDescription url debugParams json, tst)
		//Start a new task (create a process)
		["create"]
			| isJust mbSessionErr
				= (serviceResponse html "Create task" createDescription url createParams (jsonSessionErr mbSessionErr), tst)	
			
			# (mbWorkflow, tst)	= getWorkflowByName workflowParam tst
			# (json,tst) = case mbWorkflow of
				Nothing
					= (JSONObject [("success",JSONBool False),("error",JSONString "No such workflow")], tst)
				Just workflow
					# mbThread = case paramParam of
						""		= Just workflow.Workflow.thread
						param	= toNonParamThreadValue param workflow.Workflow.thread
					= case mbThread of
						Nothing
							= (JSONObject [("success",JSONBool False),("error",JSONString "Invalid parameter")], tst)
						Just thread
							# (taskId,_,_,tst) = createTaskInstance thread True True True workflow.Workflow.containerType tst
							= (JSONObject [("success",JSONBool True),("taskId",JSONString taskId)], tst)		
			= (serviceResponse html "Create task" createDescription url createParams json, tst)
		//Show task details of an individual task
		[taskId]
			| isJust mbSessionErr
				= (serviceResponse html "Task details" detailsDescription url detailsParams (jsonSessionErr mbSessionErr), tst)	
			# (mbProcess, tst)	= case session.Session.user of
				RootUser
								= getProcess taskId tst
				user			= getProcessForUser user taskId tst
			= case mbProcess of
				Nothing
					= (notFoundResponse req, tst)
				Just proc
					//Events are posted as a list of triplets
					# events		= case (fromJSON (fromString eventsParam)) of
						Just events		= events
						Nothing			= []
					# (cont,tst)		= calculateTaskTreeContainer taskId events tst
					# (jsonTree,tst)	= accIWorldTSt (toJSONTreeContainer cont) tst
					# task = JSONObject (taskProperties proc ++ [("parts", JSONArray (taskParts jsonTree))]) 
					# json = JSONObject [("success",JSONBool True),("task",task)]
					= (serviceResponse html "Task details" detailsDescription url detailsParams json, tst)
		//Dump the raw tasktree datastructure
		[taskId,"debug"]
			| isJust mbSessionErr
				= (serviceResponse html "Task debug" taskDebugDescription url debugParams (jsonSessionErr mbSessionErr), tst)
			# (mbProcess, tst)	= getProcess taskId tst
			= case mbProcess of
				Nothing
					= (notFoundResponse req, tst)
				Just proc
					# (cont,tst) = calculateTaskTreeContainer taskId [] tst
					# (treeJson,tst) = case typeParam of
						"ui"	= appFst toJSON (accIWorldTSt (toUITreeContainer cont) tst)
						"json"	= appFst toJSON (accIWorldTSt (toJSONTreeContainer cont) tst)
						_		= (toJSON (toSpineTreeContainer cont),tst)
					# json		= JSONObject [("success",JSONBool True),("task",toJSON proc),("tree",treeJson)]
					= (serviceResponse html "Task debug" taskDebugDescription url debugParams json, tst)
		//Show / Update task user interface definition
		[taskId,"tui"]
			| isJust mbSessionErr
				= (serviceResponse html "Task user interface" tuiDescription url tuiParams (jsonSessionErr mbSessionErr), tst)	
			# (mbProcess, tst)	= case session.Session.user of
				RootUser		= getProcess taskId tst
				user			= getProcessForUser user taskId tst
			= case mbProcess of
				Nothing
					= (notFoundResponse req, tst)
				Just proc
					# task			= taskItem proc
					//Events are posted as a list of triplets
					# events		= case (fromJSON (fromString eventsParam)) of
						Just events		= events
						Nothing			= []
					# (cont,tst)		= calculateTaskTreeContainer taskId events tst
					# (timestamp,tst)	= getTimestamp tst
					# (uiContent,tst)	= accIWorldTSt (toUITreeContainer cont) tst
					# tui				= buildTaskPanel uiContent
					# json				= JSONObject [("success",JSONBool True),("task",toJSON task),("timestamp",toJSON timestamp),("tui",toJSON tui)]
					= (serviceResponse html "Task user interface" tuiDescription url tuiParams json,tst)
		//Cancel / Abort / Delete the current task
		[taskId,"cancel"]
			| isJust mbSessionErr
				= (serviceResponse html "Cancel task" cancelDescription url detailsParams (jsonSessionErr mbSessionErr), tst)
			# (mbProcess, tst) = getProcess taskId tst
			= case mbProcess of
				Nothing
					= (notFoundResponse req, tst)
				Just proc
					// check whether the current user is allowed to cancel the task (user == manager)
					# (mbUProcess, tst) = case session.Session.user of
						RootUser = (Just proc,tst)
						user	 = getProcessForManager user proc.Process.taskId tst
					| not (isJust mbUProcess) 
						= (serviceResponse html "Cancel task" cancelDescription url detailsParams (JSONObject [("success",JSONBool False),("error",JSONString "You do not have permission to cancel this task")]), tst)		
					# tst = deleteTaskInstance proc.Process.taskId tst
					= (serviceResponse html "Cancel task" cancelDescription url detailsParams (JSONObject [("success",JSONBool True),("message",JSONString "Task deleted")]), tst)	
					
		//Show / update Manager properties
		[taskId,"managerProperties"]
			| isJust mbSessionErr
				= (serviceResponse html "Manager properties" manPropsDescription url propParams (jsonSessionErr mbSessionErr), tst)			
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
					= (serviceResponse html "Manager properties" manPropsDescription url propParams json, tst)
		[taskId,"managerProperties",_]
			# param = last path
			| isJust mbSessionErr
				= (serviceResponse html ("Manager property: "+++param) manPropDescription url propParams (jsonSessionErr mbSessionErr), tst)
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
					= (serviceResponse html ("Manager property: "+++param) manPropDescription url propParams json, tst)
		
		//TODO: Worker properties & System properties
				
		//taskId/result -> show result of a finished in serialized form (to be implemented)
		//Show the result of a finished task as interface definition	
		//TODO: Prevent access in case of a faulty user
		[taskId,"result","tui"]
		| isJust mbSessionErr
				= (serviceResponse html "Task result user interface" tuiResDescription url detailsParams (jsonSessionErr mbSessionErr), tst)
		# (mbProcess, tst) = getProcess taskId tst
		= case mbProcess of
			Nothing 
				= (notFoundResponse req, tst)
			Just proc
				# task			= taskItem proc
				# (cont,tst)	= calculateTaskTreeContainer taskId [] tst
				# (uiCont,tst)	= accIWorldTSt (toUITreeContainer cont) tst
				# tui			= buildResultPanel uiCont
				# json			= JSONObject [("success",JSONBool True),("task",toJSON task),("tui",toJSON tui)]
				= (serviceResponse html "Task result user interface" tuiResDescription url detailsParams json, tst)
		[taskId,"refresh"]
			# (mbProcess, tst)	= getProcess taskId tst
			= case mbProcess of
				Nothing
					= (notFoundResponse req, tst)
				Just proc
					# (_,tst) = calculateTaskTreeContainer taskId [] tst
					# json = JSONObject [("success",JSONBool True)]
					= (serviceResponse html "Task details" refreshDescription url [] json, tst)
		_
			= (notFoundResponse req, tst)
where
	sessionParam	= paramValue "session" req
	userParam		= paramValue "user" req
	
	createParams	= [("session",sessionParam,True),("workflow",workflowParam,True),("parameter",paramParam,False)]
	workflowParam	= paramValue "workflow" req
	paramParam		= paramValue "parameter" req
	
	listParams		= [("session",sessionParam,True),("user",userParam,False)]
	
	debugParams		= [("session",sessionParam,True),("type",typeParam,False)]
	typeParam		= paramValue "type" req
	
	detailsParams	= [("session",sessionParam,True),("events",eventsParam,False)]
	eventsParam		= paramValue "events" req
	
	tuiParams		= [("session",sessionParam,True),("events",eventsParam,False),("since",sinceParam,False)]
	sinceParam		= paramValue "since" req
	
	propParams		= [("session",sessionParam,True),("update",updateParam,False)]
	updateParam		= paramValue "update" req
	
	jsonSessionErr (Just error)
					= JSONObject [("success",JSONBool False),("error", JSONString error)]
	
	taskItems processes = map taskItem processes
	taskItem process	= process.Process.properties	
		
	getManagerProperty :: !String !ManagerProperties -> JSONNode
	getManagerProperty param {worker,priority,deadline} = case param of
		"worker" 	= JSONObject [("success",JSONBool True),(param,toJSON worker)] 
		"priority"	= JSONObject [("success",JSONBool True),(param,toJSON priority)]
		"deadline"	= JSONObject [("success",JSONBool True),(param,toJSON deadline)]
		_		 	= JSONObject [("success",JSONBool False),("error",JSONString ("Property "+++param+++" does not exist"))]
		
	updateManagerProperty :: !String !String !Process !*TSt -> (JSONNode,*TSt)
	updateManagerProperty param update proc tst
		# manProps 		= proc.Process.properties.managerProperties
		# (ok,newProps) = case param of
			"worker" = case fromJSON(fromString update) of
				Nothing = (False,manProps)
				Just upd = (True,{manProps & worker = upd})
			"priority" = case fromJSON(fromString update) of
				Nothing = (False,manProps)
				Just upd = (True,{manProps & priority = upd})
			"deadline" = case fromJSON(fromString update) of
				Nothing = (False,manProps)
				Just upd = (True,{manProps & deadline = upd})
			_ = (False,manProps)
		= case ok of
			True
				# (ok,tst) = updateProcessProperties proc.Process.taskId (\p -> {p & managerProperties = newProps}) tst
				| ok	= (getManagerProperty param newProps, tst)
						= (JSONObject [("success",JSONBool False),("error",JSONString "Failed to update properties")],tst)
			False 
				= (JSONObject [("success", JSONBool False),("error", JSONString ("Cannot update '"+++param+++"' property"))],tst) 

	taskProperties :: Process -> [(String,JSONNode)]
	taskProperties proc = case (toJSON proc.Process.properties) of (JSONObject fields) = fields

	taskParts :: !JSONTreeContainer -> [JSONNode]
	taskParts (TTContainer _ tree) = taskParts` tree
	
	taskParts` (TTParallelTask _ trees) = flatten (map taskParts trees)	
	taskParts` (TTInteractiveTask ti _ json)
		= [JSONObject [("taskId",JSONString ti.TaskInfo.taskId),("type",JSONString "interactive"),("value",json)]]
	taskParts` _ = []
	
	getTimestamp :: !*TSt -> (!Timestamp,!*TSt)
	getTimestamp tst=:{TSt|iworld=iworld=:{IWorld|timestamp}} = (timestamp,tst)

listDescription			:== "This service lists all tasks for the user of the provided session."
listDebugDescription	:== "This service dumps all information currently in the process database of running instances."
detailsDescription		:== "This service provides all meta-properties of a running task instance."
createDescription		:== "This service let's you create new instances of a workflow.<br />"
						+++ "The 'workflow' parameter is the path of a workflow (separated by slashes) as listed by the workflow directory service. "
						+++ "E.g. Foo/Bar/Baz"
taskDebugDescription	:== "This service dumps all information about a running task instance. Both its meta-properties and its task tree."
manPropsDescription		:== "This service displays the properties of a task instance that can be modified by a manager." 
manPropDescription		:== "This service displays a single manager property."
cancelDescription		:== "This service let's you cancel (delete) a running task instance."
tuiDescription 			:== "This yields an abstract user interface description for the current task."
tuiResDescription		:== "This yields an abstract user interface description that displays the current value of the task."
refreshDescription		:== "This service recalculates the task tree of a running task instance."
