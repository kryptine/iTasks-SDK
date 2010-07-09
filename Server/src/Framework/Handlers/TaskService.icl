implementation module TaskService
import Http, TSt
import HtmlUtil, Text
import StdList

import ProcessDB
import TaskPanel

derive JSONEncode TaskItem
derive JSONEncode Menu, MenuItem, Action

JSONEncode{|Timestamp|}	(Timestamp x)	= JSONEncode{|*|} x

:: TaskItem =
	{ taskId		:: !TaskId
	, parent		:: !Maybe TaskId
	, properties	:: !TaskProperties
	}

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
		//Start a new task (create a process)
		["create"]
			| isJust mbSessionErr
				= (serviceResponse html "create task" url createParams (jsonSessionErr mbSessionErr), tst)	
			
			# (mbWorkflow, tst)	= getWorkflowByName workflowParam tst
			# (json,tst) = case mbWorkflow of
				Nothing
					= (JSONObject [("success",JSONBool False),("error",JSONString "No such workflow")], tst)
				Just workflow
					# (_,taskId,tst) = createTaskInstance workflow.Workflow.thread True Nothing True True tst
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
					# menu			= proc.Process.menus
					# (tree,tst)	= calculateTaskTree taskId [] tst //TODO Add update events as parameter
					# (tui,tst)		= buildTaskPanel tree Nothing session.Session.user tst //TODO: Clean up this conversion. TSt should be irrelevant 
					# json			= JSONObject [("success",JSONBool True),("task",toJSON task),("menu",toJSON menu),("tui",toJSON tui)]
					= (serviceResponse html "task user interface" url tuiParams json, tst)
		
		//taskId/result -> show result of a finished in serialized form (to be implemented)
		
		//Show the result of a finished task as interface definition	
		[taskId,"result","tui"]
			| isJust mbSessionErr
				= (serviceResponse html "task result user interface" url tuiParams (jsonSessionErr mbSessionErr), tst)
			/*# (mbProcess, tst)	= case session.Session.user of
				RootUser		= getProcess taskId tst
				user			= getProcessForUser user taskId tst*/
			# (mbProcess, tst) = getProcess taskId tst
			= case mbProcess of
				Nothing 
					= (notFoundResponse req, tst)
				Just proc
					# task			= taskItem proc
					# (tree,tst)	= calculateTaskTree taskId [] tst
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
	
	detailsParams	= [("_session",sessionParam,True)]
	tuiParams		= [("_session",sessionParam,True)]
	
	jsonSessionErr (Just error)
					= JSONObject [("success",JSONBool False),("error", JSONString error)]
	
	taskItems processes = map taskItem processes
	taskItem process	= {TaskItem | taskId = process.Process.taskId, parent = process.Process.parent, properties = process.Process.properties} 
	
	