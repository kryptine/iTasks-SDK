implementation module TaskService
import Http, TSt
import HtmlUtil, Text
import StdList

import StdMisc // abort

import ProcessDB
import TaskPanel

derive bimap (,), Maybe

derive JSONEncode Menu, MenuItem, Action, User, UserDetails, Password
derive JSONDecode ManagerProperties, TaskPriority, User, UserDetails, Password

JSONEncode{|Timestamp|}	(Timestamp x)	= JSONEncode{|*|} x
JSONDecode{|Timestamp|} [JSONInt x:c]	= (Just (Timestamp x),c)

import StdDebug

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
		[taskId,"result","tui"]
			| isJust mbSessionErr
				= (serviceResponse html "task result user interface" url tuiParams (jsonSessionErr mbSessionErr), tst)
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
