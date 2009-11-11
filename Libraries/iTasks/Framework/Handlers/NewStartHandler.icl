implementation module NewStartHandler

import StdEnv
import Http, TSt, ProcessDB, UserDB, Time

handleNewStartRequest :: !HTTPRequest !*TSt -> (!HTTPResponse, !*TSt)
handleNewStartRequest request tst	
	# (mbWorkflow, tst)		= getWorkflowByName workflowId tst
	= case mbWorkflow of
		Nothing
			= ({http_emptyResponse & rsp_data = "{\"success\" : false }" }, tst)
		Just workflow
			# (taskid, tst)	= startWorkflow workflow tst
			= ({http_emptyResponse & rsp_data = response taskid}, tst)			

where
	workflowId :: String
	workflowId = http_getValue "workflow" request.arg_post ""
	
	response :: String -> String
	response taskid	= "{\"success\" : true, \"taskid\": \""  +++ taskid +++ "\"}"
	
	startWorkflow :: !Workflow !*TSt -> (!ProcessId,!*TSt)
	startWorkflow workflow tst
		# (uid,tst) 	= getCurrentUser tst
		# (user,tst)	= getUser uid tst
		# properties	= { worker = (user.User.userId, user.User.displayName)
						  , subject = workflow.Workflow.label
						  , priority	= NormalPriority
						  , deadline	= Nothing
						  }
		= createTaskInstance workflow.Workflow.mainTask properties True tst