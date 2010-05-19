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
	startWorkflow workflow tst=:{staticInfo}
		# user			= staticInfo.currentSession.user
		# properties	= { worker			= toUserName user
						  , subject 		= workflow.Workflow.label
						  , priority		= NormalPriority
						  , deadline		= Nothing
						  }
		# (_,pid,tst) = createTaskInstance workflow.Workflow.mainTask properties True Nothing True tst
		= (pid,tst)