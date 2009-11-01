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
			# (taskid, tst)	= startNewWorkflow workflow tst
			= ({http_emptyResponse & rsp_data = response taskid}, tst)			

where
	workflowId :: String
	workflowId = http_getValue "workflow" request.arg_post ""
	
	response :: String -> String
	response taskid	= "{\"success\" : true, \"taskid\": \""  +++ taskid +++ "\"}"

	startNewWorkflow :: Workflow *TSt -> (ProcessId, *TSt)
	startNewWorkflow workflow tst
		# (currentUserId, tst)	= getCurrentUser tst
		# (currentUser,tst)		= getUser currentUserId tst
		# (currentTime, tst)	= accWorldTSt time tst
		# (taskId, tst) 		= createProcess (mkProcessEntry workflow.Workflow.label currentTime 
									(currentUser.User.userId,currentUser.User.displayName) 
									(currentUser.User.userId,currentUser.User.displayName) Active "") tst
		# taskNr				= taskNrFromString taskId
		# tst					= storeTaskFunctionDynamic taskNr (createDynamicTask workflow.Workflow.mainTask) tst
		= (taskId, tst)