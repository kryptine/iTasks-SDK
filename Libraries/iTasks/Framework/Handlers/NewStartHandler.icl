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
	
	response :: Int -> String
	response taskid	= "{\"success\" : true, \"taskid\": \""  +++ (toString taskid) +++ "\"}"

	startNewWorkflow :: Workflow *TSt -> (Int, *TSt)
	startNewWorkflow workflow tst
		# (currentUserId, tst)	= getCurrentUser tst
		# (currentUser,tst)		= getUser currentUserId tst
		# (currentTime, tst)	= accHStTSt (accWorldHSt time) tst
		= createProcess (mkStaticProcessEntry workflow currentTime currentUser currentUser Active) tst