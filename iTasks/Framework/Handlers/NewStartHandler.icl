implementation module NewStartHandler

import StdEnv
import Http, Session
import JSON
import InternaliTasksCommon


:: NewWorkItem	= 	{ icon		:: String 	// An icon name. The actual icon image is defined in the css. 
					, label		:: String 	// A label of the workflow that is started
					}

derive JSONEncode NewWorkItem

handleNewStartRequest :: !(LabeledTask a) !Int !HTTPRequest !Session *HSt -> (!HTTPResponse, !*HSt) | iData a
handleNewStartRequest labeledTask mainuser request session hst	
	# (taskid,hst) = startNewProcess labeledTask hst
	= ({http_emptyResponse & rsp_data = response taskid}, hst)
where
	workflow = http_getValue "workflow" request.arg_get ""
	response taskid = "{\"success\" : true, \"taskid\": \""  +++ (toString taskid) /* workflow */ +++ "\"}"
	
	thisUser		= session.Session.userId							// fetch user id from the session
	
	startNewProcess labeledTask hst 
	# tst 				= mkTst mainuser LSTxtFile LSTxtFile hst			// create initial tst	
	# (processId, tst) 	= latestProcessId tst
	# (wid,tst=:{hst}) 	= appTaskTSt (spawnWorkflow thisUser True labeledTask) {tst & tasknr = [processId]}
	= (getProcessId wid, hst)
	
	
import iTasksProcessHandling, Combinators, iTasksEditors