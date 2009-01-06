implementation module TaskTreeForestHandler //iTasks.Handlers.TaskTreeForestHandler

import StdEnv
import Http, Session
import InternaliTasksCommon
import TaskTree, TaskTreeFilters
import JSON
import iDataForms, iDataState

/**
* Handles the ajax requests for a TaskTreeForest tab panel.
*/
handleTaskTreeForestRequest :: !(LabeledTask a) !Int !HTTPRequest !Session *HSt -> (!HTTPResponse, !*HSt) | iData a
handleTaskTreeForestRequest mainTask mainUser request session hst
	# thisUserId				= session.Session.userId
	# (toServer, htmlTree, maybeError, maybeProcessTable, maybeThreadTable, hst)	
								= calculateTaskTree thisUserId True True True mainTask mainUser hst 					// calculate the TaskTree given the id of the current user
	# taskTreeTrace				= getFullTraceFromTaskTree  htmlTree													// calculate Task Tree
	# content					= toString (DivTag [IdAttr "itasks-tasktreeforest",ClassAttr "trace"] [taskTreeTrace])
	= ({http_emptyResponse & rsp_data = content}, hst)														// create the http response
	
	
