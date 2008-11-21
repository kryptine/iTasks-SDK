implementation module TaskTreeForestHandler //iTasks.Handlers.TaskTreeForestHandler

import StdEnv
import Http
import InternaliTasksCommon
import TaskTree, TaskTreeFilters
import JSON
import iDataForms, iDataState

/**
* Handles the ajax requests for a TaskTreeForest tab panel.
*/
handleTaskTreeForestRequest :: !(Task a) !HTTPRequest *HSt -> (!HTTPResponse, !*HSt) | iData a
handleTaskTreeForestRequest mainTask request hst
	# thisUserId							= 0																// TODO: has to be fetched from the session in the future
	# (toServer, htmlTree, maybeError, maybeTrace, maybeProcessTable, maybeThreadTable, hst)	
											= calculateTaskTree thisUserId True True True mainTask hst 		// calculate the TaskTree given the id of the current user
	# taskTreeTrace							= showTaskTree  maybeTrace										// TEMP fix to show taskTree
	# content								= toString (DivTag [IdAttr "itasks-tasktreeforest",ClassAttr "trace"] [taskTreeTrace])
	= ({http_emptyResponse & rsp_data = content}, hst)														// create the http response
	
	
