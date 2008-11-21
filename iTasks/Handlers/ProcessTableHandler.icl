implementation module ProcessTableHandler //iTasks.Handlers.ProcessTableHandler

import StdEnv
import Http
import InternaliTasksCommon
import TaskTree, TaskTreeFilters
import iDataForms, iDataState

/**
* Handles the ajax requests for a ProcessTable tab panel.
*/
handleProcessTableRequest :: !(Task a) !HTTPRequest *HSt -> (!HTTPResponse, !*HSt) | iData a
handleProcessTableRequest mainTask request hst
	# thisUserId							= 0																// TODO: has to be fetched from the session in the future
	# (toServer, htmlTree, maybeError, maybeTrace, maybeProcessTable, maybeThreadTable, hst)	
											= calculateTaskTree thisUserId True True True mainTask hst 		// calculate the TaskTree given the id of the current user
	# processTable							= if (isNothing maybeProcessTable) [] (fromJust maybeProcessTable) 
	# content								= toString (DivTag [IdAttr "itasks-processtable", ClassAttr "trace"] processTable)
	= ({http_emptyResponse & rsp_data = content}, hst)														// create the http response
	
	
