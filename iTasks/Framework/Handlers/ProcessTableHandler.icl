implementation module ProcessTableHandler //iTasks.Handlers.ProcessTableHandler

import StdEnv
import Http, Session
import InternaliTasksCommon
import TaskTree, TaskTreeFilters
import iDataForms, iDataState

/**
* Handles the ajax requests for a ProcessTable tab panel.
*/
handleProcessTableRequest :: !(Task a) !HTTPRequest !Session *HSt -> (!HTTPResponse, !*HSt) | iData a
handleProcessTableRequest mainTask request session hst
	# thisUserId			= session.Session.userId
	# (toServer, htmlTree, maybeError, maybeProcessTable, maybeThreadTable, hst)	
							= calculateTaskTree thisUserId True True True mainTask hst 				// calculate the TaskTree given the id of the current user
	# processTable			= if (isNothing maybeProcessTable) [] (fromJust maybeProcessTable) 
	# content				= toString (DivTag [IdAttr "itasks-processtable", ClassAttr "trace"] processTable)
	= ({http_emptyResponse & rsp_data = content}, hst)												// create the http response
	
	
