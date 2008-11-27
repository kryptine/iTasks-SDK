implementation module ThreadTableHandler //iTasks.Handlers.ThreadTableHandler

import StdEnv
import Http, Session
import InternaliTasksCommon
import TaskTree, TaskTreeFilters
import iDataForms, iDataState

/**
* Handles the ajax requests for a ThreadTable tab panel.
*/
handleThreadTableRequest :: !(Task a) !HTTPRequest !Session *HSt -> (!HTTPResponse, !*HSt) | iData a
handleThreadTableRequest mainTask request session hst
	# thisUserId							= session.Session.userId
	# (toServer, htmlTree, maybeError, maybeTrace, maybeProcessTable, maybeThreadTable, hst)	
											= calculateTaskTree thisUserId True True True mainTask hst 		// calculate the TaskTree given the id of the current user
	# threadTable							= if (isNothing maybeThreadTable) [] (fromJust maybeThreadTable) 
	# content								= toString (DivTag [IdAttr "itasks-threadtable", ClassAttr "trace"] threadTable)											// create tab data record
	= ({http_emptyResponse & rsp_data = content}, hst)														// create the http response
