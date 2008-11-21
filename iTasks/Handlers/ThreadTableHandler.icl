implementation module ThreadTableHandler //iTasks.Handlers.ThreadTableHandler

import StdEnv
import Http
import InternaliTasksCommon
import TaskTree, TaskTreeFilters
import iDataForms, iDataState

/**
* Handles the ajax requests for a ThreadTable tab panel.
*/
handleThreadTableRequest :: !(Task a) !HTTPRequest *HSt -> (!HTTPResponse, !*HSt) | iData a
handleThreadTableRequest mainTask request hst
	# thisUserId							= 0																// TODO: has to be fetched from the session in the future
	# (toServer, htmlTree, maybeError, maybeTrace, maybeProcessTable, maybeThreadTable, hst)	
											= calculateTaskTree thisUserId True True True mainTask hst 		// calculate the TaskTree given the id of the current user
	# threadTable							= if (isNothing maybeThreadTable) [] (fromJust maybeThreadTable) 
	# content								= toString (DivTag [IdAttr "itasks-threadtable", ClassAttr "trace"] threadTable)											// create tab data record
	= ({http_emptyResponse & rsp_data = content}, hst)														// create the http response
