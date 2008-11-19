implementation module ProcessTableHandler //iTasks.Handlers.ProcessTableHandler

import StdEnv
import Http
import InternaliTasksCommon
import TaskTree, TaskTreeFilters
import JSON
import iDataForms, iDataState

derive JSONEncode TabContent, InputId, UpdateEvent, HtmlState, StorageFormat, Lifespan

:: TabContent 	= { html		:: String			//The HTML content of the tab
				  }

/**
* Handles the ajax requests for a ProcessTable tab panel.
*/
handleProcessTableRequest :: !(Task a) !HTTPRequest *HSt -> (!HTTPResponse, !*HSt) | iData a
handleProcessTableRequest mainTask request hst
	# thisUserId							= 0																// TODO: has to be fetched from the session in the future
	# taskId 								= http_getValue "taskid" request.arg_get "error"				// fetch task id of the tab selecetd
	# (toServer, htmlTree, maybeError, maybeTrace, maybeProcessTable, maybeThreadTable, hst)	
											= calculateTaskTree thisUserId True True True mainTask hst 		// calculate the TaskTree given the id of the current user
	# processTable							= if (isNothing maybeProcessTable) [] (fromJust maybeProcessTable) 
	# content								=
		{TabContent
		|	html 		= toString (DivTag [IdAttr ("itasks-tab-" +++ taskId)] processTable)
		} 																									// create tab data record
	= ({http_emptyResponse & rsp_data = toJSON content}, hst)												// create the http response
	
	
