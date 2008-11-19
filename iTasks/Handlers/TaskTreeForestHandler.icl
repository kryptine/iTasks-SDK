implementation module TaskTreeForestHandler //iTasks.Handlers.TaskTreeForestHandler

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
* Handles the ajax requests for a TaskTreeForest tab panel.
*/
handleTaskTreeForestRequest :: !(Task a) !HTTPRequest *HSt -> (!HTTPResponse, !*HSt) | iData a
handleTaskTreeForestRequest mainTask request hst
	# thisUserId							= 0																// TODO: has to be fetched from the session in the future
	# taskId 								= http_getValue "taskid" request.arg_get "error"				// fetch task id of the tab selecetd
	# (toServer, htmlTree, maybeError, maybeTrace, maybeProcessTable, maybeThreadTable, hst)	
											= calculateTaskTree thisUserId True True True mainTask hst 			// calculate the TaskTree given the id of the current user
	# taskTreeTrace							= showTaskTree  maybeTrace										// TEMP fix to show taskTree
	# content								=
		{TabContent
		|	html 		= toString (DivTag [IdAttr ("itasks-tab-" +++ taskId)] 
							[taskTreeTrace])
		} 																									// create tab data record
	= ({http_emptyResponse & rsp_data = toJSON content}, hst)												// create the http response
	
	
