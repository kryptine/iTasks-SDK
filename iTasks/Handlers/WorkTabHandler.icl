implementation module WorkTabHandler //iTasks.Handlers.WorkTabHandler

import StdEnv
import Http
import InternaliTasksCommon
import TaskTree, TaskTreeFilters
import JSON

derive JSONEncode TabContent, InputId, UpdateEvent

:: TabContent 	= { html	:: String
				  , inputs	:: [InputId]					
				  }

/**
* Handles the ajax requests for a work tab panel.
*/
handleWorkTabRequest :: !(Task a) !HTTPRequest *HSt -> (!HTTPResponse, !*HSt) | iData a
handleWorkTabRequest mainTask request hst
	# thisUserId							= 0																// has to be fetched from the request in the future
	# taskId 								= http_getValue "taskid" request.arg_get "error"				// fetch task id of the tab selecetd
	# (toServer, htmlTree, maybeError, maybeTrace, maybeTable, hst)	
											= calculateTaskTree thisUserId True True mainTask hst 				// calculate the TaskTree given the id of the current user
	# (html,inputs,hst =:{states}) 			= determineTaskForTab thisUserId taskId htmlTree hst 			// filter out the code and inputs to display in this tab
	# (stateTrace,states)					= traceStates states											// TEMP: Always trace states
	# (updateTrace,states)					= traceUpdates states											// TEMP: Always trace updates
	# taskTreeTrace							= showTaskTree  maybeTrace										// TEMP fix to show taskTree
	# taskTreeTraceOfTask					= showTaskTreeOfTask  taskId maybeTrace							// TEMP fix to show taskTree
	# content								= {TabContent|
		html = toString (DivTag [IdAttr ("itasks-tab-" +++ taskId)] 
					[updateTrace,stateTrace,taskTreeTrace:fromJust maybeTable ++ html]), inputs = inputs} // create tab data record
	= ({http_emptyResponse & rsp_data = toJSON content}, {hst & states = states})							// create the http response
