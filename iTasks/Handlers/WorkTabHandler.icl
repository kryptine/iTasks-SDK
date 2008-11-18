implementation module WorkTabHandler //iTasks.Handlers.WorkTabHandler

import StdEnv
import Http
import InternaliTasksCommon
import TaskTree, TaskTreeFilters
import JSON
import iDataForms, iDataState

derive JSONEncode TabContent, InputId, UpdateEvent, HtmlState, StorageFormat, Lifespan

:: TabContent 	= { html	:: String
				  , inputs	:: [InputId]
				  , state	:: [HtmlState]					
				  }

/**
* Handles the ajax requests for a work tab panel.
*/
handleWorkTabRequest :: !(Task a) !HTTPRequest *HSt -> (!HTTPResponse, !*HSt) | iData a
handleWorkTabRequest mainTask request hst
	# thisUserId							= 0																// TODO: has to be fetched from the session in the future
	# taskId 								= http_getValue "taskid" request.arg_get "error"				// fetch task id of the tab selecetd
	# (toServer, htmlTree, maybeError, maybeTrace, maybeTable, hst)	
											= calculateTaskTree thisUserId True True mainTask hst 			// calculate the TaskTree given the id of the current user
	# (html,inputs,hst =:{states}) 			= determineTaskForTab thisUserId taskId htmlTree hst 			// filter out the code and inputs to display in this tab
	# (htmlstates,states)					= getHtmlStates states											// Collect states that must be temporarily stored in the browser
	# (instateTrace,states)					= traceInStates states											// TEMP: Always trace initial html states
	# (stateTrace,states)					= traceStates states											// TEMP: Always trace states
	# (updateTrace,states)					= traceUpdates states											// TEMP: Always trace updates
	# taskTreeTrace							= showTaskTree  maybeTrace										// TEMP fix to show taskTree
	# taskTreeTraceOfTask					= showTaskTreeOfTask  taskId maybeTrace							// TEMP fix to show taskTree


	# content								= {TabContent|
		html = toString (DivTag [IdAttr ("itasks-tab-" +++ taskId)] 
					[updateTrace,instateTrace,stateTrace,taskTreeTrace:fromJust maybeTable ++ html]),
		inputs = inputs,
		state = htmlstates} 																						// create tab data record
	= ({http_emptyResponse & rsp_data = toJSON content}, {hst & states = states})							// create the http response
