implementation module WorkTabHandler //iTasks.Handlers.WorkTabHandler

import StdEnv
import Http
import InternaliTasksCommon
import TaskTree, TaskTreeFilters
import JSON
import iDataForms, iDataState

derive JSONEncode TabContent, InputId, UpdateEvent, HtmlState, StorageFormat, Lifespan

:: TabContent 	= { done		:: Bool				//Is the requested work finished
				  , html		:: String			//The HTML content of the tab
				  , inputs		:: [InputId]		//The interactive inputs in the tab
				  , state		:: [HtmlState]		//The task state that must be stored in the tab
				  , activeTasks	:: Maybe [String]	//Optional list of task id's to sync the open tabs with the known states on the server
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
	# (taskDone,html,inputs,hst =:{states}) = determineTaskForTab thisUserId taskId htmlTree hst 			// filter out the code and inputs to display in this tab
	# (htmlstates,states)					= getHtmlStates states											// Collect states that must be temporarily stored in the browser
	# (instateTrace,states)					= traceInStates states											// TEMP: Always trace initial html states
	# (stateTrace,states)					= traceStates states											// TEMP: Always trace states
	# (updateTrace,states)					= traceUpdates states											// TEMP: Always trace updates
	# taskTreeTrace							= showTaskTree  maybeTrace										// TEMP fix to show taskTree
	# taskTreeTraceOfTask					= showTaskTreeOfTask  taskId maybeTrace							// TEMP fix to show taskTree

	# activeTasks							= if taskDone
												(Just [	mytaskdescr.taskNrId													
													  \\ mytaskdescr <- collectTaskList (\taskdescr -> taskdescr.taskWorkerId == thisUserId) htmlTree
													  ])
											    Nothing
	# content								=
		{TabContent
		|	done		= taskDone
		,	html 		= toString (DivTag [IdAttr ("itasks-tab-" +++ taskId)] 
							[updateTrace,instateTrace,stateTrace,taskTreeTrace:fromJust maybeTable ++ html])
		,	inputs		= inputs
		,	state		= htmlstates
		,	activeTasks	= Nothing
		} 																									// create tab data record
	= ({http_emptyResponse & rsp_data = toJSON content}, {hst & states = states})							// create the http response
	
	
