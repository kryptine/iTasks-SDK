implementation module WorkTabHandler //iTasks.Handlers.WorkTabHandler

import StdEnv
import Http
import InternaliTasksCommon
import TaskTree, TaskTreeFilters
import JSON
import iDataForms, iDataState

derive JSONEncode TabContent, InputId, UpdateEvent, HtmlState, StorageFormat, Lifespan

:: TabContent 	= { done			:: Bool				//Is the requested work finished
				  , error			:: Maybe String		//Optional error if something went wrong on the server
				  , html			:: String			//The HTML content of the tab
				  , inputs			:: [InputId]		//The interactive inputs in the tab
				  , state			:: [HtmlState]		//The task state that must be stored in the tab
				  , activeTasks		:: Maybe [String]	//Optional list of task id's to sync the open tabs with the known states on the server
				  , stateTrace		:: Maybe String		//Optional state trace info
				  , updateTrace		:: Maybe String		//Optional update trace info
				  , subtreeTrace	:: Maybe String		//Optional trace of the sub tasktree of this task
				  }

/**
* Handles the ajax requests for a work tab panel.
*/
handleWorkTabRequest :: !(Task a) !HTTPRequest *HSt -> (!HTTPResponse, !*HSt) | iData a
handleWorkTabRequest mainTask request hst
	# thisUserId									= 0																// TODO: has to be fetched from the session in the future
	# taskId 										= http_getValue "taskid" request.arg_get "error"				// fetch task id of the tab selecetd
	# (toServer, htmlTree, maybeError, maybeTrace, maybeProcessTable, maybeThreadTable, hst)	
													= calculateTaskTree thisUserId True True True mainTask hst 		// calculate the TaskTree given the id of the current user
	# (taskDone,html,inputs,hst =:{states,world})	= determineTaskForTab thisUserId taskId htmlTree hst 			// filter out the code and inputs to display in this tab
	# (htmlstates,states)							= getHtmlStates states											// Collect states that must be temporarily stored in the browser
	# (states,world)								= storeServerStates states world								// Write states that are stored on the server

	//Tracing
	# (stateTrace,states)							= mbStateTrace request states
	# (updateTrace,states)							= mbUpdateTrace request states
	# subTreeTrace									= mbSubTreeTrace request taskId maybeTrace

	# activeTasks									= if taskDone
														(Just [	mytaskdescr.taskNrId													
														  \\ mytaskdescr <- collectTaskList (\taskdescr -> taskdescr.taskWorkerId == thisUserId) htmlTree
													 	 ])
											    		Nothing
	# content										=
		{TabContent
		|	done			= taskDone
		,	error			= maybeError
		,	html 			= toString (DivTag [IdAttr ("itasks-tab-" +++ taskId)] html)
		,	inputs			= inputs
		,	state			= htmlstates
		,	activeTasks		= activeTasks
		,	stateTrace		= stateTrace
		,	updateTrace		= updateTrace
		,	subtreeTrace	= subTreeTrace
		} 																									// create tab data record
	= ({http_emptyResponse & rsp_data = toJSON content}, {hst & states = states, world = world})			// create the http response
	
where
	mbStateTrace req states
		| http_getValue "traceStates" req.arg_get "" == "1"
			# (trace1,states)	= traceInStates states
			# (trace2,states)	= traceStates states
			= (Just (toString (DivTag [] [trace1,trace2])), states)
		| otherwise
			= (Nothing, states)
	mbUpdateTrace req states
		| http_getValue "traceUpdates" req.arg_get "" == "1"
			# (trace,states)	= traceUpdates states
			= (Just (toString trace), states)
		| otherwise
			= (Nothing, states)	
	mbSubTreeTrace req taskId maybeTrace
		| http_getValue "traceSubTrees" req.arg_get "" == "1"
			= Just (toString (showTaskTreeOfTask taskId maybeTrace))
		| otherwise
			= Nothing
	
