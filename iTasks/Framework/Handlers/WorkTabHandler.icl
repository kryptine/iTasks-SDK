implementation module WorkTabHandler //iTasks.Handlers.WorkTabHandler

import StdEnv
import Http, Session
import InternaliTasksCommon
import TaskTree, TaskTreeFilters
import JSON
import iDataForms, iDataState

derive JSONEncode TabContent, TaskStatus, InputId, UpdateEvent, HtmlState, StorageFormat, Lifespan

:: TabContent 	= { status			:: TaskStatus		//Is the requested work active, finished, or deleted 
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
handleWorkTabRequest :: !(Task a) !HTTPRequest !Session *HSt -> (!HTTPResponse, !*HSt) | iData a
handleWorkTabRequest mainTask request session hst
	# thisUserId									= session.Session.userId										// fetch user id from the session
	# taskId 										= http_getValue "taskid" request.arg_get "error"				// fetch task id of the tab selecetd
	# (toServer, htmlTree, maybeError, maybeProcessTable, maybeThreadTable, hst)	
													= calculateTaskTree thisUserId True True True mainTask hst 		// calculate the TaskTree given the id of the current user
	# (taskStatus,html,inputs)						= determineTaskForTab thisUserId taskId htmlTree				// filter out the code and inputs to display in this tab
	# (htmlstates,hst)								= getPageStates hst												// Collect states that must be temporarily stored in the browser
	# hst =: {states}								= storeStates hst												// Write states that are stored on the server

	//Tracing
	# (stateTrace,states)							= mbStateTrace request states
	# (updateTrace,states)							= mbUpdateTrace request states
	# subTreeTrace									= mbSubTreeTrace request thisUserId taskId htmlTree

	# activeTasks									= if (taskStatus == TaskFinished || taskStatus == TaskDeleted) 
														(Just [	mytaskdescr.taskNrId													
														  \\ mytaskdescr <- determineTaskList thisUserId htmlTree
													 	 ])
											    		Nothing
	# tempMessage									= case taskStatus of
														TaskFinished -> "TaskFinished"
														TaskDeleted -> "TaskDeleted"
														TaskActivated -> "TaskActivated"

	# content										=
		{TabContent
		|	status			= taskStatus 
		,	error			= maybeError
		,	html 			= toString (DivTag [IdAttr ("itasks-tab-" +++ taskId)] html)
		,	inputs			= inputs
		,	state			= htmlstates
		,	activeTasks		= activeTasks
		,	stateTrace		= stateTrace
		,	updateTrace		= updateTrace
		,	subtreeTrace	= subTreeTrace
		}																						// create tab data record
	= ({http_emptyResponse & rsp_data = toJSON content}, {hst & states = states})				// create the http response
	
where
	mbStateTrace req states
		| http_getValue "traceStates" req.arg_post "" == "1"
			# (trace1,states)	= traceInStates states
			# (trace2,states)	= traceStates states
			= (Just (toString (DivTag [] [trace1,trace2])), states)
		| otherwise
			= (Nothing, states)
	mbUpdateTrace req states
		| http_getValue "traceUpdates" req.arg_post "" == "1"
			# (trace,states)	= traceUpdates states
			= (Just (toString trace), states)
		| otherwise
			= (Nothing, states)	
	mbSubTreeTrace req thisUserId taskId htmlTree
		| http_getValue "traceSubTrees" req.arg_post "" == "1"
			= Just (toString (filterTaskTreeOfTask thisUserId taskId htmlTree))
		| otherwise
			= Nothing
	
