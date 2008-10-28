implementation module WorkTabHandler //iTasks.Handlers.WorkTabHandler

import StdEnv
import Http
import InternaliTasksCommon
import TaskTree, TaskTreeFilters
import StdStrictLists

/**
* Handles the ajax requests for a work tab panel.
*/
handleWorkTabRequest :: !(Task a) !HTTPRequest *HSt -> (!HTTPResponse, !*HSt) | iData a
handleWorkTabRequest mainTask request hst
	# thisUserId							= 0														// has to be fetched from the request in the future
	# taskId 								= http_getValue "taskid" request.arg_get "error"		// fetch task id of the tab selecetd
	# (toServer, htmlTree, maybeError, hst)	= calculateTaskTree thisUserId mainTask hst 			// calculate the TaskTree given the id of the current user
	# (htmlcode,hst) 						= determineTaskForTab thisUserId taskId htmlTree hst 	// filter out the code to display in this tab
	# htmlstring							= toString (print_to_stdout htmlcode [#!])
	= ({http_emptyResponse & rsp_data = htmlstring},hst)