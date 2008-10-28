implementation module WorkListHandler

import StdEnv
import Http
import Text
import JSON
import Time
import iDataHandler
import TaskTree, TaskTreeFilters, InternaliTasksCommon

:: WorkListItem 	= 	{ taskid		:: String 					// Task id of the work item
			/* from */	, for			:: String 					// Id of the user who issued the work
						, processname	:: String					// Name given to the work process the task belongs to
				 		, subject		:: String 					// Name give to the task, which can be a short description of the work to do
				 		, priority		:: TaskPriority				// Priority of the task
				 		, timestamp		:: Int						// Time stamp when the task was issued
				  		}

derive JSONEncode WorkListItem, TaskPriority

handleWorkListRequest :: !(Task a) !HTTPRequest *HSt -> (!HTTPResponse, !*HSt) | iData a
handleWorkListRequest mainTask request hst
	# thisUserId							= 0												// has to be fetched from the request in the future
	# (toServer, htmlTree, maybeError, hst)	= calculateTaskTree thisUserId mainTask hst 	// Calculate the TaskTree given the id of the current user
	# worklist								= [	{ taskid 		= toString mytaskdescr.taskNrId
												, for 			= toString mytaskdescr.delegatorId
												, processname	= mytaskdescr.worflowLabel
												, subject 		= mytaskdescr.taskLabel
												, priority		= mytaskdescr.taskPriority
												, timestamp		= (\(Time i) -> i) mytaskdescr.timeCreated
												}				
											  \\ mytaskdescr <- collectTaskList (\taskdescr -> taskdescr.taskWorkerId == thisUserId) htmlTree
											  ]
	
	= ({http_emptyResponse & rsp_data = toJSON worklist}, hst)




