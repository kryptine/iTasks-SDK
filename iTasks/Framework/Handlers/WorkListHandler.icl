implementation module WorkListHandler

import StdEnv
import Http, Session
import Text
import JSON
import Time
import iDataForms
import TaskTree, TaskTreeFilters, InternaliTasksCommon

:: WorkList			= 	{ success		:: Bool
						, total			:: Int
						, worklist		:: [WorkListItem]
						}
						
:: WorkListItem 	= 	{ taskid		:: String 					// Task id of the work item
						, delegator		:: String 					// Id of the user who issued the work
						, processname	:: String					// Name given to the work process the task belongs to
				 		, subject		:: String 					// Name give to the task, which can be a short description of the work to do
				 		, priority		:: TaskPriority				// Priority of the task
				 		, timestamp		:: Int						// Time stamp when the task was issued
				 		, tree_path		:: [Bool]					// Path in the tree structure
				 		, tree_last		:: Bool						// Is this item the last of a set of siblings
				 		, tree_icon		:: String					// An icon name. The actual icon image is defined in the css.
				 													// Current possible values: editTask, orTask, andTask, conditionTask, timeTask, systemTask, finishedTask 
				  		}											// And also: andTaskMU, maybeTask
																	


derive JSONEncode WorkList, WorkListItem, TaskPriority

handleWorkListRequest :: !(Task a) !HTTPRequest !Session *HSt -> (!HTTPResponse, !*HSt) | iData a
handleWorkListRequest mainTask request session hst
	# thisUserId							= session.Session.userId
	# (toServer, htmlTree, maybeError, maybeProcessTable, maybeThreadTable,hst)	
											= calculateTaskTree thisUserId False False False mainTask hst 	// Calculate the TaskTree given the id of the current user
	# workitems								= [	{ taskid 		= mytaskdescr.taskNrId
												, delegator		= toString mytaskdescr.delegatorId
												, processname	= mytaskdescr.worflowLabel
												, subject 		= mytaskdescr.taskLabel
												, priority		= mytaskdescr.taskPriority
												, timestamp		= (\(Time i) -> i) mytaskdescr.timeCreated
												, tree_path		= []
												, tree_last		= True
												, tree_icon		= "orTask"
												}				
											  \\ mytaskdescr <- determineTaskList thisUserId htmlTree | not mytaskdescr.curStatus
											  ]
	# worklist								= { success		= True
											  , total		= length workitems
											  , worklist	= workitems
											  }
	
	= ({http_emptyResponse & rsp_data = toJSON worklist}, hst)




