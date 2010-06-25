implementation module WorkListHandler

import StdEnv
import Http, TSt, UserDB, ProcessDB
import Text, JSON, Time, Util

:: WorkList			= 	{ success		:: Bool
						, total			:: Int
						, worklist		:: [WorkListItem]
						}
						
:: WorkListItem 	= 	{ taskid			:: !String 					// Task id of the work item
						, manager			:: !String					// Id of the user who issued the work
				 		, subject			:: !String 					// Name give to the task, which can be a short description of the work to do
				 		, priority			:: !TaskPriority			// Priority of the task
				 		, progress			:: !TaskProgress			// Progress of the task
				 		, timestamp			:: !Timestamp				// Time stamp when the task was issued
				 		, latestExtEvent	:: !Maybe Timestamp			// Time stamp when the latest event on the task occurred
				 		, deadline			:: !Maybe Timestamp			// Time stamp with deadline
				 		, tree_path			:: ![Bool]					// Path in the tree structure
				 		, tree_last			:: !Bool					// Is this item the last of a set of siblings
				 		, tree_icon			:: !String					// An icon name. The actual icon image is defined in the css.											
				  		, tree_new			:: !Bool					// Is this item new
				  		}
//JSON encoding for the used types 				
derive JSONEncode WorkList, WorkListItem, TaskPriority, TaskProgress
//JSON specialization for Timestamp: Ignore the constructor
JSONEncode{|Timestamp|}	(Timestamp x)	= JSONEncode{|*|} x

handleWorkListRequest :: !HTTPRequest !*TSt -> (!HTTPResponse, !*TSt)
handleWorkListRequest request tst=:{staticInfo}
	# user					= staticInfo.currentSession.user
	# (proclist,tst)		= getProcessesForUser user [Active] tst
	# fproclist				= filter proclist
	# workitems				= bldWorkItems fproclist
	# worklist				= { success		= True
							  , total		= length workitems
							  , worklist	= workitems
							  }
	= ({http_emptyResponse & rsp_data = toString (toJSON worklist)}, tst)
where
	filter plist = [p \\ p <- plist | filter` p]
	where
		filter` p 
			= case p.inParallelType of
				Nothing 		= True
				(Just Open)
					= case p.parent of
						Just parent = not (isMember parent [p.Process.taskId \\ p <- plist])
						Nothing		= False
				(Just Closed)
					= staticInfo.currentSession.user <> p.Process.properties.systemProps.SystemProperties.manager

bldWorkItems :: [Process] -> [WorkListItem]
bldWorkItems processes
	= markLast [
		{ taskid		= taskId
		, manager		= toString p.systemProps.SystemProperties.manager
		, subject		= p.managerProps.ManagerProperties.subject
		, priority		= p.managerProps.ManagerProperties.priority
		, progress		= p.workerProps.WorkerProperties.progress
		, timestamp		= p.systemProps.SystemProperties.issuedAt
		, latestExtEvent = p.systemProps.SystemProperties.latestExtEvent
		, deadline		= p.managerProps.ManagerProperties.deadline
		, tree_path		= []
		, tree_last		= False
		, tree_icon		= "task"
		, tree_new		= isNothing p.systemProps.SystemProperties.firstEvent
		} \\ {Process|taskId,properties = p} <- processes]
		
markLast :: [WorkListItem] -> [WorkListItem]
markLast [] = []
markLast l	= init l ++ [{last l & tree_last = True}]