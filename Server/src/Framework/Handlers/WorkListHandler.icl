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
JSONEncode{|Timestamp|}	(Timestamp x) c	= JSONEncode{|*|} x c

import StdDebug
handleWorkListRequest :: !HTTPRequest !*TSt -> (!HTTPResponse, !*TSt)
handleWorkListRequest request tst=:{staticInfo}
	# username				= staticInfo.currentSession.user.User.userName
	# (processes,tst)		= getProcessesForUser username [Active] tst
	# (tmpprocs ,tst)		= getTempProcessesForUser username [Active] tst
	# proclist				= processes ++ tmpprocs
	# fproclist				= filter proclist
	# workitems				= bldWorkItems fproclist
	# worklist				= { success		= True
							  , total		= length workitems
							  , worklist	= workitems
							  }
	= ({http_emptyResponse & rsp_data = toJSON worklist}, tst)
where
	filter plist = [p \\ p <- plist | filter` p]
	where
		filter` p 
			= case p.inParallelType of
				Nothing 		= True
				(Just Open)
					= not (isMember p.parent [p.Process.processId \\ p <- plist])
				(Just Closed)
					| staticInfo.currentSession.user.User.userName <> toUserId p.Process.properties.systemProps.TaskSystemProperties.manager = True
					| otherwise = False	

bldWorkItems :: [Process] -> [WorkListItem]
bldWorkItems processes
	= markLast [
		{ taskid		= processId
		, manager		= p.systemProps.TaskSystemProperties.manager
		, subject		= p.managerProps.TaskManagerProperties.subject
		, priority		= p.managerProps.TaskManagerProperties.priority
		, progress		= p.workerProps.TaskWorkerProperties.progress
		, timestamp		= p.systemProps.TaskSystemProperties.issuedAt
		, latestExtEvent = p.systemProps.TaskSystemProperties.latestExtEvent
		, deadline		= p.managerProps.TaskManagerProperties.deadline
		, tree_path		= []
		, tree_last		= False
		, tree_icon		= "task"
		, tree_new		= isNothing p.systemProps.TaskSystemProperties.firstEvent
		} \\ {Process|processId,properties = p} <- processes]
		
markLast :: [WorkListItem] -> [WorkListItem]
markLast [] = []
markLast l	= init l ++ [{last l & tree_last = True}]