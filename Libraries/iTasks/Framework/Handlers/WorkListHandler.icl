implementation module WorkListHandler

import StdEnv
import Http, TSt, UserDB
import Text, JSON, Time, Util

:: WorkList			= 	{ success		:: Bool
						, total			:: Int
						, worklist		:: [WorkListItem]
						}
						
:: WorkListItem 	= 	{ taskid		:: String 					// Task id of the work item
						, delegatorId	:: Int 						// Id of the user who issued the work
						, delegatorName	:: String					// Display name of the user who issued the work 
						, processname	:: String					// Name given to the work process the task belongs to
				 		, subject		:: String 					// Name give to the task, which can be a short description of the work to do
				 		, priority		:: TaskPriority				// Priority of the task
				 		, progress		:: TaskProgress				// Progress of the task
				 		, timestamp		:: Timestamp				// Time stamp when the task was issued
				 		, deadline		:: Maybe Timestamp			// Time stamp with deadline
				 		, tree_path		:: [Bool]					// Path in the tree structure
				 		, tree_last		:: Bool						// Is this item the last of a set of siblings
				 		, tree_icon		:: String					// An icon name. The actual icon image is defined in the css.											
				  		}

//JSON encoding for the used types 				
derive JSONEncode WorkList, WorkListItem, TaskPriority, TaskProgress
//JSON specialization for Timestamp: Ignore the constructor
JSONEncode{|Timestamp|}	(Timestamp x) c						= JSONEncode{|*|} x c

handleWorkListRequest :: !HTTPRequest !*TSt -> (!HTTPResponse, !*TSt)
handleWorkListRequest request tst
	# (mbError,forest,tst)			= calculateTaskForest False tst
	# (uid, tst)					= getCurrentUser tst
	# (workitems,tst)				= addDelegatorNames (determineListWorkItems uid forest) tst
	# worklist						= { success		= True
										, total		= length workitems
										, worklist	= workitems
									  }
	= ({http_emptyResponse & rsp_data = toJSON worklist}, tst)

//Calculate the work items for a list of trees.
//
//We calculate the items of a task forest in reverse order because
//when a single tree has no output the second last must be treated
//as last in the forest. This is needed to get the tree lines displayed
//correct.
determineListWorkItems :: !UserId ![TaskTree] -> [WorkListItem]
determineListWorkItems userId forest = flatten (reverse (determineListWorkItems` userId (reverse forest)))
where
	determineListWorkItems` userId []
		= []
	determineListWorkItems` userId [x:xs]
		# tree = determineTreeWorkItems userId True x
		| isEmpty tree	= determineListWorkItems` userId xs
		| otherwise		= [tree : map (determineTreeWorkItems userId False) xs]
	

//Calculates the work items for a list of trees, but with
//a fixed "isLast" parameter. This is necessary to propagate the current "isLast"
//parameter down the tree when nodes are skipped.
determineListWorkItemsFixed :: !UserId !Bool ![TaskTree] -> [WorkListItem]
determineListWorkItemsFixed userId isLast forest = flatten (map (determineTreeWorkItems userId isLast) forest)

determineTreeWorkItems :: !UserId !Bool !TaskTree -> [WorkListItem]
//MainTask nodes: the actual entries
determineTreeWorkItems userId isLast (TTMainTask ti mti sequence)
	| (not ti.TaskInfo.active) || ti.TaskInfo.finished									// Inactive or finished, ignore whole branch
		= []
	| fst mti.managerProps.worker <> userId 											// Not our work, no new item
		= determineListWorkItemsFixed userId isLast sequence	
	| otherwise
		# items		= determineListWorkItems userId sequence
		= [{mainTaskItem & tree_last = isLast}: shiftWorkItems (not isLast) items]
where
	mainTaskItem = {mkWorkItem
				   & taskid = ti.TaskInfo.taskId
				   , subject = mti.systemProps.TaskSystemProperties.subject
				   , delegatorId = fst mti.systemProps.TaskSystemProperties.manager
				   , tree_last = isLast
				   , timestamp = mti.systemProps.TaskSystemProperties.issuedAt
				   , deadline = mti.managerProps.TaskManagerProperties.deadline
				   , priority = mti.managerProps.TaskManagerProperties.priority
				   , progress = mti.workerProps.TaskWorkerProperties.progress
				   }

//Sequence nodes
determineTreeWorkItems userId isLast (TTSequenceTask ti sequence)
	| (not ti.TaskInfo.active) || ti.TaskInfo.finished				// Inactive or finished, ignore whole branch
		= []
	| otherwise
		| isLast	= determineListWorkItems userId sequence
		| otherwise	= determineListWorkItemsFixed userId False sequence
//Parallel nodes
determineTreeWorkItems userId isLast (TTParallelTask info combination branches)	
	| (not info.TaskInfo.active) || info.TaskInfo.finished			//Inactive or finished, ignore whole branch
		= []
	| otherwise
		| isLast	= determineListWorkItems userId branches
		| otherwise	= determineListWorkItemsFixed userId False branches

//Other nodes	
determineTreeWorkItems _ _ _ = []

mkWorkItem :: WorkListItem
mkWorkItem =	{ taskid		= ""
				, delegatorId	= -1
				, delegatorName	= ""
				, processname	= ""
				, subject		= ""
				, priority		= NormalPriority
				, progress		= TPActive
				, timestamp		= Timestamp 0
				, deadline		= Nothing
				, tree_path		= []
				, tree_last		= False
				, tree_icon		= "task"
				}

shiftWorkItem :: !Bool !WorkListItem -> WorkListItem
shiftWorkItem step item =:{tree_path} = {item & tree_path = [step:tree_path]}				

shiftWorkItems :: !Bool ![WorkListItem] -> [WorkListItem]
shiftWorkItems step items = map (shiftWorkItem step) items

addDelegatorNames :: [WorkListItem] *TSt -> ([WorkListItem], *TSt)
addDelegatorNames items tst
	# (names, tst)		= getDisplayNames [i.WorkListItem.delegatorId \\ i <- items] tst
	= ([{i & delegatorName = if (i.WorkListItem.delegatorId <> -1) name ""} \\ i <- items & name <- names], tst)
	

