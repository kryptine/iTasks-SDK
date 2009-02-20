implementation module WorkListHandler

import StdEnv
import Http, TSt, UserDB
import Text, JSON, Time, Util
import iDataForms

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
				 		, timestamp		:: Int						// Time stamp when the task was issued
				 		, split			:: Bool						// Is this task split into subtasks
				 		, tree_path		:: [Bool]					// Path in the tree structure
				 		, tree_last		:: Bool						// Is this item the last of a set of siblings
				 		, tree_icon		:: String					// An icon name. The actual icon image is defined in the css.
				 													// Current possible values: editTask, orTask, andTask, conditionTask, timeTask, systemTask, finishedTask 
				  		}
				  													// And also: andTaskMU, maybeTask
					
derive JSONEncode WorkList, WorkListItem, TaskPriority

handleWorkListRequest :: !HTTPRequest !*TSt -> (!HTTPResponse, !*TSt)
handleWorkListRequest request tst
	# (mbError,forest,tst)			= calculateTaskForest False tst
	# (uid, tst)					= getCurrentUser tst
	# (workitems,tst)				= addDelegatorNames (determineListWorkItems uid False forest) tst
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
determineListWorkItems :: !UserId !Bool ![TaskTree] -> [WorkListItem]
determineListWorkItems userId addSequences forest = flatten (reverse (determineListWorkItems` userId addSequences (reverse forest)))
where
	determineListWorkItems` userId addSequences [] 	= []
	determineListWorkItems` userId addSequences [x:xs]
		# tree = determineTreeWorkItems userId addSequences True x
		= case tree of
			[]	= determineListWorkItems` userId addSequences xs
			_	= [tree : map (determineTreeWorkItems userId addSequences False) xs]	

//Calculates the work items for a list of trees, but with
//a fixed "isLast" parameter. This is necessary to propagate the current "isLast"
//paramater down the tree when nodes are skipped.
determineListWorkItemsFixed :: !UserId !Bool !Bool ![TaskTree] -> [WorkListItem]
determineListWorkItemsFixed userId addSequences isLast forest = flatten (map (determineTreeWorkItems userId addSequences isLast) forest)

determineTreeWorkItems :: !UserId !Bool !Bool !TaskTree -> [WorkListItem] //Work item, along with the amount of children it has
//Process nodes
determineTreeWorkItems userId addSequences isLast (TTProcess info sequence)
	| info.ProcessInfo.userId <> userId
		= determineListWorkItemsFixed userId True isLast sequence				//Not our work, no new item
	| otherwise
		# subitems	= determineListWorkItemsFixed userId False isLast sequence 
		= case subitems of
			[]					= [processItem]									//Add a new item
			[item:items]
				| item.split	= [{item & taskid = (toString info.processId), subject = info.processLabel}:items]	//'Merge' with subitem
								= [processItem,item:items]
where
	processItem = mkWorkItem (toString info.processId) info.processLabel False isLast "editTask" info.ProcessInfo.delegatorId						

//Sequence nodes
determineTreeWorkItems userId addSequences isLast (TTSequenceTask info sequence)
	| (not info.TaskInfo.active) || info.TaskInfo.finished			//Inactive or finished, ignore whole branch
		= []
	| info.TaskInfo.userId <> userId								//Not our work, no new item
		= determineListWorkItemsFixed userId True isLast sequence	
	| not addSequences												//We don't need to add the sequence
		= determineListWorkItemsFixed userId False isLast sequence
	| otherwise
		# subitems	= determineListWorkItemsFixed userId False isLast sequence
		= case subitems of
			[]					= [sequenceItem]										//Add item
			[item:items]
				| item.split	= [{item & taskid = info.TaskInfo.taskId, subject = info.TaskInfo.taskLabel}:items]		//'Merge' with subitem
								= [sequenceItem,item:items]								//Add item
where
	sequenceItem = mkWorkItem info.TaskInfo.taskId info.TaskInfo.taskLabel False isLast "editTask" info.TaskInfo.delegatorId

//Parallel nodes
determineTreeWorkItems userId addSequences isLast (TTParallelTask info combination branches)	
	| (not info.TaskInfo.active) || info.TaskInfo.finished			//Inactive or finished, ignore whole branch
		= []
	| info.TaskInfo.userId <> userId								//Not our work, no new item
		= determineListWorkItemsFixed userId True isLast branches
	| otherwise
		= case combination of
			(TTSplit _)	= [parallelItem : map (shiftWorkItem (not isLast)) (determineListWorkItems userId True branches) ]	
			_			= determineListWorkItemsFixed userId False isLast branches
where
	parallelItem = mkWorkItem info.TaskInfo.taskId info.TaskInfo.taskLabel True isLast "andTask" info.TaskInfo.delegatorId

//Basic nodes			
determineTreeWorkItems _ _ _ _ = []

mkWorkItem :: !TaskId !String !Bool !Bool !String !UserId -> WorkListItem
mkWorkItem taskId label split last icon delegator
			=	{ taskid		= taskId
				, delegatorId	= delegator
				, delegatorName	= ""
				, processname	= ""
				, subject		= label
				, priority		= NormalPriority
				, timestamp		= 0
				, split			= split
				, tree_path		= []
				, tree_last		= last
				, tree_icon		= icon
				}

shiftWorkItem :: !Bool !WorkListItem -> WorkListItem
shiftWorkItem step item =:{tree_path} = {item & tree_path = [step:tree_path]}				

addDelegatorNames :: [WorkListItem] *TSt -> ([WorkListItem], *TSt)
addDelegatorNames items tst
	# (names, tst)		= accHStTSt (accNWorldHSt (accUserDBNWorld (getDisplayNames [i.WorkListItem.delegatorId \\ i <- items]))) tst
	= ([{i & delegatorName = name} \\ i <- items & name <- names], tst)
	
