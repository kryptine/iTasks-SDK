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
				 		, priority		:: Maybe TaskPriority		// Priority of the task
				 		, timestamp		:: Maybe Int				// Time stamp when the task was issued
				 		, tree_shift	:: Bool						// Marks if the item has been shifted to the right
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
	# (workitems,tst)				= addDelegatorNames (determineListWorkItems uid False False forest) tst
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
determineListWorkItems :: !UserId !Bool !Bool ![TaskTree] -> [WorkListItem]
determineListWorkItems userId ourWork addSequences forest = flatten (reverse (determineListWorkItems` userId ourWork addSequences (reverse forest)))
where
	determineListWorkItems` userId ourWork addSequences [] 	= []
	determineListWorkItems` userId ourWork addSequences [x:xs]
		# tree = determineTreeWorkItems userId ourWork addSequences True x
		| isEmpty tree	= determineListWorkItems` userId ourWork addSequences xs
		| otherwise		= [tree : map (determineTreeWorkItems userId ourWork addSequences False) xs]
	

//Calculates the work items for a list of trees, but with
//a fixed "isLast" parameter. This is necessary to propagate the current "isLast"
//parameter down the tree when nodes are skipped.
determineListWorkItemsFixed :: !UserId !Bool !Bool !Bool ![TaskTree] -> [WorkListItem]
determineListWorkItemsFixed userId ourWork addSequences isLast forest = flatten (map (determineTreeWorkItems userId ourWork addSequences isLast) forest)

determineTreeWorkItems :: !UserId !Bool !Bool !Bool !TaskTree -> [WorkListItem]
//MainTask nodes
//Always a maintask that we have to do, or have delegated.
determineTreeWorkItems userId ourWork addSequences isLast (TTMainTask ti mti sequence)
	| (not ti.TaskInfo.active) || ti.TaskInfo.finished									// Inactive or finished, ignore whole branch
		= []
	| mti.TaskProperties.userId <> userId 												// Not our work, no new item
		= determineListWorkItemsFixed userId False True isLast sequence	
	| otherwise
		| isLast
			= case (determineListWorkItems userId True False sequence) of
				[] 		= [mainTaskItem]
				items	= [{mainTaskItem & tree_last = shiftedItems items} : items]
		| otherwise
			= [mainTaskItem : determineListWorkItemsFixed userId True False isLast sequence]	// A task we have to do, or have delegated, add a new item.
where
	mainTaskItem = {mkWorkItem & taskid = ti.TaskInfo.taskId, subject = mti.TaskProperties.subject, delegatorId = mti.TaskProperties.delegatorId
				   , tree_last = isLast, timestamp = Just ((\(Time i) . i) mti.TaskProperties.issuedAt), priority = Just mti.TaskProperties.priority }

//Sequence nodes
determineTreeWorkItems userId ourWork addSequences isLast (TTSequenceTask ti sequence)
	| (not ti.TaskInfo.active) || ti.TaskInfo.finished				// Inactive or finished, ignore whole branch
		= []
	| not addSequences												// We don't need to add the sequence
		= determineListWorkItemsFixed userId ourWork False isLast sequence
	| not ourWork
		= determineListWorkItemsFixed userId ourWork False isLast sequence
	| otherwise
		| isLast													// We are last, but maybe our subitems will add some more items
			= case (determineListWorkItems userId ourWork False sequence) of
				[]		= [sequenceItem]
				items	= [{sequenceItem & tree_last = shiftedItems items} : items]
		| otherwise
			= [sequenceItem: determineListWorkItemsFixed userId ourWork False False sequence]
where
	sequenceItem = {mkWorkItem & taskid = ti.TaskInfo.taskId, subject = ti.TaskInfo.taskLabel, tree_last = isLast}

//Parallel nodes
determineTreeWorkItems userId ourWork addSequences isLast (TTParallelTask info combination branches)	
	| (not info.TaskInfo.active) || info.TaskInfo.finished			//Inactive or finished, ignore whole branch
		= []
	| otherwise
		= case combination of
			(TTSplit _)
				# subitems	= determineListWorkItems userId ourWork True branches
				| ourWork	= map (shiftWorkItem (not isLast)) subitems 
				| otherwise	= subitems
			_			
				| isLast	= determineListWorkItems userId ourWork False branches
				| otherwise	= determineListWorkItemsFixed userId ourWork False False branches

//Basic nodes	
determineTreeWorkItems _ _ _ _ _ = []

mkWorkItem :: WorkListItem
mkWorkItem =	{ taskid		= ""
				, delegatorId	= -1
				, delegatorName	= ""
				, processname	= ""
				, subject		= ""
				, priority		= Nothing
				, timestamp		= Nothing
				, tree_shift	= False
				, tree_path		= []
				, tree_last		= False
				, tree_icon		= "editTask"
				}

shiftWorkItem :: !Bool !WorkListItem -> WorkListItem
shiftWorkItem step item =:{tree_path} = {item & tree_path = [step:tree_path], tree_shift = True}				

addDelegatorNames :: [WorkListItem] *TSt -> ([WorkListItem], *TSt)
addDelegatorNames items tst
	# (names, tst)		= accHStTSt (getDisplayNames [i.WorkListItem.delegatorId \\ i <- items]) tst
	= ([{i & delegatorName = if (i.WorkListItem.delegatorId <> -1) name ""} \\ i <- items & name <- names], tst)

shiftedItems :: [WorkListItem] -> Bool
shiftedItems [{WorkListItem|tree_shift}:xs] = tree_shift
	

