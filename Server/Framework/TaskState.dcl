definition module TaskState

import SystemTypes

from Task		import :: TaskTime, :: TaskResult
from GenUpdate	import :: UpdateMask

derive JSONEncode TaskInstance, TaskTree, ParallelMeta, ParallelItem
derive JSONDecode TaskInstance, TaskTree, ParallelMeta, ParallelItem

//Persistent context of active tasks
:: TaskInstance =
	{ instanceNo	:: !InstanceNo
	, sessionId		:: !Maybe SessionId
	, nextTaskNo	:: !TaskNo
	, nextTaskTime	:: !TaskTime
	, progress		:: !ProgressMeta
	, management	:: !ManagementMeta
	, task			:: !Dynamic
	, result		:: !TaskResult JSONNode
	, shares		:: ![(!TaskNo,!JSONNode)]			//Locally shared data
	, lists			:: ![(!TaskNo,![TaskListEntry])]	//Shared task lists of parallel tasks
	}

:: TaskTree
	= TCInit		!TaskId !TaskTime
	| TCBasic		!TaskId !TaskTime !JSONNode !Bool 									//Encoded value and stable indicator
	| TCInteract	!TaskId !TaskTime !JSONNode !JSONNode !JSONNode !UpdateMask
	| TCProject		!TaskId !JSONNode !TaskTree
	| TCStep		!TaskId !(Either TaskTree (!JSONNode,!Int,!TaskTree))
	| TCParallel	!TaskId !ParallelMeta ![ParallelItem] 
	| TCShared		!TaskId !TaskTree
	| TCStable		!TaskId !TaskTime !JSONNode
	| TCEmpty		!TaskId !TaskTime

:: TaskListEntry	=
	{ state				:: !TaskListEntryState		//Tree if embedded, or instance no if detached
	, result			:: !TaskResult JSONNode		//Stored result of last evaluation
	, removed			:: !Bool					//Flag for marking this entry as 'removed', actual removal is done by the controlling parallel combinator
	}

:: TaskListEntryState
	= EmbeddedState !Dynamic !TaskTree				//Task & tree
	| DetachedState !InstanceNo	

//Parallel has a bit more complex state so we define it as a record
:: ParallelMeta = 
	{ nextIdx		:: !Int
	, listVersion	:: !Int		//Version number of the shared list state
	}
	
:: ParallelItem =
	{ taskId			:: !TaskId					//Unique task id
	
	, task				:: !Dynamic					// Encoded task definition
	, state				:: !TaskTree				// State of the parallel item
	, lastValue			:: !JSONNode				// Cached task value, this field is recomputed on each evaluation
	, lastAttributes	:: ![TaskAttribute]			// Cached meta-data, this field is recomputed on each evaluation

	, stack				:: !Int						//Stack order (required for properly laying out tasks in tabs or windows)
	, progress			:: !Maybe ProgressMeta
	, management		:: !Maybe ManagementMeta
	}

:: ParallelControl 		//Never actually stored, but used for manipulating sets of parallel items
	= AppendTask		!ParallelItem		// add an item to a parallel list																		
	| RemoveTask		!TaskId				// remove the task with indicated id from the set


//Conversion to a representation of task states which hides all internal details
instanceToTaskListItem	:: !TaskInstance -> TaskListItem
stateToTaskListItems	:: !TaskTree -> [TaskListItem]