definition module TaskState

import SystemTypes

from Task		import :: TaskTime
from GenUpdate	import :: UpdateMask

derive JSONEncode TopInstance, TaskState, ParallelMeta, ParallelItem
derive JSONDecode TopInstance, TaskState, ParallelMeta, ParallelItem

//Persistent context of active tasks
:: TopInstance =
	{ instanceId	:: !(Either SessionId TopNo)
	, nextTaskNo	:: !TaskNo
	, nextTaskTime	:: !TaskTime
	, progress		:: !ProgressMeta
	, management	:: !ManagementMeta
	, task			:: !Dynamic
	, state			:: !Either TaskState String	//Task state or error message
	, attributes	:: !TaskMeta
	}

:: TaskState
	= TCBasic		!TaskId !JSONNode !TaskTime !Bool 									//Encoded value and stable indicator
	| TCInteract	!TaskId !JSONNode !TaskTime ![(!JSONNode,!UpdateMask,!Bool)] !Int
	| TCProject		!TaskId !JSONNode !TaskState
	| TCStep		!TaskId !(Either TaskState (!JSONNode,!Int,!TaskState))
	| TCParallel	!TaskId !ParallelMeta ![ParallelItem] 
	| TCShared		!TaskId !JSONNode !Int !TaskState
	| TCStable		!TaskId	!JSONNode !TaskTime
	| TCEmpty		!TaskId !TaskTime

//Parallel has a bit more complex state so we define it as a record
:: ParallelMeta = 
	{ nextIdx		:: !Int
	, listVersion	:: !Int		//Version number of the shared list state
	}
	
:: ParallelItem =
	{ taskId			:: !TaskId					//Unique task id
	
	, task				:: !Dynamic					// Encoded task definition
	, state				:: !TaskState				// State of the parallel item
	, lastValue			:: !JSONNode				// Cached task value, this field is recomputed on each evaluation
	, lastAttributes	:: ![TaskAttribute]			// Cached meta-data, this field is recomputed on each evaluation

	, stack				:: !Int						//Stack order (required for properly laying out tasks in tabs or windows)
	, detached			:: !Bool
	, progress			:: !Maybe ProgressMeta
	, management		:: !Maybe ManagementMeta
	}

:: ParallelControl 		//Never actually stored, but used for manipulating sets of parallel items
	= AppendTask		!ParallelItem		// add an item to a parallel list																		
	| RemoveTask		!TaskId				// remove the task with indicated id from the set


//Conversion to a representation of task states which hides all internal details
instanceToTaskListItem	:: !TopInstance -> TaskListItem
stateToTaskListItems	:: !TaskState -> [TaskListItem]