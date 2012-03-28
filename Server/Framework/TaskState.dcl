definition module TaskState

import SystemTypes

from Task		import :: TaskTime
from GenUpdate	import :: UpdateMask

derive JSONEncode TopInstance, TaskTree, ParallelMeta, ParallelItem
derive JSONDecode TopInstance, TaskTree, ParallelMeta, ParallelItem

//Persistent context of active tasks
:: TopInstance =
	{ instanceId	:: !TopNo
	, sessionId		:: !Maybe SessionId
	, nextTaskNo	:: !TaskNo
	, nextTaskTime	:: !TaskTime
	, progress		:: !ProgressMeta
	, management	:: !ManagementMeta
	, task			:: !Dynamic
	, tree			:: !Either TaskTree String			//Task state or error message
	, shares		:: ![(!TaskNo,!JSONNode)]
	, attributes	:: !TaskMeta
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
instanceToTaskListItem	:: !TopInstance -> TaskListItem
stateToTaskListItems	:: !TaskTree -> [TaskListItem]