definition module TaskContext

import SystemTypes

from GenUpdate	import :: UpdateMask

derive JSONEncode TopInstance, TaskState, ParallelMeta, ParallelItem
derive JSONDecode TopInstance, TaskState, ParallelMeta, ParallelItem

//Persistent context of active tasks
:: TopInstance =
	{ instanceId	:: !(Either SessionId TopNo)
	, nextTaskNo	:: !TaskNo
	, progress		:: !ProgressMeta
	, management	:: !ManagementMeta
	, task			:: !Dynamic
	, state			:: !Either TaskState String	//Task state or error message
	, attributes	:: !TaskMeta
	}

:: TaskState
	= TCBasic		!TaskId !JSONNode !Bool 										//Encoded value and stable indicator
	| TCInteract	!TaskId !JSONNode ![(!JSONNode,!UpdateMask,!Bool)] !Int
	| TCProject		!TaskId !JSONNode !TaskState
	| TCStep		!TaskId !(Either TaskState (!JSONNode,!Int,!TaskState))
	| TCParallel	!TaskId !JSONNode !ParallelMeta ![ParallelItem] 
	| TCEmpty		!TaskId

//Parallel has a bit more complex state so we define it as a record
:: ParallelMeta = 
	{ nextIdx		:: !Int
	, stateVersion	:: !Int		//Version number of the shared state
	, metaVersion	:: !Int		//Version number of the meta-data of the parallel composition
	}
	
:: ParallelItem =
	{ taskId		:: !TaskId					//Unique task id
	, stack			:: !Int						//Stack order (required for properly laying out tasks in tabs or windows)
	, detached		:: !Bool
	, progress		:: !Maybe ProgressMeta
	, management	:: !Maybe ManagementMeta
	, task			:: !Dynamic					// Encoded task definition
	, state			:: !TaskState				// State of the parallel item
	, attributes	:: ![TaskAttribute]			// Cached meta-data, this field is recomputed on each evaluation
	}

//Conversion to a representation of task states which hides all internal details
instanceToTaskListItem	:: !TopInstance -> TaskListItem
stateToTaskListItems	:: !TaskState -> [TaskListItem]