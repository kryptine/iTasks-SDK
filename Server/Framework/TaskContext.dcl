definition module TaskContext

import SystemTypes

from GenUpdate	import :: UpdateMask

derive JSONEncode TaskContext, ProcessState, TaskState, ParallelMeta, ParallelItem
derive JSONDecode TaskContext, ProcessState, TaskState, ParallelMeta, ParallelItem

//Persistent context of active tasks
:: TaskContext = TaskContext !(Either SessionId TopNo) !TaskNo !ProgressMeta !ManagementMeta !TaskMeta !ProcessState

:: ProcessState
	= TTCRunning !Dynamic !TaskState
	| TTCFinished !Dynamic
	| TTCExcepted !String

:: TaskState
	= TCBasic !TaskId !JSONNode !Bool 										//Encoded value and stable indicator
	| TCInteract !TaskId !JSONNode ![(!JSONNode,!UpdateMask,!Bool)] !Int
	| TCProject !JSONNode !TaskState
	| TCStep !TaskId !(Either TaskState (!JSONNode,!Int,!TaskState))
	| TCParallel !TaskId !ParallelContext !ParallelMeta ![ParallelItem] 
	| TCEmpty !TaskId

:: ParallelContext 	:== JSONNode

//Parallel has a bit more complex administration so we define it as a record
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
			
	, task			:: !Dynamic				// Encoded task definition
	, state			:: !TaskState			// State of the parallel item
	}

