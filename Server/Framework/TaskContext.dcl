definition module TaskContext

import SystemTypes

from GenUpdate	import :: UpdateMask

derive JSONEncode TaskContext, ProcessState, TaskContextTree, SubTaskContext, ParallelMeta
derive JSONDecode TaskContext, ProcessState, TaskContextTree, SubTaskContext, ParallelMeta

//Persistent context of active tasks
:: TaskContext = TaskContext !ProcessId !ProgressMeta !ManagementMeta !TaskMeta !ProcessState

:: ProcessState
	= TTCRunning !Dynamic !TaskContextTree
	| TTCFinished !Dynamic
	| TTCExcepted !String

:: TaskContextTree
	= TCBasic !JSONNode !Bool //Encoded value and stable indicator
	| TCInteract !JSONNode ![(!JSONNode,!UpdateMask,!Bool)] !Int
	| TCProject !JSONNode !TaskContextTree
	| TCStep !(Either TaskContextTree (!JSONNode,!Int,!TaskContextTree))
	| TCParallel !JSONNode !ParallelMeta ![(!SubTaskId,!SubTaskOrder,!SubTaskContext)]
	| TCEmpty

:: SubTaskId	:== Int
:: SubTaskOrder :== Int	//Extra ordering information of tasks (required for properly laying out tasks in tabs or windows)

:: SubTaskContext	//Task (encoded), Context or JSON node	
	= STCEmbedded !(Maybe (!JSONNode, TaskContextTree))			
	| STCDetached !TaskId !ProgressMeta !ManagementMeta !TaskMeta !(Maybe (!JSONNode,!TaskContextTree))

//Parallel has a bit more complex administration so we define it as a record
:: ParallelMeta = 
	{ nextIdx		:: !Int
	, stateId		:: !String
	, stateVersion	:: !Int		//Version number of the shared state
	, metaVersion	:: !Int		//Version number of the meta-data of the parallel composition
	}
