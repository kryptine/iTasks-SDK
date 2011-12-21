definition module TaskContext

import SystemTypes

from GenUpdate	import :: UpdateMask

derive JSONEncode TaskContext, ProcessState, TaskContextTree, SubTaskContext, ParallelMeta
derive JSONDecode TaskContext, ProcessState, TaskContextTree, SubTaskContext, ParallelMeta

//Persistent context of active tasks
:: TaskContext = TaskContext !ProcessId !TaskMeta !ProgressMeta !ManagementMeta !ChangeNo !ProcessState

:: ChangeNo	:== Int
:: ProcessState
	= TTCRunning !Dynamic !TaskContextTree
	| TTCFinished !Dynamic
	| TTCExcepted !String

:: TaskContextTree
	= TCBasic !JSONNode !Bool //Encoded value and stable indicator
	| TCInteract !JSONNode ![(!JSONNode,!UpdateMask,!Bool)] !Timestamp !(Maybe (String,JSONNode))
	| TCProject !JSONNode !TaskContextTree
	| TCStep !(Either TaskContextTree (!JSONNode,!Int,!TaskContextTree))
	| TCParallel !JSONNode !ParallelMeta ![(!SubTaskId,!SubTaskOrder,!SubTaskContext)]
	| TCEmpty

:: SubTaskId	:== Int
:: SubTaskOrder :== Int	//Extra ordering information of tasks (required for properly laying out tasks in tabs or windows)

:: SubTaskContext
	= STCHidden !TaskMeta !(Maybe (!JSONNode,!TaskContextTree))		//Properties, Task (encoded), Context or JSON node
	| STCEmbedded !TaskMeta !(Maybe (!JSONNode,TaskContextTree))			
	| STCDetached !TaskId !TaskMeta !ProgressMeta !ManagementMeta !(Maybe (!JSONNode,!TaskContextTree))

//Parallel has a bit more complex administration so we define it as a record
:: ParallelMeta = 
	{ nextIdx		:: !Int
	, stateId		:: !String
	, stateChanged	:: !Timestamp
	, infoChanged	:: !Timestamp
	}