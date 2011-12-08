definition module TaskContext

import SystemTypes

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
	= TCBasic !(Map String JSONNode)
	| TCBind !(Either TaskContextTree (!JSONNode,!Int,!TaskContextTree))
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
	
//Access functions for basic tasks
getLocalVar :: !String !TaskContextTree -> Maybe a | JSONDecode{|*|} a
setLocalVar :: !String !a !TaskContextTree -> TaskContextTree | JSONEncode{|*|} a
delLocalVar :: !String !TaskContextTree -> TaskContextTree