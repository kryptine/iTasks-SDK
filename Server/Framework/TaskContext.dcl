definition module TaskContext

import SystemTypes
from ProcessDB import :: Process(..)

derive JSONEncode TaskContext, ProcessState, TaskContextTree, SubTaskContext, ParallelMeta
derive JSONDecode TaskContext, ProcessState, TaskContextTree, SubTaskContext, ParallelMeta

//Persistent context of active tasks
:: TaskContext = TaskContext !ProcessId !ProcessProperties !Int !ProcessState

:: ProcessState
	= TTCRunning !Dynamic !TaskContextTree
	| TTCFinished !Dynamic
	| TTCExcepted !String

:: TaskContextTree
	= TCBasic !(Map String JSONNode)
	| TCBind !(Either TaskContextTree (!JSONNode,!TaskContextTree))
	| TCParallel !JSONNode !ParallelMeta ![(!Int,!SubTaskContext)]
	| TCTry !(Either TaskContextTree (!JSONNode,!TaskContextTree))
	| TCEmpty

:: SubTaskContext
	= STCHidden !TaskMeta !(Maybe (!JSONNode,!TaskContextTree))		//Properties, Task (encoded), Context or JSON node
	| STCBody !TaskMeta !(Maybe (!JSONNode,TaskContextTree))			
	| STCDetached !ProcessProperties !(Maybe (!JSONNode,!TaskContextTree))

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