definition module TaskContext

import Types
from ProcessDB import :: Process(..)

derive JSONEncode TaskContext, TopTaskContext, SubTaskContext, ParallelMeta
derive JSONDecode TaskContext, TopTaskContext, SubTaskContext, ParallelMeta

//Persistent context of active tasks
:: TaskContext
	= TCTop !ProcessProperties !Int !TopTaskContext	
	| TCBasic !(Map String JSONNode)
	| TCBind !(Either TaskContext (!JSONNode,!TaskContext))
	| TCParallel !JSONNode !ParallelMeta ![(!Int,!SubTaskContext)]
	| TCTry !(Either TaskContext (!JSONNode,!TaskContext))

:: TopTaskContext
	= TTCActive !TaskContext
	| TTCSuspended !TaskContext
	| TTCFinished !JSONNode
	| TTCExcepted !String

:: SubTaskContext
	= STCHidden !TaskProperties !(Maybe (!JSONNode,!TaskContext))		//Properties, Task (encoded), Context or JSON node
	| STCBody !TaskProperties !(Maybe (!JSONNode,TaskContext))			
	| STCDetached !ProcessProperties !(Maybe (!JSONNode,!TaskContext))

//Parallel has a bit more complex administration so we define it as a record
:: ParallelMeta = 
	{ nextIdx		:: !Int
	, stop			:: !Bool
	, infoChanged	:: !Timestamp
	, stateChanged	:: !Timestamp
	}
	
//Access functions for basic tasks
getLocalVar :: !String !TaskContext -> Maybe a | JSONDecode{|*|} a
setLocalVar :: !String !a !TaskContext -> TaskContext | JSONEncode{|*|} a
delLocalVar :: !String !TaskContext -> TaskContext