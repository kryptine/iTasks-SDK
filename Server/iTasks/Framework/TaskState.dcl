definition module iTasks.Framework.TaskState

import iTasks.API.Core.SystemTypes

from iTasks.Framework.Task	import :: TaskTime, :: TaskResult, :: TaskRep, :: EventNo

derive JSONEncode TIMeta, SessionInfo, TIReduct, TaskTree
derive JSONDecode TIMeta, SessionInfo, TIReduct, TaskTree

//Persistent context of active tasks
//Split up version of task instance information

:: TIMeta =
	{ instanceNo	:: !InstanceNo			//Unique global identification
	, session		:: !Maybe SessionInfo	//Set for task instances that are linked to user sessions
	, parent		:: !InstanceNo		
	, worker		:: !Maybe User			//Identity of the user working on this instance (this determines the value of the currentUser share)
	, progress		:: !ProgressMeta
	, management	:: !ManagementMeta
	}
:: SessionInfo =
	{ sessionId		:: SessionId
	, lastEvent		:: EventNo
	}
	
:: TIReduct = 
	{ task			:: !Task JSONNode
	, nextTaskNo	:: !TaskNo
	, nextTaskTime	:: !TaskTime
	, shares		:: !Map TaskId JSONNode				//Locally shared data
	, lists			:: !Map TaskId [TaskListEntry]		//Parallel task lists
	, tasks			:: !Map TaskId Dynamic				//Task functions of embedded parallel tasks
	}

:: TaskTree
	= TCInit		!TaskId !TaskTime													//Initial state for all tasks
	| TCBasic		!TaskId !TaskTime !JSONNode !Bool 									//Encoded value and stable indicator
	| TCInteract	!TaskId !TaskTime !JSONNode !JSONNode !JSONNode !InteractionMask
	| TCInteract1	!TaskId !TaskTime !JSONNode !InteractionMask
	| TCInteract2	!TaskId !TaskTime !JSONNode !JSONNode !InteractionMask
	| TCProject		!TaskId !JSONNode !TaskTree
	| TCStep		!TaskId !TaskTime !(Either TaskTree (DeferredJSON,Int,TaskTree))
	| TCParallel	!TaskId !TaskTime
	| TCShared		!TaskId !TaskTime !TaskTree
	| TCStable		!TaskId !TaskTime !DeferredJSON
	| TCNop			
	| TCDestroy		!TaskTree															//Marks a task state as garbage that must be destroyed

:: DeferredJSON
	= E. a:	DeferredJSON !a & TC a & JSONEncode{|*|} a
	|		DeferredJSONNode !JSONNode

derive JSONEncode DeferredJSON
derive JSONDecode DeferredJSON
	
:: TaskListEntry	=
	{ entryId			:: !TaskId					//Identification of entries in the list (for easy updating)
	, state				:: !TaskListEntryState		//Tree if embedded, or instance no if detached
	, lastEval			:: !TaskResult JSONNode		//Result of last evaluation
	, attributes		:: !Map String String		//Stored attributes of last evaluation
	, createdAt			:: !TaskTime				//Time the entry was added to the set (used by layouts to highlight new items)
	, lastEvent			:: !TaskTime				//Last modified time
	, removed			:: !Bool					//Flag for marking this entry as 'removed', actual removal is done by the controlling parallel combinator
	}												//If it is false we have determined that this is not necessary during the last computation

:: TaskListEntryState
	= EmbeddedState 											//An embedded task
	| DetachedState !InstanceNo !ProgressMeta !ManagementMeta	//A reference to the detached task (management and progress meta are cached copies)
