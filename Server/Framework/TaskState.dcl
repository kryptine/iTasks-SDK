definition module TaskState

import SystemTypes

from Task		import :: TaskTime, :: TaskResult
from GenUpdate	import :: UpdateMask

derive JSONEncode TaskInstance, TaskTree
derive JSONDecode TaskInstance, TaskTree

//Persistent context of active tasks
:: TaskInstance =
	{ instanceNo	:: !InstanceNo						//Unique global identification
	, sessionId		:: !Maybe SessionId					//Optionally an alternative identification by session id
	, parent		:: !InstanceNo						//zero for top-level instances, instance that detached this one otherwise
	, nextTaskNo	:: !TaskNo
	, nextTaskTime	:: !TaskTime
	, progress		:: !ProgressMeta					
	, management	:: !ManagementMeta
	, task			:: !Dynamic
	, result		:: !TaskResult JSONNode				//Result of last evaluation
	, shares		:: ![(!TaskNo,!JSONNode)]			//Locally shared data
	, lists			:: ![(!TaskId,![TaskListEntry])]	//Shared task lists of parallel tasks
	, observers		:: ![InstanceNo]					//List of instances that may be affected by changes in this instance
	}

:: TaskTree
	= TCInit		!TaskId !TaskTime
	| TCBasic		!TaskId !TaskTime !JSONNode !Bool 									//Encoded value and stable indicator
	| TCInteract	!TaskId !TaskTime !JSONNode !JSONNode !JSONNode !UpdateMask
	| TCProject		!TaskId !JSONNode !TaskTree
	| TCStep		!TaskId !(Either TaskTree (!JSONNode,!Int,!TaskTree))
	| TCParallel	!TaskId 
	| TCShared		!TaskId !TaskTree
	| TCStable		!TaskId !TaskTime !JSONNode
	| TCEmpty		!TaskId !TaskTime

:: TaskListEntry	=
	{ entryId			:: !TaskId					//Identification of entries in the list (for easy updating)
	, state				:: !TaskListEntryState		//Tree if embedded, or instance no if detached
	, result			:: !TaskResult JSONNode		//Stored result of last evaluation (for detached tasks this is a cached copy)
	, time				:: !TaskTime				//Last modified time
	, removed			:: !Bool					//Flag for marking this entry as 'removed', actual removal is done by the controlling parallel combinator
	}

:: TaskListEntryState
	= EmbeddedState !Dynamic									//The task definition
	| DetachedState !InstanceNo !ProgressMeta !ManagementMeta	//A reference to the detached task (management and progress meta are cached copies)
