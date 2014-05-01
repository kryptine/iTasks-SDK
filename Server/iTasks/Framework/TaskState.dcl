definition module iTasks.Framework.TaskState

import iTasks.API.Core.Types

from iTasks.Framework.Task	import :: TaskTime, :: TaskResult, :: TaskRep, :: EventNo

derive JSONEncode TIMeta, TIReduct, TaskTree
derive JSONDecode TIMeta, TIReduct, TaskTree

//Persistent context of active tasks
//Split up version of task instance information

:: TIMeta =
	{ instanceNo	:: !InstanceNo			//Unique global identification
    , instanceKey   :: !InstanceKey         //Random string that a client needs to provide to access the task instance
    , instanceType  :: !TIType
    , session       :: !Bool                //Is this a session
	, listId        :: !TaskId              //Reference to parent tasklist
    , name          :: !Maybe String        //Identifier
	, progress		:: !ProgressMeta
	, attributes    :: !TaskAttributes      //Arbitrary meta-data
	}

:: TIProgress =
	{ value             :: !ValueStatus             //* Status of the task value
    , issuedAt			:: !DateTime				//* When was the task created
	, issuedBy			:: !User					//* By whom was the task created
    , involvedUsers     :: ![User]                  //* Users currently involved in the task
	, firstEvent		:: !Maybe DateTime			//* When was the first work done on this task
	, latestEvent		:: !Maybe DateTime			//* When was the latest event on this task	
	}

:: TIType
    = DetachedInstance                      //A detached task that is not in use
    | AttachedInstance ![TaskId] !User      //A previously detached task that has been attached to another instance
    | TmpAttachedInstance ![TaskId] !User   //A temporarily attached task that will automatically turn into a detached instance after evaluation

:: TIReduct =
	{ task			:: !Task JSONNode                   //Main task definition
    , tree          :: !TaskTree                        //Main task state
	, nextTaskNo	:: !TaskNo                          //Local task number counter
	, nextTaskTime	:: !TaskTime                        //Local task time (incremented at every evaluation)
    , lastEventNo   :: !EventNo                         //Last event number received from a client
	, shares		:: !Map TaskId JSONNode				//Locally shared data
	, lists			:: !Map TaskId [TaskListEntry]		//Parallel task lists
	, tasks			:: !Map TaskId Dynamic				//Task functions of embedded parallel tasks
	}

:: TIValue
   = TIValue !(TaskValue JSONNode)
   | TIException !Dynamic !String

:: TaskTree
	= TCInit		            !TaskId !TaskTime													//Initial state for all tasks
	| TCBasic		            !TaskId !TaskTime !JSONNode !Bool 									//Encoded value and stable indicator
	| TCInteract	            !TaskId !TaskTime !JSONNode !JSONNode !JSONNode !InteractionMask
	| TCInteractLocal	        !TaskId !TaskTime !JSONNode !JSONNode !InteractionMask
	| TCInteractViewOnly	    !TaskId !TaskTime !JSONNode !JSONNode !InteractionMask
	| TCInteractLocalViewOnly   !TaskId !TaskTime !JSONNode !InteractionMask
	| TCInteract1				!TaskId !TaskTime !JSONNode !InteractionMask
	| TCInteract2				!TaskId !TaskTime !JSONNode !JSONNode !InteractionMask
	| TCProject					!TaskId !JSONNode !TaskTree
	| TCStep					!TaskId !TaskTime !(Either TaskTree (DeferredJSON,Int,TaskTree))
	| TCParallel				!TaskId !TaskTime
	| TCShared					!TaskId !TaskTime !TaskTree
	| TCExposedShared			!TaskId !TaskTime !String !TaskTree	// +URL
	| TCStable					!TaskId !TaskTime !DeferredJSON
	| TCNop			
	| TCDestroy					!TaskTree															//Marks a task state as garbage that must be destroyed
	| TCTasklet			

:: DeferredJSON
	= E. a:	DeferredJSON !a & TC a & JSONEncode{|*|} a
	|		DeferredJSONNode !JSONNode

derive JSONEncode DeferredJSON
derive JSONDecode DeferredJSON
	
:: TaskListEntry	=
	{ entryId			:: !TaskId					//Identification of entries in the list (for easy updating)
    , name              :: !Maybe String            //Optional name, for easy referencing
	, state				:: !TaskListEntryState		//Tree if embedded, or instance no if detached
	, lastEval          :: !TaskResult JSONNode     //Value of last evaluation
	, uiAttributes		:: !Map String String		//Stored attributes of last evaluation
	, createdAt			:: !TaskTime				//Time the entry was added to the set (used by layouts to highlight new items)
    , lastFocus         :: !Maybe TaskTime          //Time the entry was last explicitly focused
	, lastEvent			:: !TaskTime				//Last modified time
	, removed			:: !Bool					//Flag for marking this entry as 'removed', actual removal is done by the controlling parallel combinator
	}												//If it is false we have determined that this is not necessary during the last computation

:: TaskListEntryState
	= EmbeddedState 										    //An embedded task
	| DetachedState !InstanceNo !ProgressMeta !TaskAttributes	//A reference to the detached task (management and progress meta are cached copies)

