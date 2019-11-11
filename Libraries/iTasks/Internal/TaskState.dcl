definition module iTasks.Internal.TaskState

from iTasks.UI.Definition import :: UIChange
from iTasks.UI.Editor import :: Editor, :: EditState
from iTasks.UI.Editor.Generic import generic gEditor
from iTasks.UI.Layout import :: LUI, :: LUIMoves, :: LUIMoveID, :: LUINo, :: LUIEffectStage
from iTasks.Internal.IWorld import :: IWorld
from iTasks.Internal.Generic.Visualization import generic gText, :: TextFormat
from iTasks.Internal.TaskEval import :: TaskTime
from iTasks.SDS.Definition import :: SimpleSDSLens, :: SDSLens, :: SDSSequence
from iTasks.Util.DeferredJSON import :: DeferredJSON
from iTasks.WF.Definition import :: Task, :: TaskResult, :: TaskValue, :: TaskException, :: TaskNo, :: TaskId, :: TaskAttributes, :: TaskEvalOpts, :: Event
from iTasks.WF.Definition import :: InstanceNo, :: InstanceKey, :: InstanceProgress, :: InstanceConstants, :: ValueStatus
from iTasks.WF.Definition import class iTask
from iTasks.WF.Combinators.Core import :: AttachmentStatus
from iTasks.WF.Combinators.Core import :: TaskListFilter, :: TaskListItem

from Text.GenJSON import generic JSONEncode, generic JSONDecode, :: JSONNode
from Data.GenDefault import generic gDefault
from Data.Map import :: Map
from Data.Set import :: Set
from Data.Maybe import :: Maybe
from Data.Queue import :: Queue
from Data.GenEq import generic gEq
from Data.Error import :: MaybeError
from Data.Either import :: Either
from System.Time import :: Timestamp, :: Timespec
from System.File import :: FileError
from System.FilePath import :: FilePath

//FIXME: Extensions should not be imported in core
from iTasks.Extensions.Document import :: Document, :: DocumentId

derive JSONEncode TIMeta, TIType, TIReduct
derive JSONDecode TIMeta, TIType, TIReduct

//Persistent context of active tasks
//Split up version of task instance information

// This type is not actually used, because the parts are independent and accessed through different SDSs
// We include it to illustrate what state a task consists of.
/*
:: TaskState =
	{ task        :: Dynamic            // (Task a): The task, rewrites after each event
	, meta        :: TaskMeta           // Constant data, and management 
	, value       :: Value              // Last value
	, progress    :: TaskProgress       //  tasks -attributes and evaluation info
	, localshares :: Map TaskId Dynamic // Locally shared data (directly manipulated by subtasks)
	}
*/

:: TIMeta =
    //Static information
	{ taskId        :: !TaskId	            //Unique global identification
	, instanceType  :: !TIType              //There are 3 types of tasks: startup tasks, sessions, and persistent tasks
    , build         :: !String              //Application build version when the instance was created
    , createdAt     :: !Timespec
    //Evaluation information
	, valuestatus   :: !ValueStatus
    , attachedTo    :: ![TaskId]
	, instanceKey   :: !Maybe InstanceKey //* Random token that a client gets to have (temporary) access to the task instance
	, firstEvent    :: !Maybe Timespec    //* When was the first work done on this task
	, lastEvent     :: !Maybe Timespec    //* When was the latest event on this task (excluding Refresh events)
    //Identification and classification information
	, taskAttributes        :: !TaskAttributes  //Cached attributes from the task UI
	, managementAttributes  :: !TaskAttributes  //Arbitrary writable attributes for managing collections of task instances
	, unsyncedAttributes    :: !Set String      //When the `managementAttributes` are written they need to be synced to the UI on the next evaluation
	// Control information 
	, change               :: !Maybe TaskChange //Changes like removing or replacing a parallel task are only done when the
	, initialized          :: !Bool //TODO: Get rid of in this record
	}

:: ParallelTaskState =
	{ taskId               :: !TaskId                       //Identification
	, detached             :: !Bool
	, taskAttributes       :: !TaskAttributes            //Attributes that reflect the latest attributes from the task UI
	, managementAttributes :: !TaskAttributes            //Attributes that are explicitly written to the list through the tasklist
	, unsyncedAttributes   :: !Set String                //When the `managementAttributes` are written they need to be synced to the UI on the next evaluation
	, createdAt			   :: !TaskTime                  //Time the entry was added to the set (used by layouts to highlight new items)
	, lastEvent			   :: !TaskTime                  //Last modified time
	, change               :: !Maybe TaskChange          //Changes like removing or replacing a parallel task are only done when the
	                                                     //parallel is evaluated. This field is used to schedule such changes.
	, initialized       :: !Bool
	}

:: TaskChange
    = RemoveTask                            //Mark for removal from the set on the next evaluation
    | ReplaceTask !Dynamic                  //Replace the task on the next evaluation

:: TIType
	= TIStartup
	| TISession !InstanceKey
	| TIPersistent !InstanceKey !(Maybe TaskId)

:: TIReduct =
	{ task			:: !Task DeferredJSON               //Main task definition
	, nextTaskNo	:: !TaskNo                          //Local task number counter
	, nextTaskTime	:: !TaskTime                        //Local task time (incremented at every evaluation)
    // TODO Remove from reduct!
	, tasks			:: !Map TaskId Dynamic				//Task functions of embedded parallel tasks
	}

:: TIValue
   = TIValue !(TaskValue DeferredJSON)
   | TIException !Dynamic !String

derive gDefault TIMeta

:: InstanceFilter =
	{ //'Vertical' filters
	  onlyInstanceNo    :: !Maybe [TaskId]
	, notInstanceNo     :: !Maybe [TaskId]
	, includeSessions   :: !Bool
	, includeDetached   :: !Bool
	, includeStartup    :: !Bool
	, matchAttribute 	:: !Maybe (!String,!JSONNode)
	  //'Horizontal' filters
	, includeConstants  :: !Bool
	, includeProgress   :: !Bool
	, includeAttributes :: !Bool
	}
derive gDefault InstanceFilter
derive class iTask InstanceFilter

:: InstanceData :==
	( !TaskId
	, !Maybe InstanceConstants
	, !Maybe InstanceProgress
	, !Maybe (TaskAttributes,TaskAttributes) // fst are management attributes; snd are implicit task attributes
	)

mergeTaskAttributes :: !(!TaskAttributes,!TaskAttributes) -> TaskAttributes

//Fresh identifier generation
newInstanceNo           :: !*IWorld -> (!MaybeError TaskException InstanceNo,!*IWorld)
newInstanceKey          :: !*IWorld -> (!InstanceKey,!*IWorld)

//=== Task instance index: ===

//A global index of all task instances is maintained

//This counter is used to ensure unique instance numbers
nextInstanceNo :: SimpleSDSLens Int

//This index contains all meta-data about the task instances on this engine
taskInstanceIndex :: SimpleSDSLens [TIMeta]

//Task instance state is accessible as shared data sources
filteredInstanceIndex   :: SDSLens InstanceFilter [InstanceData] [InstanceData]

//Filtered views on the instance index
taskInstance            :: SDSLens InstanceNo InstanceData InstanceData
taskInstanceConstants   :: SDSLens InstanceNo InstanceConstants ()
taskInstanceProgress    :: SDSLens InstanceNo InstanceProgress InstanceProgress

//* fst are management attributes, snd are implicit task attributes 
taskInstanceAttributes :: SDSLens InstanceNo (TaskAttributes,TaskAttributes) (TaskAttributes,TaskAttributes)

// === Evaluation state of instances: ===
taskInstanceReduct            :: SDSLens InstanceNo (Maybe TIReduct) (Maybe TIReduct)
taskInstanceValue             :: SDSLens InstanceNo (Maybe TIValue) (Maybe TIValue)
taskInstanceShares            :: SDSLens InstanceNo (Maybe (Map TaskId DeferredJSON)) (Maybe (Map TaskId DeferredJSON))

taskInstanceParallelTaskLists :: SDSLens InstanceNo (Maybe (Map TaskId [ParallelTaskState])) (Maybe (Map TaskId [ParallelTaskState]))
taskInstanceParallelValues  :: SDSLens InstanceNo (Maybe (Map TaskId (Map TaskId (TaskValue DeferredJSON)))) (Maybe (Map TaskId (Map TaskId (TaskValue DeferredJSON))))

topLevelTaskList        :: SDSLens TaskListFilter (!TaskId,![TaskListItem a]) [(TaskId,TaskAttributes)]

taskInstanceIO 			:: SDSLens InstanceNo (Maybe (!String,!Timespec)) (Maybe (!String,!Timespec))
allInstanceIO           :: SimpleSDSLens (Map InstanceNo (!String,Timespec))

//Filtered views on evaluation state of instances:

//Shared source
localShare              			:: SDSLens TaskId a a | iTask a

//Core parallel task list state structure
taskInstanceParallelTaskList        :: SDSLens (TaskId,TaskListFilter) [ParallelTaskState] [ParallelTaskState]
taskInstanceParallelTaskListValues  :: SDSLens (TaskId,TaskListFilter) (Map TaskId (TaskValue DeferredJSON)) (Map TaskId (TaskValue DeferredJSON)) 

//Private interface used during evaluation of parallel combinator
taskInstanceParallelTaskListItem    :: SDSLens (TaskId,TaskId) ParallelTaskState ParallelTaskState
taskInstanceParallelTaskListValue   :: SDSLens (TaskId,TaskId) (TaskValue DeferredJSON) (TaskValue DeferredJSON) 

taskInstanceEmbeddedTask            :: SDSLens TaskId (Task a) (Task a) | iTask a

//Public interface used by parallel tasks
parallelTaskList                    :: SDSSequence (!TaskId,!TaskId,!TaskListFilter) (!TaskId,![TaskListItem a]) [(TaskId,TaskAttributes)] | iTask a

//=== Access functions: ===

createClientTaskInstance :: !(Task a) !String !InstanceNo !*IWorld -> *(!MaybeError TaskException TaskId, !*IWorld) |  iTask a

createStartupTaskInstance :: !(Task a) !TaskAttributes !*IWorld -> (!MaybeError TaskException InstanceNo, !*IWorld) | iTask a

createSessionTaskInstance :: !(Task a) !TaskAttributes !*IWorld -> (!MaybeError TaskException (!InstanceNo,InstanceKey),!*IWorld) | iTask a

/**
* Create a stored task instance in the task store (lazily without evaluating it)
* @param The task to store
* @param Whether it is a top-level task
* @param The task evaluation options
* @param The instance number for the task
* @param Management meta data
* @param The parallel task list to which the task belongs
* @param If the instance needs to be evaluated immediately, the attachment is temporarily set to the issuer
* @param The IWorld state
*
* @return The task id of the stored instance
* @return The IWorld state
*/
createDetachedTaskInstance :: !(Task a) !Bool !TaskEvalOpts !InstanceNo !TaskAttributes !TaskId !Bool !*IWorld -> (!MaybeError TaskException TaskId, !*IWorld) | iTask a

/**
* Replace a stored task instance in the task store.
* The execution state is reset, but the meta-data is kept.
* @param The instance id
* @param The new task to store
*
* @param The IWorld state
*/
replaceTaskInstance :: !InstanceNo !(Task a) *IWorld -> (!MaybeError TaskException (), !*IWorld) | iTask a

deleteTaskInstance	:: !InstanceNo !*IWorld -> *(!MaybeError TaskException (), !*IWorld)

//FIXME: Documents should not be part of the core server
newDocumentId			:: !*IWorld -> (!DocumentId, !*IWorld)
createDocument 			:: !String !String !String !*IWorld -> (!MaybeError FileError Document, !*IWorld)
loadDocumentContent		:: !DocumentId !*IWorld -> (!Maybe String, !*IWorld)
loadDocumentMeta		:: !DocumentId !*IWorld -> (!Maybe Document, !*IWorld)
documentLocation		:: !DocumentId !*IWorld -> (!FilePath,!*IWorld)

