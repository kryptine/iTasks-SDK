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
from iTasks.WF.Definition import :: InstanceNo, :: InstanceKey
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

derive JSONEncode TaskMeta, ExtendedTaskListFilter
derive JSONDecode TaskMeta, ExtendedTaskListFilter

derive gDefault TaskMeta, ExtendedTaskListFilter
derive gText ExtendedTaskListFilter

//Persistent context of active tasks
//Split up version of task instance information

// This type is not actually used, because the parts are independent and accessed through different SDSs
// We include it to illustrate what state a task consists of.
/*
:: TaskState =
	{ task        :: Task a             // (Task a): The task, rewrites after each event
	, meta        :: TaskMeta           // Constant data, and management 
	, value       :: Value              // Last value
	, localshares :: Map TaskId Dynamic // Locally shared data (directly manipulated by subtasks)
	}
*/

:: TaskMeta =
    //Static information
	{ taskId        :: !TaskId	            //Unique global identification
	, instanceType  :: !InstanceType        //There are 3 types of tasks: startup tasks, sessions, and persistent tasks
    , build         :: !String              //* Application build version when the instance was created
    , createdAt     :: !Timespec
    //Evaluation information
	, status        :: !Either String Bool  //* Exception message, or stability
	, nextTaskNo	:: !TaskNo              //* Local task number counter
	, nextTaskTime	:: !TaskTime            //* Local task time (incremented at every evaluation)
    , attachedTo    :: ![TaskId]
	, connectedTo   :: !Maybe String 
	, instanceKey   :: !Maybe InstanceKey //* Random token that a client gets to have (temporary) access to the task instance
	, firstEvent    :: !Maybe Timespec    //* When was the first work done on this task
	, lastEvent     :: !Maybe Timespec    //* When was the latest event on this task (excluding Refresh events)
	, lastIO        :: !Maybe Timespec
    //Identification and classification information
	, taskAttributes        :: !TaskAttributes  //Cached attributes from the task UI
	, managementAttributes  :: !TaskAttributes  //Arbitrary writable attributes for managing collections of task instances
	, unsyncedAttributes    :: !Set String      //When the `managementAttributes` are written they need to be synced to the UI on the next evaluation
	// Control information 
	, change               :: !Maybe TaskChange //Changes like removing or replacing a parallel task are only done when the
	, initialized          :: !Bool //TODO: Get rid of in this record
	}

/**
* There are three types of task instances:
* Startup instances: temporary tasks that are started when a task server starts up, typically driven by a clock or external I/O.
* Session instances: temporary tasks that represent and facilitate interactive sessions between a user and the server.
* Persistent instances: persistent long-running tasks that may be shared between users and exist between sessions.
*/
:: InstanceType
	= StartupInstance
	| SessionInstance
	| PersistentInstance !(Maybe TaskId) //* If the task is a sub-task a detached part of another instance

:: TaskChange
    = RemoveTask                            //Mark for removal from the set on the next evaluation
    | ReplaceTask !Dynamic                  //Replace the task on the next evaluation


//Internally we need more options to filter task list data
:: ExtendedTaskListFilter =
	//Extra filter on task type
	{ includeSessions   :: !Bool
	, includeDetached   :: !Bool
	, includeStartup    :: !Bool
	//Extra horizontal filtering options
	, includeTaskReduct :: !Bool
	, includeTaskIO     :: !Bool
	}

//Predefined filters
fullExtendedTaskListFilter :: ExtendedTaskListFilter

//Fresh identifier generation
newInstanceNo           :: !*IWorld -> (!MaybeError TaskException InstanceNo,!*IWorld)
newInstanceKey          :: !*IWorld -> (!InstanceKey,!*IWorld)

//=== Core task state  === //

//This counter is used to ensure unique instance numbers
nextInstanceNo :: SimpleSDSLens Int

//All Task state is accessible as shared data sources
//taskListData :: SDSLens (!TaskId,!TaskId,!TaskListFilter,!ExtendedTaskListFilter) (!TaskId, [TaskMeta], Map TaskId (TaskValue a), Task a) | iTask a
taskListMetaData :: SDSLens (!TaskId,!TaskId,!TaskListFilter,!ExtendedTaskListFilter) (!TaskId,![TaskMeta]) [TaskMeta]

taskListDynamicValueData :: SDSLens (!TaskId,!TaskId,!TaskListFilter,!ExtendedTaskListFilter) (Map TaskId (TaskValue DeferredJSON)) (Map TaskId (TaskValue DeferredJSON))
taskListTypedValueData :: SDSLens (!TaskId,!TaskId,!TaskListFilter,!ExtendedTaskListFilter) (Map TaskId (TaskValue a)) (Map TaskId (TaskValue a)) | iTask a

taskListDynamicTaskData :: SDSLens (!TaskId,!TaskId,!TaskListFilter,!ExtendedTaskListFilter) (Map TaskId (Task DeferredJSON)) (Map TaskId (Task DeferredJSON))
taskListTypedTaskData :: SDSLens (!TaskId,!TaskId,!TaskListFilter,!ExtendedTaskListFilter) (Map TaskId (Task a)) (Map TaskId (Task a)) | iTask a

//== Filtered views on the task state for different purposes ==//

//Interface used during evaluation of parallel combinator
taskInstanceParallelTaskList       :: SDSLens (TaskId,TaskListFilter) (TaskId,[TaskMeta]) [TaskMeta]
taskInstanceParallelTaskListValues :: SDSLens (TaskId,TaskListFilter) (Map TaskId (TaskValue a)) (Map TaskId (TaskValue a)) | iTask a
taskInstanceParallelTaskListTasks  :: SDSLens (TaskId,TaskListFilter) (Map TaskId (Task a)) (Map TaskId (Task a)) | iTask a

taskInstanceParallelTaskListItem    :: SDSLens (TaskId,TaskId,Bool) TaskMeta TaskMeta
taskInstanceParallelTaskListValue   :: SDSLens (TaskId,TaskId) (TaskValue a) (TaskValue a) | iTask a
taskInstanceParallelTaskListTask    :: SDSLens (TaskId,TaskId) (Task a) (Task a) | iTask a

//Interface used during the evalation of toplevel tasks
//Filtered views on the instance index
taskInstance            :: SDSLens (InstanceNo,Bool,Bool,Bool) TaskMeta TaskMeta

taskInstanceAttributes  :: SDSLens InstanceNo (TaskAttributes,TaskAttributes) (TaskAttributes,TaskAttributes)

taskInstanceValue       :: SDSLens InstanceNo (TaskValue DeferredJSON) (TaskValue DeferredJSON) 
taskInstanceTask        :: SDSLens InstanceNo (Task DeferredJSON) (Task DeferredJSON)

//Locally shared data
taskInstanceShares      :: SDSLens InstanceNo (Maybe (Map TaskId DeferredJSON)) (Maybe (Map TaskId DeferredJSON))

//Public interface used by parallel tasks
parallelTaskList :: SDSLens (!TaskId,!TaskId,!TaskListFilter) (!TaskId,![TaskListItem a]) [(TaskId,TaskAttributes)] | iTask a
topLevelTaskList :: SDSLens TaskListFilter (!TaskId,![TaskListItem a]) [(TaskId,TaskAttributes)] | iTask a

localShare :: SDSLens TaskId a a | iTask a

//Conversion to task lists
toTaskListItem :: !TaskId !TaskMeta -> TaskListItem a

//=== Access functions: ===

createClientTaskInstance :: !(Task a) !String !InstanceNo !*IWorld -> *(!MaybeError TaskException TaskId, !*IWorld) |  iTask a

createStartupTaskInstance :: !(Task a) !TaskAttributes !*IWorld -> (!MaybeError TaskException InstanceNo, !*IWorld) | iTask a

createSessionTaskInstance :: !(Task a) !TaskAttributes !*IWorld -> (!MaybeError TaskException (!InstanceNo,InstanceKey),!*IWorld) | iTask a

/**
* Create a stored task instance in the task store (lazily without evaluating it)
* @param The task to store
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
createDetachedTaskInstance :: !(Task a) !TaskEvalOpts !InstanceNo !TaskAttributes !TaskId !Bool !*IWorld -> (!MaybeError TaskException TaskId, !*IWorld) | iTask a

/**
* Replace a stored task instance in the task store.
* The execution state is reset, but the meta-data is kept.
* @param The instance id
* @param The new task to store
*
* @param The IWorld state
*/
replaceTaskInstance :: !InstanceNo !(Task a) *IWorld -> (!MaybeError TaskException (), !*IWorld) | iTask a

deleteTaskInstance :: !InstanceNo !*IWorld -> *(!MaybeError TaskException (), !*IWorld)

//Update the I/O information for task instances
updateInstanceConnect :: !String ![InstanceNo] !*IWorld -> *(!MaybeError TaskException (), !*IWorld)
updateInstanceLastIO :: ![InstanceNo] !*IWorld -> *(!MaybeError TaskException (), !*IWorld)
updateInstanceDisconnect :: ![InstanceNo] !*IWorld -> *(!MaybeError TaskException (), !*IWorld)

//FIXME: Documents should not be part of the core server
newDocumentId			:: !*IWorld -> (!DocumentId, !*IWorld)
createDocument 			:: !String !String !String !*IWorld -> (!MaybeError FileError Document, !*IWorld)
loadDocumentContent		:: !DocumentId !*IWorld -> (!Maybe String, !*IWorld)
loadDocumentMeta		:: !DocumentId !*IWorld -> (!Maybe Document, !*IWorld)
documentLocation		:: !DocumentId !*IWorld -> (!FilePath,!*IWorld)

