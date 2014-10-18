definition module iTasks.Framework.TaskStore
/**
* This module provides storage of task instances
* It contains two types of task instances:
* Session instances: temporary tasks for each interactive session between a user and the server. 
* Workflow instances: persistent long-running tasks that may be shared between users and exist between sessions.
*/
import iTasks.Framework.Task, iTasks.Framework.TaskState, iTasks.Framework.UIDefinition, iTasks.Framework.SDS
import iTasks.API.Core.Types

from Data.Maybe     import :: Maybe
from Data.Error     import :: MaybeError
from System.Time    import :: Timestamp

:: InstanceFilter =
    { //'Vertical' filters
      onlyInstanceNo    :: !Maybe [InstanceNo]
    , onlySession       :: !Maybe Bool
      //'Horizontal' filters
    , includeConstants  :: !Bool
    , includeProgress   :: !Bool
    , includeAttributes :: !Bool
    }
:: InstanceData :== (!InstanceNo,!Maybe InstanceConstants,!Maybe InstanceProgress,!Maybe TaskAttributes)

derive class iTask InstanceFilter

//Fresh identifier generation
newInstanceNo           :: !*IWorld -> (!MaybeError TaskException InstanceNo,!*IWorld)
newInstanceKey          :: !*IWorld -> (!InstanceKey,!*IWorld)
newDocumentId			:: !*IWorld -> (!DocumentId, !*IWorld)

//Task instance state is accessible as shared data sources
filteredInstanceIndex   :: RWShared InstanceFilter [InstanceData] [InstanceData]

//Filtered views on the instance index
taskInstance            :: RWShared InstanceNo InstanceData InstanceData
taskInstanceConstants   :: ROShared InstanceNo InstanceConstants
taskInstanceProgress    :: RWShared InstanceNo InstanceProgress InstanceProgress
taskInstanceAttributes  :: RWShared InstanceNo TaskAttributes TaskAttributes

topLevelTaskList        :: RWShared TaskListFilter (!TaskId,![TaskListItem a]) [(!TaskId,!TaskAttributes)]

//Evaluation state of instances
taskInstanceReduct		:: RWShared InstanceNo TIReduct TIReduct
taskInstanceValue       :: RWShared InstanceNo TIValue TIValue
taskInstanceRep         :: RWShared InstanceNo TaskRep TaskRep
taskInstanceShares      :: RWShared InstanceNo (Map TaskId JSONNode) (Map TaskId JSONNode)

//Filtered views on evaluation state of instances
localShare              :: RWShared TaskId a a | iTask a

//Core parallel task list state structure
taskInstanceParallelTaskList        :: RWShared (TaskId,TaskListFilter) [ParallelTaskState] [ParallelTaskState]
//Private interface used during evaluation of parallel combinator
taskInstanceParallelTaskListItem    :: RWShared (TaskId,TaskId,Bool) ParallelTaskState ParallelTaskState

taskInstanceEmbeddedTask            :: RWShared TaskId (Task a) (Task a) | iTask a

//Public interface used by parallel tasks
parallelTaskList                    :: RWShared (!TaskId,!TaskId,!TaskListFilter) (!TaskId,![TaskListItem a]) [(!TaskId,!TaskAttributes)] | iTask a

//Access to remote shared data
exposedShare 	        :: !String -> 	RWShared p r w	    | iTask r & iTask w & TC r & TC w & TC p & JSONEncode{|*|} p

//Create and delete task instances

createClientTaskInstance :: !(Task a) !SessionId !InstanceNo !*IWorld -> *(!TaskId, !*IWorld) |  iTask a

//Create a task instance
createTaskInstance :: !(Task a) !*IWorld -> (!MaybeError TaskException (!InstanceNo,InstanceKey),!*IWorld) | iTask a

/**
* Create a stored task instance in the task store (lazily without evaluating it)
* @param The task to store
* @param The instance number for the task
* @param Management meta data
* @param The user who issued the task
* @param The parallel task list to which the task belongs
* @param If the instance needs to be evaluated immediately, the attachment is temporarily set to the issuer
* @param The IWorld state
*
* @return The task id of the stored instance
* @return The IWorld state
*/
createDetachedTaskInstance :: !(Task a) !InstanceNo !TaskAttributes !User !TaskId !Bool !*IWorld -> (!TaskId, !*IWorld) | iTask a

/**
* Replace a stored task instance in the task store.
* The execution state is reset, but the meta-data is kept.
* @param The instance id
* @param The new task to store
*
* @param The IWorld state
*/
replaceTaskInstance :: !InstanceNo !(Task a) *IWorld -> (!MaybeErrorString (), !*IWorld) | iTask a

deleteTaskInstance		:: !InstanceNo !*IWorld -> *IWorld

//Documents
createDocument 			:: !String !String !String !*IWorld -> (!MaybeError FileError Document, !*IWorld)
loadDocumentContent		:: !DocumentId !*IWorld -> (!Maybe String, !*IWorld)
loadDocumentMeta		:: !DocumentId !*IWorld -> (!Maybe Document, !*IWorld)
documentLocation		:: !DocumentId !*IWorld -> (!FilePath,!*IWorld)

