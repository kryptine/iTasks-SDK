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
    { instanceNo    :: Maybe InstanceNo
    , session       :: Maybe Bool
    }

newInstanceNo			:: !*IWorld -> (!InstanceNo, !*IWorld)
maxInstanceNo			:: !*IWorld -> (!InstanceNo, !*IWorld)
newInstanceKey          :: !*IWorld -> (!InstanceKey,!*IWorld)
newDocumentId			:: !*IWorld -> (!DocumentId, !*IWorld)

//Create and delete task instances
deleteInstance			:: !InstanceNo !*IWorld -> *IWorld

//Rebuild the task instance index in the iworld from the store content
initInstanceMeta        :: !*IWorld -> *IWorld

//Task instance state is accessible as shared data sources
//fullInstanceMeta        :: RWShared Void [TIMeta] [TIMeta]
filteredInstanceMeta    :: RWShared InstanceFilter [TIMeta] [TIMeta]

taskInstanceMeta        :: RWShared InstanceNo TIMeta TIMeta
taskInstanceReduct		:: !InstanceNo -> RWShared Void TIReduct TIReduct
taskInstanceValue       :: !InstanceNo -> RWShared Void TIValue TIValue
taskInstanceRep         :: !InstanceNo -> RWShared Void TaskRep TaskRep

//Documents
createDocument 			:: !String !String !String !*IWorld -> (!MaybeError FileError Document, !*IWorld)
loadDocumentContent		:: !DocumentId !*IWorld -> (!Maybe String, !*IWorld)
loadDocumentMeta		:: !DocumentId !*IWorld -> (!Maybe Document, !*IWorld)
documentLocation		:: !DocumentId !*IWorld -> (!FilePath,!*IWorld)

