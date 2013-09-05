definition module iTasks.Framework.TaskStore
/**
* This module provides storage of task instances
* It contains two types of task instances:
* Session instances: temporary tasks for each interactive session between a user and the server. 
* Workflow instances: persistent long-running tasks that may be shared between users and exist between sessions.
*/
import iTasks.Framework.Task, iTasks.Framework.TaskState, iTasks.Framework.UIDefinition
import iTasks.API.Core.SystemTypes

from Data.Maybe import :: Maybe
from Data.Error import :: MaybeError
from System.Time			import :: Timestamp
from Data.SharedDataSource	import :: BasicShareId, :: RWShared

newSessionId			:: !*IWorld -> (!SessionId,	!*IWorld)
newInstanceNo			:: !*IWorld -> (!InstanceNo, !*IWorld)
maxInstanceNo			:: !*IWorld -> (!InstanceNo, !*IWorld)
newDocumentId			:: !*IWorld -> (!DocumentId, !*IWorld)

//Create and delete task instances
deleteInstance			:: !InstanceNo !*IWorld -> *IWorld

//Task instance state is accessible as shared data sources
detachedInstances		:: RWShared (Map InstanceNo TIMeta) (Map InstanceNo TIMeta) IWorld //The master index of detached instances
sessionInstances        :: RWShared (Map InstanceNo TIMeta) (Map InstanceNo TIMeta) IWorld //The master index of session instances

detachedInstanceMeta	:: !InstanceNo -> RWShared TIMeta TIMeta IWorld
sessionInstanceMeta		:: !InstanceNo -> RWShared TIMeta TIMeta IWorld
taskInstanceReduct		:: !InstanceNo -> RWShared TIReduct TIReduct IWorld
taskInstanceResult		:: !InstanceNo -> RWShared (TaskResult JSONNode) (TaskResult JSONNode) IWorld

//Documents
createDocument 			:: !String !String !String !*IWorld -> (!MaybeError FileError Document, !*IWorld)
createDocumentWith		:: !String !String (*File -> *File) !*IWorld -> (!MaybeError FileError Document, !*IWorld)
loadDocumentContent		:: !DocumentId !*IWorld -> (!Maybe String, !*IWorld)
loadDocumentMeta		:: !DocumentId !*IWorld -> (!Maybe Document, !*IWorld)
documentLocation		:: !DocumentId !*IWorld -> (!FilePath,!*IWorld)

//Keep track of outdated task instances that need to be refreshed
addShareRegistration		:: !BasicShareId !InstanceNo !*IWorld -> *IWorld
clearShareRegistrations		:: !InstanceNo !*IWorld -> *IWorld
//Queue evaluation when shares change
addOutdatedOnShareChange	:: !BasicShareId !(InstanceNo -> Bool) !*IWorld -> *IWorld
addOutdatedInstances		:: ![(!InstanceNo, !Maybe Timestamp)] !*IWorld -> *IWorld

