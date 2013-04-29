definition module TaskStore
/**
* This module provides storage of task instances
* It contains two types of task instances:
* Session instances: temporary tasks for each interactive session between a user and the server. 
* Workflow instances: persistent long-running tasks that may be shared between users and exist between sessions.
*/
import Maybe, Error, SystemTypes, Task, TaskState, UIDefinition

from Time				import :: Timestamp
from SharedDataSource	import :: BasicShareId, :: RWShared

newSessionId			:: !*IWorld -> (!SessionId,	!*IWorld)
newInstanceNo			:: !*IWorld -> (!InstanceNo, !*IWorld)
maxInstanceNo			:: !*IWorld -> (!InstanceNo, !*IWorld)
newDocumentId			:: !*IWorld -> (!DocumentId, !*IWorld)

//Create and delete task instances
deleteInstance			:: !InstanceNo !*IWorld -> *IWorld

//Task instance state is accessible as shared data sources
taskInstances			:: RWShared (Map InstanceNo TIMeta) (Map InstanceNo TIMeta) IWorld //The master index of available instances

taskInstanceMeta		:: !InstanceNo -> RWShared TIMeta TIMeta IWorld
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

//Keep last version of session user interfaces around, to be able to send differences to client
storeCurUI				:: !SessionId !Int !UIDef !*IWorld -> *IWorld
loadPrevUI				:: !SessionId !Int !*IWorld -> (!Maybe UIDef, !*IWorld)

//Sync previous user interfaces to disk (Only used with CGI wrapper)
saveUICache					:: !*IWorld -> *IWorld
restoreUICache				:: !*IWorld -> *IWorld
