definition module TaskStore
/**
* This module provides storage of task instances
* It contains two types of task instances:
* Session instances: temporary tasks for each interactive session between a user and the server. 
* Workflow instances: persistent long-running tasks that may be shared between users and exist between sessions.
*/
import Maybe, Error, SystemTypes, Task, TaskState, TUIDefinition
from Time import :: Timestamp


newSessionId		:: !*IWorld -> (!SessionId,	!*IWorld)
newInstanceId		:: !*IWorld -> (!InstanceNo, !*IWorld)

storeTaskInstance	:: !TaskInstance !*IWorld -> *IWorld

loadTaskInstance	:: !InstanceNo !*IWorld -> (!MaybeErrorString TaskInstance, !*IWorld)
loadSessionInstance	:: !SessionId !*IWorld -> (!MaybeErrorString TaskInstance, !*IWorld)
deleteTaskInstance	:: !InstanceNo !*IWorld -> *IWorld

storeTaskTUI		:: !SessionId !TUIDef !Int !*IWorld -> *IWorld
loadTaskTUI			:: !SessionId !*IWorld -> (!MaybeErrorString (!TUIDef,!Int), !*IWorld)
