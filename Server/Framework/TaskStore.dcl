definition module TaskStore
/**
* This module provides storage of task instances
* It contains two types of task instances:
* Session instances: temporary tasks for each interactive session between a user and the server. 
* Workflow instances: persistent long-running tasks that may be shared between users and exist between sessions.
*/
import Maybe, Error, SystemTypes, Task, TaskContext, TUIDefinition
from Time import :: Timestamp

newSessionId		:: !*IWorld -> (!SessionId,	!*IWorld)
newTopNo			:: !*IWorld -> (!TopNo,	!*IWorld)

storeTaskInstance	:: !TaskContext !*IWorld -> *IWorld
loadTaskInstance	:: !(Either SessionId TopNo) !*IWorld -> (!MaybeErrorString TaskContext, !*IWorld)
deleteTaskInstance	:: !(Either SessionId TopNo) !*IWorld -> *IWorld

storeTaskTUI		:: !SessionId !TUIDef !Int !*IWorld -> *IWorld
loadTaskTUI			:: !SessionId !*IWorld -> (!MaybeErrorString (!TUIDef,!Int), !*IWorld)
