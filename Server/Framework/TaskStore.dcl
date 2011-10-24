definition module TaskStore
/**
* This module provides storage of task instances
* It contains two types of task instances:
* Session instances: temporary tasks for each interactive session between a user and the server. 
* Workflow instances: persistent long-running tasks that may be shared between users and exist between sessions.
*/
import Maybe, Error, SystemTypes, Task, TaskContext, TUIDefinition
from Time import :: Timestamp

newSessionId		:: !*IWorld -> (!ProcessId,	!*IWorld)
newWorkflowId		:: !*IWorld -> (!ProcessId,	!*IWorld)

storeTaskInstance	:: !TaskContext !*IWorld -> *IWorld
loadTaskInstance	:: !ProcessId !*IWorld -> (!MaybeErrorString TaskContext, !*IWorld)

storeTaskTUI		:: !ProcessId !TUIDef !*IWorld -> *IWorld
loadTaskTUI			:: !ProcessId !*IWorld -> (!MaybeErrorString (!TUIDef,!Timestamp), !*IWorld)