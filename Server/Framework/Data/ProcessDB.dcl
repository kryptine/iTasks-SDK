definition module ProcessDB
/**
* This module provides a database for storing persistent task instances.
* It contains two types of task instances:
* Session instances: temporary tasks for each interactive session between a user and the server. 
* Workflow instances: persistent long-running tasks that may be shared between users and exist between sessions.
*/
import Maybe, SystemTypes, Task, TaskContext
from Time import :: Timestamp

class ProcessDB st
where
	getNewSessionId			::														!*st -> (!ProcessId,		!*st)
	getNewWorkflowId		::														!*st -> (!ProcessId,		!*st)

	getProcess				:: !ProcessId											!*st -> (!Maybe TaskInstanceMeta,	!*st)
	getProcessContext		:: !ProcessId											!*st -> (!Maybe TaskContext,!*st)
	
	getProcessForUser		:: !User !ProcessId										!*st -> (!Maybe TaskInstanceMeta,	!*st)
	getProcesses 			:: ![TaskStatus]										!*st -> (![TaskInstanceMeta], 		!*st)
	getProcessesById		:: ![ProcessId]											!*st -> (![TaskInstanceMeta],		!*st)
	getProcessesForUser		:: !User ![TaskStatus] 									!*st -> (![TaskInstanceMeta],		!*st)
	
	setProcessContext		:: !ProcessId !TaskContext								!*st -> *st

	deleteProcess			:: !ProcessId											!*st -> *st
	
	//Gets the timestamp of the last change of the workflow database.
	lastChange				::														!*st -> (!Timestamp,!*st)
	
instance ProcessDB IWorld