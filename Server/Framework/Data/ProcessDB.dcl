definition module ProcessDB
/**
* This module provides a database for storing persistent task instances.
* It contains two types of task instances:
* Session instances: temporary tasks for each interactive session between a user and the server. 
* Workflow instances: persistent long-running tasks that may be shared between users and exist between sessions.
*/
import Maybe, SystemTypes, Task, TaskContext
from Time import :: Timestamp

derive JSONEncode	Process
derive JSONDecode	Process
derive gEq			Process

/**
* Our local process type
*/
:: Process 		= {	processId		:: !ProcessId					// The process identification				  
				  , properties		:: !ProcessProperties			// The properties of the main task node of this process
				  , subprocesses	:: ![Process]					// The sub processes of this process
				  }

class ProcessDB st
where
	getNewSessionId			::														!*st -> (!ProcessId,		!*st)
	getNewWorkflowId		::														!*st -> (!ProcessId,		!*st)

	getProcess				:: !ProcessId											!*st -> (!Maybe Process,	!*st)
	getProcessContext		:: !ProcessId											!*st -> (!Maybe TaskContext,!*st)
	
	getProcessForUser		:: !User !ProcessId										!*st -> (!Maybe Process,	!*st)
	getProcesses 			:: ![TaskStatus] ![RunningTaskStatus]					!*st -> (![Process], 		!*st)
	getProcessesById		:: ![ProcessId]											!*st -> (![Process],		!*st)
	getProcessesForUser		:: !User ![TaskStatus] ![RunningTaskStatus]				!*st -> (![Process],		!*st)
	
	setProcessContext		:: !ProcessId !TaskContext								!*st -> *st

	deleteProcess			:: !ProcessId											!*st -> *st
	
	//Gets the timestamp of the last change of the workflow database.
	lastChange				::														!*st -> (!Timestamp,!*st)
	
instance ProcessDB IWorld