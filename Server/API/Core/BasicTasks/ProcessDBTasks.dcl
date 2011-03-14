definition module ProcessDBTasks
/**
* This module provides access to the process database
*/
import Maybe, Types
from TSt		import :: Task
from ProcessDB	import :: Process(..)
from TaskTree	import :: TaskParallelType
from Time		import :: Timestamp 
from Shared		import :: ReadOnlyShared
import iTaskClass

derive gVisualize	Process, TaskParallelType, ProcessProperties, TaskProgress, SystemProperties, ManagerProperties, TaskProperties, TaskStatus, TaskPriority, TaskDescription
derive gUpdate		Process, TaskParallelType, ProcessProperties, TaskProgress, SystemProperties, ManagerProperties, TaskProperties, TaskStatus, TaskPriority, TaskDescription
derive gDefaultMask	Process, TaskParallelType, ProcessProperties, TaskProgress, SystemProperties, ManagerProperties, TaskProperties, TaskStatus, TaskPriority, TaskDescription
derive gVerify		Process, TaskParallelType, ProcessProperties, TaskProgress, SystemProperties, ManagerProperties, TaskProperties, TaskStatus, TaskPriority, TaskDescription

/**
* Retrieves a Process record from the process table
* 
* @param The process id
*
* @return When found, the Process record. Nothing when the process cannot be found.
*/
getProcess				:: !ProcessId				-> Task (Maybe Process)
/**
* Retrieves a Process record with an additional check on the process owner. Only
* when the process is owned by the indicated user it will be returned.
*
* @param The owner of the indicated process
* @param The process id
*
* @return When found, the Process record. Nothing when the process cannot be found.
*/
getProcessForUser		:: !User !ProcessId			-> Task (Maybe Process)
/**
* Retrieves a Process record with an additional check on the process manager. Only
* when the process is managed by the indicated user it will be returned.
*
* @param The manager of the indicated process
* @param The process id
*
* @return When found, the Process record. Nothing when the process cannot be found.
*/
getProcessForManager 	:: !User !ProcessId			-> Task (Maybe Process)
/**
* Retrieves the processes with indicated process ids
*
* @param A list of process ids to match on
* 
* @return The list of found processes
*/
getProcesses			:: ![ProcessId]				-> Task [Process]
/**
* Retrieves all process that have one of the given statuses
*
* @param A list of statuses to match on
* 
* @return The list of processes having the given statuses
*/
getProcessesWithStatus	:: ![TaskStatus]			-> Task [Process]
/**
* Retrieves the processes that are owned by indicated user and have one of the
* given statuses.
*
* @param A process owner to match on
* @param A list of statuses to match on
* @param Ignore embedded processes
*
* @return The list of found processes
*/
getProcessesForUser		:: !User ![TaskStatus]		-> Task [Process]
/**
* Poll who is the owner of a process.
*
* @param The process reference
*
* @return A task that yields the owner if the referenced process is not deleted
*/
getProcessOwner 		:: !ProcessId 				-> Task (Maybe User)
/**
* Changes the owner of the indicated process. The current user is automatically set
* as delegator of the process.
*
* @param The new process owner
* @param The process id
*/
setProcessOwner			:: !User !ProcessId			-> Task Void
/**
* Poll the status of a process.
*
* @param The process reference
*
* @return A task that yields the status of the referenced process
*/
getProcessStatus		:: !ProcessId				-> Task TaskStatus
/**
* Change the process status to Active
*
* @param The process reference
*
* @return A task that yields True when the process was successfully activated
*         and False when the process could not be found.
*/
activateProcess			:: !ProcessId				-> Task Void
/**
* Change the process status to suspended.
* The tasks within this process will be inaccessible until the process is activated again.
*
* @param The process reference
*/
suspendProcess			:: !ProcessId				-> Task Void
/**
* Delete a process from the process database
* Once a process is deleted all of its results are lost.
*
* @param The process reference
*/
deleteProcess			:: !ProcessId				-> Task Void
