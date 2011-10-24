definition module ProcessDBTasks
/**
* This module provides access to the process database
*/
import Maybe, SystemTypes
from Task		import :: Task
from Time		import :: Timestamp 
import iTaskClass

derive gVisualizeText	ProcessId, TaskInstanceMeta, ProgressMeta, TaskMeta, TaskStatus
derive gVisualizeHtml	ProcessId, TaskInstanceMeta, ProgressMeta, TaskMeta, TaskStatus
derive gVisualizeEditor	ProcessId, TaskInstanceMeta, ProgressMeta, TaskMeta, TaskStatus
derive gUpdate			ProcessId, TaskInstanceMeta, ProgressMeta, TaskMeta, TaskStatus
derive gDefaultMask		ProcessId, TaskInstanceMeta, ProgressMeta, TaskMeta, TaskStatus
derive gVerify			ProcessId, TaskInstanceMeta, ProgressMeta, TaskMeta, TaskStatus

/**
* Retrieves a Process record from the process table
* 
* @param The process id
*
* @return When found, the Process record. Nothing when the process cannot be found.
*/
getProcess				:: !ProcessId				-> Task (Maybe TaskInstanceMeta)
/**
* Retrieves a Process record with an additional check on the process owner. Only
* when the process is owned by the indicated user it will be returned.
*
* @param The owner of the indicated process
* @param The process id
*
* @return When found, the Process record. Nothing when the process cannot be found.
*/
getProcessForUser		:: !User !ProcessId			-> Task (Maybe TaskInstanceMeta)
/**
* Retrieves the processes with indicated process ids
*
* @param A list of process ids to match on
* 
* @return The list of found processes
*/
getProcesses			:: ![ProcessId]				-> Task [TaskInstanceMeta]
/**
* Retrieves all process that have one of the given statuses
*
* @param A list of statuses to match on
* 
* @return The list of processes having the given statuses
*/
getProcessesWithStatus	:: ![TaskStatus]		-> Task [TaskInstanceMeta]
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
getProcessesForUser		:: !User ![TaskStatus]	-> Task [TaskInstanceMeta]

/**
* Poll the status of a process.
*
* @param The process reference
*
* @return A task that yields the status of the referenced process
*/
getProcessStatus		:: !ProcessId				-> Task TaskStatus

/**
* Delete a process from the process database
* Once a process is deleted all of its results are lost.
*
* @param The process reference
*/
deleteProcess			:: !ProcessId				-> Task Void
