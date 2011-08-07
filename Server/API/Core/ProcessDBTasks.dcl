definition module ProcessDBTasks
/**
* This module provides access to the process database
*/
import Maybe, SystemTypes
from Task		import :: Task
from ProcessDB	import :: Process(..)
from Time		import :: Timestamp 
import iTaskClass

derive gVisualizeText	Process, ProcessProperties, SystemProperties, TaskMeta, TaskStatus, TaskDescription
derive gVisualizeHtml	Process, ProcessProperties, SystemProperties, TaskMeta, TaskStatus, TaskDescription
derive gVisualizeEditor	Process, ProcessProperties, SystemProperties, TaskMeta, TaskStatus, TaskDescription
derive gUpdate			Process, ProcessProperties, SystemProperties, TaskMeta, TaskStatus, TaskDescription
derive gDefaultMask		Process, ProcessProperties, SystemProperties, TaskMeta, TaskStatus, TaskDescription
derive gVerify			Process, ProcessProperties, SystemProperties, TaskMeta, TaskStatus, TaskDescription

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
getProcessesWithStatus	:: ![TaskStatus] ![RunningTaskStatus]		-> Task [Process]
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
getProcessesForUser		:: !User ![TaskStatus] ![RunningTaskStatus]	-> Task [Process]
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
getProcessStatus		:: !ProcessId				-> Task (TaskStatus,RunningTaskStatus)

updateManagerProperties :: !ProcessId !(ManagerProperties -> ManagerProperties) -> Task Void

/**
* Delete a process from the process database
* Once a process is deleted all of its results are lost.
*
* @param The process reference
*/
deleteProcess			:: !ProcessId				-> Task Void
