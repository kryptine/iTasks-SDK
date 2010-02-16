definition module ProcessDBTasks
/**
* This module provides access to the process database
*/
import StdMaybe
from TSt		import :: Task
from ProcessDB	import :: ProcessStatus(..), :: Process(..)
from Types		import :: ProcessId, :: ProcessRef, :: DynamicId, :: TaskId
from TaskTree	import :: TaskProperties, :: TaskPriority, :: TaskProgress
from Time		import :: Timestamp 

from iTasks		import class iTask
import GenPrint, GenParse, GenVisualize, GenUpdate

derive gVisualize	ProcessRef, Process, ProcessStatus, TaskProperties, TaskPriority, TaskProgress, Timestamp
derive gUpdate		ProcessRef, Process, ProcessStatus, TaskProperties, TaskPriority, TaskProgress, Timestamp
derive gPrint		ProcessRef, Process, ProcessStatus, TaskProperties, TaskPriority, TaskProgress, Timestamp
derive gParse		ProcessRef, Process, ProcessStatus, TaskProperties, TaskPriority, TaskProgress, Timestamp

//Allow either typed or untyped references to lookup a process table entry
class toProcessId a where toProcessId :: a -> ProcessId
instance toProcessId ProcessId
instance toProcessId (ProcessRef a)

/**
* Retrieves a Process record from the process table
* 
* @param The process id
*
* @return When found, the Process record. Nothing when the process can not be found.
*/
getProcess				:: !pid 					-> Task (Maybe Process) | toProcessId pid
/**
* Retrieves a Process record with an additional check on the process owner. Only
* when the process is owned by the indicated user it will be returned.
*
* @param The owner of the indicated process
* @param The process id
*
* @return When found, the Process record. Nothing when the process can not be found.
*/
getProcessForUser		:: !UserName !pid			-> Task (Maybe Process)	| toProcessId pid
/**
* Retrieves the processes with indicated process ids
*
* @param A list of process ids to match on
* 
* @return The list of found processes
*/
getProcesses			:: ![pid]					-> Task [Process] 		| toProcessId pid
/**
* Retrieves all process that have one of the given statuses
*
* @param A list of statuses to match on
* 
* @return The list of processes having the given statuses
*/
getProcessesWithStatus	:: ![ProcessStatus]			-> Task [Process]
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
getProcessesForUser		:: !UserName ![ProcessStatus]	-> Task [Process]
/**
* Poll who is the owner of a process.
*
* @param The process reference
*
* @return A task that yields the owner if the referenced process is not deleted
*/
getProcessOwner 		:: !pid 					-> Task (Maybe UserName) | toProcessId pid
/**
* Changes the owner of the indicated process. The current user is automatically set
* as delegator of the process.
*
* @param The new process owner
* @param The process id
*
* @return True when the process is updated, False if the process was not found.
*/
setProcessOwner			:: !UserName !pid			-> Task Bool 			| toProcessId pid
/**
* Poll the status of a process.
*
* @param The process reference
*
* @return A task that yields the status of the referenced process
*/
getProcessStatus		:: !pid						-> Task ProcessStatus	| toProcessId pid
/**
* Change the process status to Active
*
* @param The process reference
*
* @return A task that yields True when the process was successfully activated
*         and False when the process could not be found.
*/
activateProcess			:: !pid						-> Task Bool			| toProcessId pid
/**
* Change the process status to suspended.
* The tasks within this process will be inaccessible until the process is activated again.
*
* @param The process reference
*
* @return A task that yields True when the process was successfully suspended
*         and False when the process could not be found.
*/
suspendProcess			:: !pid						-> Task Bool			| toProcessId pid
/**
* Delete a process from the process database
* Once a process is deleted all of its results are lost.
*
* @param The process reference
*
* @return A task that yields True when the process was successfully deleted
*         and False when the process could not be found.
*/
deleteProcess			:: pid						-> Task Bool			| toProcessId pid
