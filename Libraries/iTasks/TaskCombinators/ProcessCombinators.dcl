definition module ProcessCombinators
/**
* This module provides modules to create and manipulate dynamic
* subprocesses inside a workflow process.
*/

import StdMaybe
from TSt		import :: Task, :: LabeledTask
from ProcessDB	import :: ProcessStatus(..), :: Process(..), :: ProcessType(..)
from Types		import :: UserId, :: ProcessId, :: DynamicId, :: TaskId
from TaskTree	import :: TaskProperties

import iDataForms

derive gForm	ProcessReference, Process, ProcessStatus
derive gUpd		ProcessReference, Process, ProcessStatus
derive gPrint	ProcessReference, Process, ProcessStatus
derive gParse	ProcessReference, Process, ProcessStatus

/**
* A typed process reference. These references are used to reference
* a dynamically created process. The type parameter a ensures that use
* of the process result is type safe. 
*/
:: ProcessReference a	= ProcessReference !ProcessId

/**
* Create a new process.
*
* @param The user that will perform processes main task.
* @param Activate the process immediately (False creates the process in a suspended state)
*
* @return A reference to the newly created process
*/
spawnProcess			:: !UserId !Bool !(LabeledTask a)	-> Task (ProcessReference a) | iData a

/**
* Wait (blocking) for a process to complete.
*
* @param The process reference
*
* @return A task that maybe gives the result of the process.
*         When a process is prematurely deleted, the task yields Nothing
*/
waitForProcess			:: (ProcessReference a)				-> Task (Maybe a)				| iData a

/**
* Poll the status of a process.
*
* @param The process reference
*
* @return A task that yields the status of the referenced process
*/
getProcessStatus		:: (ProcessReference a)				-> Task ProcessStatus			| iData a

/**
* Poll who is the owner of a process.
*
* @param The process reference
*
* @return A task that yields the owner if the referenced process is not deleted
*/
getProcessOwner 		:: (ProcessReference a) 			-> Task (Maybe Int)
/**
* Change the process status to Active
*
* @param The process reference
*
* @return A task that yields True when the process was successfully activated
*         and False when the process could not be found.
*/
activateProcess			:: (ProcessReference a)				-> Task Bool					| iData a

/**
* Change the process status to suspended.
* The tasks within this process will be inaccessible until the process is activated again.
*
* @param The process reference
*
* @return A task that yields True when the process was successfully suspended
*         and False when the process could not be found.
*/
suspendProcess			:: (ProcessReference a)				-> Task Bool					| iData a
/**
* Change the process status of the current process to suspended.
*
* @return A task that yields True when the process was successfully suspended
*         and False when the process could not be found.
*/
suspendCurrentProcess	::									   Task Bool
/**
* Delete a process from the process database
* Once a process is deleted all of its results are lost.
*
* @param The process reference
*
* @return A task that yields True when the process was successfully deleted
*         and False when the process could not be found.
*/
deleteProcess			:: (ProcessReference a)				-> Task Bool					| iData a
/**
* Delete the current process from the process database
* 
* @return A task that yields True when the process was successfully deleted
*         and False when the process could not be found.
*/
deleteCurrentProcess	::									    Task Bool
/**
* Update the process owner. By changing the process owner complete processes can be reassigned
* to a new user.
*
* @param The user that will be the new owner
* @param The process reference
*
* @return A task that yields True when the process owner was successfully updated
*         and False when the process could not be found.
*/
updateProcessOwner		:: UserId (ProcessReference a)		->	Task Bool					| iData a


//Untyped process tasks, these regard a process as a black box

/**
* Retrieves the process id of the current process
*
* @return The process id of the current process
*/
getCurrentProcessId		:: 										Task ProcessId
/**
* Retrieves a Process record from the process table
* 
* @param The process id
*
* @return When found, the Process record. Nothing when the process can not be found.
*/
getProcess				:: !ProcessId 							-> Task (Maybe Process)
/**
* Retrieves a Process record with an additional check on the process owner. Only
* when the process is owned by the indicated user it will be returned.
*
* @param The owner of the indicated process
* @param The process id
*
* @return When found, the Process record. Nothing when the process can not be found.
*/
getProcessForUser		:: !UserId !ProcessId 					-> Task (Maybe Process)
/**
* Retrieves all process that have one of the given statuses
*
* @param A list of statuses to match on
* 
* @return The list of processes having the given statuses
*/
getProcesses			:: ![ProcessStatus]						-> Task [Process]
/**
* Retrieves the processes with indicated process ids
*
* @param A list of process ids to match on
* 
* @return The list of found processes
*/
getProcessesById		:: ![ProcessId]							-> Task [Process]
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
getProcessesForUser		:: !UserId ![ProcessStatus] Bool		-> Task [Process]
/**
* Changes the owner of the indicated process. The current user is automatically set
* as delegator of the process.
*
* @param The new process owner
* @param The process id
*
* @return True when the process is updated, False if the process was not found.
*/
setProcessOwner			:: !UserId !ProcessId					-> Task Bool
/**
* Changes the status of the indicated process.
*
* @param The new process status
* @param The process id
*
* @return True when the process is updated, False if the process was not found.
*/
setProcessStatus		:: !ProcessStatus !ProcessId			-> Task Bool
