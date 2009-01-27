definition module ProcessCombinators
/**
* This module provides modules to create and manipulate dynamic
* subprocesses inside a workflow process.
*/

import StdEnv, StdMaybe
import TSt
from ProcessDB import :: ProcessStatus

derive gForm ProcessReference
derive gUpd ProcessReference
derive gPrint ProcessReference
derive gParse ProcessReference

/**
* A typed process reference. These references are used to reference
* a dynamically created process. The type parameter a ensures that use
* of the process result is type safe. 
*/
:: ProcessReference a

/**
* Create a new process.
*
* @param The user that will perform processes main task.
* @param Activate the process immediately (False creates the process in a suspended state)
*
* @return A reference to the newly created process
*/
spawnProcess			:: !UserId !Bool !(LabeledTask a)	-> Task (ProcessReference a)	| iData a

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
setProcessOwner			:: UserId (ProcessReference a)		->	Task Bool					| iData a 