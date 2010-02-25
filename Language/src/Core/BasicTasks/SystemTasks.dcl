definition module SystemTasks
/**
* This module provides tasks for interacting with the iTasks engine
*/
from TSt		import :: Task
from Types		import :: User, :: ProcessId, :: ProcessRef

from	iTasks		import class iTask
import	GenPrint, GenParse, GenVisualize, GenUpdate

/**
* Returns the user currently logged in the iTask system
*
* @return The current user
*/
getCurrentUser 			::  Task User

/**
* Retrieves the process id of the current process
*
* @return The process id of the current process
*/
getCurrentProcessId		:: 	Task ProcessId

/**
* Compute a default value 
*
* @return The default value
*/
getDefaultValue 		:: Task a | iTask a

/**
* Create a new process.
*
* @param The user that will perform processes main task.
* @param Activate the process immediately (False creates the process in a suspended state)
*
* @return A reference to the newly created process
*/
spawnProcess	:: !UserName !Bool !(Task a)	-> Task (ProcessRef a) | iTask a

/**
* Wait (blocking) for a process to complete.
*
* @param The process reference
*
* @return A task that maybe gives the result of the process.
*         When a process is prematurely deleted, the task yields Nothing
*/
waitForProcess	:: (ProcessRef a)				-> Task (Maybe a)	| iTask a