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
* Gets the user that the current task is assigned to
*
* @return The username of the user that currently works on a task
*/
getContextWorker :: Task User
/**
* Gets the user that the current task is managed by
*
* @return the username of the user that manages the current task
*/
getContextManager :: Task User
/**
* Compute a default value 
*
* @return The default value
*/
getDefaultValue 		:: Task a | iTask a
/**
* Gets a random integer
*
* @return The random number
*/ 
getRandomInt 			:: Task Int
