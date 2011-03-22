definition module SystemTasks
/**
* This module provides tasks for interacting with the iTasks engine
*/
import Task

class emailOf r where emailOf :: r -> EmailAddress
instance emailOf EmailAddress
instance emailOf String
instance emailOf User

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

/**
* Send an e-mail
*
* @param The subject line of the e-mail
* @param The body of the e-mail
* @param The list of recipients. This can be either e-mail addresses or existing system users.
*
* @return The recipients to which the email was sent
*/
sendEmail :: !String !Note ![recipient] -> Task [recipient]	| emailOf recipient & iTask recipient




