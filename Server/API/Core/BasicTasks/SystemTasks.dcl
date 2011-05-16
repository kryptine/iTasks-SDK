definition module SystemTasks
/**
* This module provides tasks for interacting with the iTasks engine
*/
import Task

/**
* Gets the user that the current task is assigned to
*
* @return The current user
*/
getCurrentUser 			::  Task User

/**
* Send an e-mail
*
* @param The subject line of the e-mail
* @param The body of the e-mail
* @param The list of recipients. This can be either e-mail addresses or existing system users.
*
* @return The recipients to which the email was sent
*/
sendEmail :: !String !Note ![EmailAddress] -> Task [EmailAddress]




