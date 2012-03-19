definition module IntegrationTasks
/**
* This module provides tasks for interaction with other systems.
*/

from Maybe	import :: Maybe
from Void	import :: Void
from Error	import :: MaybeError, :: MaybeErrorString

from Task 			import :: Task
from SystemTypes	import :: Note, :: EmailAddress

import iTaskClass

:: HTTPMethod = GET | POST

/**
* Calls an external executable. The call is non-blocking.
*
* @param Executable: path to the executable
* @param Arguments: a list of command-line arguments
* @return return-code of the process
* @throws CallException
* 
* @gin-title Start executable
* @gin-icon executable
*/
callProcess :: !FilePath ![String] -> Task Int

/**
* Calls an external HTTP webservice.
*
* @param HTTP Method: the HTTP method (GET or POST) to use
* @param URL: The URL of the webservice
* @param Parameters: A list of name/value pairs
* @param Response handler: A parse function that parses the response
* 
* @return A shared reference in which the response will be stored
* 
* @gin-title Call web service
* @gin-icon webservice
*/
callHTTP	:: !HTTPMethod !String !String !(String -> (MaybeErrorString b)) -> Task b | iTask b	
callRPCHTTP :: !HTTPMethod !String ![(String,String)] !(String -> a) -> Task a | iTask a

/**
* Send an e-mail message.
*
* @param Subject: The subject line of the e-mail
* @param Body: The body of the e-mail
* @param Sender: The sender address
* @param Recipients: The list of recipients
*
* @return The recipients to which the email was sent

* @gin-title Send e-mail
* @gin-icon email
*/
sendEmail :: !String !Note !sndr ![rcpt] -> Task [EmailAddress] | toEmail sndr & toEmail rcpt