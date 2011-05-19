definition module IntegrationTasks
/**
* This module provides tasks for interaction with other systems.
*/

from Maybe import ::Maybe
from Void import ::Void

import iTaskClass
from Task import ::Task
from Types import ::Note, ::EmailAddress
from Shared import ::Shared, ::ReadOnlyShared

::HTTPMethod = GET | POST

/**
* Calls an external executable. The call is non-blocking.
*
* @param a message shown to the user while the process is running
* @param path to the executable
* @param a list of command-line arguments
* @return return-code of the process
* @throws CallException
* @return A shared reference in which the return code will be stored
*/
callProcess :: !FilePath ![String] -> Task Int

/**
* Calls an external HTTP webservice.
*
* @param the HTTP method (GET or POST) to use
* @param The URL of the webservice
* @param A list of name/value pairs
* @param A parse function that parses the response
* 
* @return A shared reference in which the response will be stored
*/
callRPCHTTP :: !HTTPMethod !String ![(String,String)] !(String -> a) -> Task a | iTask a

/**
* Send an e-mail message.
*
* @param The subject line of the e-mail
* @param The body of the e-mail
* @param The list of recipients. This can be either e-mail addresses or existing system users.
*
* @return The recipients to which the email was sent
*/
sendEmail :: !String !Note ![EmailAddress] -> Task [EmailAddress]