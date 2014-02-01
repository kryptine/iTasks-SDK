definition module iTasks.API.Core.IntegrationTasks
/**
* This module provides tasks for interaction with other systems.
*/

from Data.Maybe  import :: Maybe
from Data.Void   import :: Void
from Data.Error  import :: MaybeError, :: MaybeErrorString
from System.FilePath import :: FilePath
from Internet.HTTP import :: HTTPMethod
from Text.URI import :: URI

import iTasks.Framework.Generic
from iTasks.Framework.Task              import :: Task
from iTasks.API.Core.Types              import class descr, :: Note, :: EmailAddress, :: ProcessStatus, :: Document
from iTasks.API.Common.InteractionTasks import :: ViewOption //TODO: We shouldn't import from Common in Core

/**
* Call a function that interacts with the world
*
* @param The function to call
*/
worldIO :: (*World -> *(!MaybeError e a,!*World)) -> Task a | iTask a & TC e & toString e

/**
* Calls an external executable. The call is non-blocking.
*
* @param Task description
* @param Executable: path to the executable
* @param Arguments: a list of command-line arguments
@ @param Optional startup directory
* @return return-code of the process
* @throws CallException
* 
* @gin-title Start executable
* @gin-icon executable
*/
callProcess :: !d ![ViewOption ProcessStatus] !FilePath ![String] !(Maybe FilePath) -> Task ProcessStatus | descr d

/**
* Calls an external executable. This call blocks task computation, only use when process is known to terminate fast.
* @param Executable: path to the executable
* @param Arguments: a list of command-line arguments
@ @param Optional startup directory
* @return return-code of the process
* @throws CallException
*/
callInstantProcess :: !FilePath ![String] !(Maybe FilePath)-> Task Int

/**
* Calls an external HTTP webservice.
*
* @param HTTP Method: the HTTP method (GET or POST) to use
* @param URL: The URL of the webservice
* @param Parameters: A list of name/value pairs
* @param Response handler: A parse function that parses the response
*
* @return The parsedd value
*
* @gin-title Call web service
* @gin-icon webservice
*/
callHTTP2   :: !HTTPMethod !URI !String !(String -> (MaybeErrorString a)) -> Task a | iTask a
callHTTP	:: !HTTPMethod !String !String !(String -> (MaybeErrorString b)) -> Task b | iTask b	
callRPCHTTP :: !HTTPMethod !String ![(String,String)] !(String -> a) -> Task a | iTask a

/**
* Downloads a document from the web
*
* @param URL: The url that has to be downloaded
* @return The downloaded document
*/
httpDownloadDocument    :: String -> Task Document

/**
* Downloads a document from the web to a specific location
*
* @param URL: The url that has to be downloaded
* @param URL: The location to which the document is downloaded
* @return The location of the downloaded document
*/
httpDownloadDocumentTo  :: String FilePath -> Task FilePath

/**
* Creates a temporary directory on the server's file system for the duration of a task
*
* @param The task that gets the temporary directory's location as argument
*
* @return The result of the task
*/
withTemporaryDirectory :: (FilePath -> Task a) -> Task a | iTask a

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

class toEmail r where toEmail :: !r -> EmailAddress
instance toEmail EmailAddress
instance toEmail String
