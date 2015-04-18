definition module iTasks.API.Core.IntegrationTasks
/**
* This module provides tasks for interaction with other systems.
*/

from Data.Maybe  import :: Maybe
from Data.Void   import :: Void
from Data.Error  import :: MaybeError, :: MaybeErrorString
from System.FilePath import :: FilePath
from Internet.HTTP import :: HTTPMethod, :: HTTPResponse
from Text.URI import :: URI

import iTasks._Framework.Generic
from iTasks._Framework.Task              import :: Task
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
callHTTP	:: !HTTPMethod !URI !String !(HTTPResponse -> (MaybeErrorString a)) -> Task a | iTask a	
callRPCHTTP :: !HTTPMethod !URI ![(String,String)] !(HTTPResponse -> a) -> Task a | iTask a

/**
* Creates a temporary directory on the server's file system for the duration of a task
*
* @param The task that gets the temporary directory's location as argument
*
* @return The result of the task
*/
withTemporaryDirectory :: (FilePath -> Task a) -> Task a | iTask a

