definition module Engine
/**
* This module provides the iTasks engine.
* This is the primary function that creates the complete
* environment in which worfklow specifications can be executed.
*/
import Maybe, JSON, Task
from TSt		import :: Workflow
from HTTP		import :: HTTPRequest, :: HTTPResponse
from Config		import :: Config

/**
* Creates the iTasks system from a set of workflow definitions
*
* @param  An optional config record
* @param  A list of workflow definitions
* @return A list of predicate/handler pairs that can be plugged into a server
*/
engine :: !(Maybe Config) [Workflow] -> [(!String -> Bool, HTTPRequest *World -> (!HTTPResponse, !*World))]

/**
* Loads the itasks specific config
*
* @param The world
* 
* @return The configuration options
* @return The updated world
*/
config :: !*World -> (!Maybe Config,!*World)

/**
* Wraps any task as a workflow with no access restrictions
*
* @param A label for the workflow. This may contain slashes to group workflows
* @param A description of the workflow
* @param The task
*/
workflow :: !String !String !(Task a) -> Workflow | iTask a


/**
*
* Wraps any task as a workflow that is only available to specified roles
*
* @param A label for the workflow. This may contain slashes to group workflows
* @param A description of the workflow
* @param A list of roles. The workflow will be available to users with any of the specified roles
* @param The task
*/
restrictedWorkflow :: !String !String ![Role] !(Task a) -> Workflow | iTask a

/**
* Determines the server executables path
*/
determineAppPath :: !*World -> (!String, !*World)

/**
* Determine the name of the application based on the executable's name
*/
determineAppName :: !*World -> (!String,!*World)