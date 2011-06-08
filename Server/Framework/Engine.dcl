definition module Engine
/**
* This module provides the iTasks engine.
* This is the primary function that creates the complete
* environment in which worfklow specifications can be executed.
*/
import Maybe, JSON, Task
from SystemTypes	import :: Workflow
from IWorld			import :: IWorld
from HTTP			import :: HTTPRequest, :: HTTPResponse
from Config			import :: Config

:: HandlerFormat :== String

:: Handler :== (!String,![HandlerFormat],!String HandlerFormat [String] HTTPRequest *IWorld -> *(!HTTPResponse, !*IWorld))

/**
* Creates the iTasks system from a set of workflow definitions
*
* @param  An optional config record
* @param  A list of workflow definitions
* @return A list of predicate/handler pairs that can be plugged into a server
*/
engine :: !(Maybe Config) [Workflow] ![Handler] -> [(!String -> Bool,!HTTPRequest *World -> (!HTTPResponse, !*World))]

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
* @param The task(container) (with or without parameter)
*/
workflow :: String String w -> Workflow | workflowTask w

/**
*
* Wraps any task as a workflow that is only available to specified roles
*
* @param A label for the workflow. This may contain slashes to group workflows
* @param A description of the workflow
* @param A list of roles. The workflow will be available to users with any of the specified roles
* @param The task(container) (with or without parameter)
*/
restrictedWorkflow :: String String [Role] w -> Workflow | workflowTask w

class workflowTask w :: String String [Role] w -> Workflow

instance workflowTask (Task a)						| iTask a
instance workflowTask (WorkflowContainer a)			| iTask a
instance workflowTask (a -> Task b)					| iTask a & iTask b
instance workflowTask (ParamWorkflowContainer a b)	| iTask a & iTask b

:: WorkflowContainer a			= Workflow		ManagerProperties (Task a)
:: ParamWorkflowContainer a b	= ParamWorkflow	ManagerProperties (a -> Task b)

/**
* Determines the server executables path
*/
determineAppPath :: !*World -> (!String, !*World)

/**
* Determine the name of the application based on the executable's name
*/
determineAppName :: !*World -> (!String,!*World)