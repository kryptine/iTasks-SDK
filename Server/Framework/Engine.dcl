definition module Engine
/**
* This module provides the iTasks engine.
* This is the primary function that creates the complete
* environment in which worfklow specifications can be executed.
*/
import Maybe, JSON, Task
from IWorld			import :: IWorld
from HTTP			import :: HTTPRequest, :: HTTPResponse
from Config			import :: Config

:: HandlerFormat :== String

:: Handler :== (!String,![HandlerFormat],!String HandlerFormat [String] HTTPRequest *IWorld -> *(!HTTPResponse, !*IWorld))

/**
* Creates the iTasks system from a set of workflow definitions
*
* @param  An optional config record
* @param  A task to execute
* @return A list of predicate/handler pairs that can be plugged into a server
*/
engine :: !(Maybe Config) (Task a) ![Handler] -> [(!String -> Bool,!HTTPRequest *World -> (!HTTPResponse, !*World))] | iTask a

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
* Determines the server executables path
*/
determineAppPath :: !*World -> (!String, !*World)

/**
* Determine the name of the application based on the executable's name
*/
determineAppName :: !*World -> (!String,!*World)