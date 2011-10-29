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

:: PublishedTask =
	{ url	:: String
	, task	:: TaskWrapper
	}

/**
* Creates the iTasks system from a set of published tasks
*
* @param  An optional config record
* @param  A task to execute
*/
engine :: !(Maybe Config) publish -> [(!String -> Bool,!HTTPRequest *World -> (!HTTPResponse, !*World))] | Publishable publish

/**
* Wraps a task together with a url to make it publishable by the engine
*/
publish :: String (Task a) -> PublishedTask | iTask a

class Publishable a
where
	publishAll :: a -> [PublishedTask]

instance Publishable (Task a) | iTask a
instance Publishable [PublishedTask]

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