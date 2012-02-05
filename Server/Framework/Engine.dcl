definition module Engine
/**
* This module provides the iTasks engine.
* This is the primary function that creates the complete
* environment in which worfklow specifications can be executed.
*/
import Maybe, JSON, FilePath, Task
from IWorld			import :: IWorld
from HTTP			import :: HTTPRequest, :: HTTPResponse

:: PublishedTask =
	{ url			:: String
	, task			:: TaskWrapper
	, defaultFormat	:: ServiceFormat
	}
	
//* The format in which a task is presented.
:: ServiceFormat
	= WebApp			
	| JSONGui
	| JSONService
	| JSONPlain

/**
* Creates the iTasks system from a set of published tasks
*
* @param  The config record
* @param  A task to execute
*/
engine :: !FilePath publish -> [(!String -> Bool,!HTTPRequest *World -> (!HTTPResponse, !*World))] | Publishable publish

/**
* Wraps a task together with a url to make it publishable by the engine
*/
publish :: String ServiceFormat (Task a) -> PublishedTask | iTask a

class Publishable a
where
	publishAll :: !a -> [PublishedTask]

instance Publishable (Task a) | iTask a
instance Publishable [PublishedTask]

/**
* Determines the server executables path
*/
determineAppPath :: !*World -> (!FilePath, !*World)

/**
* Determine the name of the application based on the executable's name
*/
determineAppName :: !*World -> (!String,!*World)

/**
* Determine the location of the iTasks SDK
*/
determineSDKPath :: ![FilePath] !*World -> (!Maybe FilePath, !*World)

