definition module iTasks._Framework.Engine
/**
* This module provides the iTasks engine.
* This is the primary function that creates the complete
* environment in which worfklow specifications can be executed.
*/

from StdList import ++, iterate, take
from System.FilePath import </>
from System.OS import IF_POSIX_OR_WINDOWS
import iTasks._Framework.Task

from Internet.HTTP			import :: HTTPRequest

//* Configuration defaults
DEFAULT_PORT			:== IF_POSIX_OR_WINDOWS 8080 80
DEFAULT_KEEPALIVE_TIME	:== 300 // 5 minutes
SESSION_TIMEOUT         :== 600 //Seconds (10 minutes)
MAX_EVENTS 		        :== 5

:: PublishedTask =
	{ url			:: String
	, task			:: WebTaskWrapper
	}

:: ServerOptions =
	{ appName 		:: String
	, appPath		:: FilePath
	, serverPort	:: Int
	, keepalive 	:: Int
	, webDirPath 	:: Maybe FilePath
	, storeDirPath 	:: Maybe FilePath
	, saplDirPath   :: Maybe FilePath
	}
	
:: WebTaskWrapper = E.a: WebTaskWrapper (HTTPRequest -> Task a) & iTask a
:: TaskWrapper = E.a: TaskWrapper (Task a) & iTask a

/**
* Starts the task engine with a list of published task definitions.
*
* @param Tasks to start
* @param The world
* @return The world
*/
startEngine :: a !*World -> *World | Publishable a

/**
* Starts the task engine with options and a list of published task definitions.
*
* @param Tasks to start
* @param Options to use like port and server paths.
* @param The world
* @return The world
*/
startEngineWithOptions :: a ServerOptions !*World -> *World | Publishable a

/**
* Wraps a task together with a url to make it publishable by the engine
*/
publish :: String (HTTPRequest -> Task a) -> PublishedTask | iTask a

/**
* This function publishes a task with autolayouting turned off 
* to enable testing and debugging without layout processing
*/
publishWithoutLayout :: String (HTTPRequest -> Task a) -> PublishedTask | iTask a

class Publishable a
where
	publishAll :: !a -> [PublishedTask]

instance Publishable (Task a) | iTask a
instance Publishable (HTTPRequest -> Task a) | iTask a
instance Publishable [PublishedTask]

determineAppName :: !*World -> (!String,!*World)

/**
* Start a stripped task engine (without an HTTP server) with a list of tasks to be created 
*/
class Runnable a
where
	toRunnable :: !a -> [TaskWrapper] 

instance Runnable (Task a) | iTask a
instance Runnable [TaskWrapper]

runTasks :: a !*World -> *World | Runnable a

//HACK FOR RUNNING BACKGROUND TASKS ON A CLIENT
background :: !*IWorld -> *IWorld

