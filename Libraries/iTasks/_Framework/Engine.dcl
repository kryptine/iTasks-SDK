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
DEFAULT_THEME           :== "gray"
SEARCH_PATHS			:== RELATIVE_LOCATIONS ++ DEFAULT_LOCATIONS
DEFAULT_LOCATIONS		:== IF_POSIX_OR_WINDOWS ["/usr/lib/itasks"] ["C:\\Clean 2.4","C:\\Program Files"]
RELATIVE_LOCATIONS		:== [".": take 5 (iterate ((</>) "..") "..")]

:: PublishedTask =
	{ url			:: String
	, task			:: TaskWrapper
	}

:: ServerOptions =
	{ appName 		:: String
	, appPath		:: FilePath
	, sdkPath		:: Maybe FilePath
	, serverPort	:: Int
	, keepalive 	:: Int
	, webDirPaths	:: Maybe [FilePath]
	, storeOpt 		:: Maybe FilePath
	, saplOpt		:: Maybe FilePath
	}
	
:: TaskWrapper = E.a: TaskWrapper (HTTPRequest -> Task a) & iTask a

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

//HACK FOR RUNNING BACKGROUND TASKS ON A CLIENT
background :: !*IWorld -> *IWorld
