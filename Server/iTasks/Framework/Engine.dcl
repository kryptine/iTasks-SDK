definition module iTasks.Framework.Engine
/**
* This module provides the iTasks engine.
* This is the primary function that creates the complete
* environment in which worfklow specifications can be executed.
*/

from StdList import ++, iterate, take
from System.FilePath import </>
from System.OS import IF_POSIX_OR_WINDOWS
import iTasks.Framework.Task

from Internet.HTTP			import :: HTTPRequest

//* Configuarion defaults
DEFAULT_PORT			:== IF_POSIX_OR_WINDOWS 8080 80
DEFAULT_KEEPALIVE_TIME	:== 300 // 5 minutes
DEFAULT_THEME           :== "gray"
SEARCH_PATHS			:== RELATIVE_LOCATIONS ++ DEFAULT_LOCATIONS
DEFAULT_LOCATIONS		:== IF_POSIX_OR_WINDOWS ["/usr/lib/itasks"] ["C:\\Clean 2.4","C:\\Program Files"]
RELATIVE_LOCATIONS		:== [".": take 5 (iterate ((</>) "..") "..")]

:: PublishedTask =
	{ url			:: String
	, task			:: TaskWrapper
	, defaultFormat	:: ServiceFormat
	}
	
:: TaskWrapper = E.a: TaskWrapper (HTTPRequest -> Task a) & iTask a
	
//* The format in which a task is presented.
:: ServiceFormat
	= WebApp [WebAppOption]
	| JSONGui
	| JSONGuiEventStream
	| JSONService
	| JSONPlain

:: WebAppOption
    = Theme String

//Connection types used by the engine
:: ConnectionType
    = EventSourceConnection InstanceNo
    | WebSocketConnection InstanceNo

/**
* Starts the task engine with a list of published task definitions.
*
* @param Tasks to start
* @param The world
* @return The world
*/
startEngine :: a !*World -> *World | Publishable a

// Backround process. TODO
background :: !*IWorld -> (!Bool,!*IWorld)

/**
* Wraps a task together with a url to make it publishable by the engine
*/
publish :: String ServiceFormat (HTTPRequest -> Task a) -> PublishedTask | iTask a

class Publishable a
where
	publishAll :: !a -> [PublishedTask]

instance Publishable (Task a) | iTask a
instance Publishable (HTTPRequest -> Task a) | iTask a
instance Publishable [PublishedTask]

determineAppName :: !*World -> (!String,!*World)
