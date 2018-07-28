definition module iTasks.Engine
/**
* This module provides the iTasks engine.
* This is the primary function that creates the complete
* environment in which worfklow specifications can be executed.
*/

from Data.Maybe      import :: Maybe
from System.FilePath import :: FilePath
from Internet.HTTP   import :: HTTPRequest
from System.Time     import :: Timespec

import iTasks.WF.Definition

:: EngineOptions =
	{ appName 		:: String
	, appPath		:: FilePath // Location of the application's executable
	, appVersion    :: String
	, serverPort	:: Int
    , serverUrl     :: String
	, keepaliveTime :: Timespec
    , sessionTime   :: Timespec
    , persistTasks  :: Bool
	, autoLayout    :: Bool
	, timeout       :: Maybe Int // The timeout
	, webDirPath 	:: FilePath  // Location of public files that are served by the iTask webserver
	, storeDirPath 	:: FilePath  // Location of the application's persistent data files 
	, tempDirPath 	:: FilePath  // Location for temporary files used in tasks
	, saplDirPath   :: FilePath  // Location of the application's sapl files (client-side code)
	}

/**
* Executes the task framework with a collection of startable task definitions.
*
* @param Tasks to start
* @param The world
* @return The world
*/
doTasks :: a !*World -> *World | Startable a
startEngine :== doTasks //Backwards compatibility

/**
* Starts the task engine with options and a list of published task definitions.
*
* @param Tasks to start
* @param An initialization function to set the engine options with:
      @param The command line arguments
      @param The default options
	  @return Maybe the engine options, in case of Nothing, the engine is not started
      @return A message that is printed to the console when the engine is started
* @param The world
* @return The world
*/
doTasksWithOptions :: ([String] EngineOptions -> (!Maybe EngineOptions,![String])) a !*World
	-> *World | Startable a

startEngineWithOptions :== doTasksWithOptions

/**
* The function that takes the 'standard' command line options of an itask engine and
* shows the default help and startup message
*
* Essentially: doTasks = doTasksWithOptions defaultEngineCLIOptions 

* @param The command line arguments
* @param The default options
* @return Maybe the engine options, in case of Nothing, the engine is not started
* @return A message that is printed to the console when the engine is started
*/
defaultEngineCLIOptions :: [String] EngineOptions -> (!Maybe EngineOptions,![String])

/**
* Start a stripped task engine (without an HTTP server) with a list of tasks to be created 
*/
runTasks :: a !*World -> *World | Runnable a

runTasksWithOptions :: ([String] EngineOptions -> (!Maybe EngineOptions,![String])) a !*World -> *World | Runnable a

/*
* There are two ways tasks can be started:
* Interactively when a user requests it through the web,
* or directly when the application (server) is started,
*/
:: StartableTask
  = WebTask !WebTask
  | StartupTask !StartupTask

:: WebTask =
	{ url  :: !String
	, task :: !WebTaskWrapper
	}

:: StartupTask =
	{ attributes :: !TaskAttributes 
	, task       :: !TaskWrapper
	}

:: WebTaskWrapper = E.a: WebTaskWrapper (HTTPRequest -> Task a) & iTask a
:: TaskWrapper = E.a: TaskWrapper (Task a) & iTask a

//Utility functions for creating collections of startable tasks
atRequest :: String (HTTPRequest -> Task a) -> StartableTask | iTask a
atStartup :: TaskAttributes (Task a) -> StartableTask | iTask a

publish :== atRequest //Backwards compatibility

class Startable a
where
	toStartable :: !a -> [StartableTask]

instance Startable (Task a) | iTask a //Default as web task
instance Startable (HTTPRequest -> Task a) | iTask a //As web task
instance Startable StartableTask
instance Startable [StartableTask]

// === Wrapping non-interactive tasks for running on the command line ===
class Runnable a
where
	toRunnable :: !a -> [StartableTask]

instance Runnable (Task a) | iTask a

/**
* Determines the default options for an application
*/
defaultEngineOptions :: !*World -> (!EngineOptions,!*World)
