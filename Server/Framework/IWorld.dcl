definition module IWorld

from FilePath		import :: FilePath
from Map			import :: Map
from Maybe			import :: Maybe
from SystemTypes	import :: DateTime, :: User, :: ProcessId, :: Config
from Time			import :: Timestamp

:: *IWorld		=	{ application			:: !String						// The name of the application	
					, build					:: !String						// The date/time identifier of the application's build
					, appDirectory			:: !FilePath					// Location of the application's executable
					, sdkDirectory			:: !FilePath					// Location of the iTasks SDK
					, dataDirectory			:: !FilePath					// Location of the applications data files
					, config				:: !Config						// The server configuration
					, timestamp				:: !Timestamp					// The timestamp of the current request
					, localDateTime			:: !DateTime					// The local date & time of the current request
					, latestEvent			:: !Maybe DateTime				// The date & time of the last event of the (detached) process
					, currentUser			:: !User						// The current user
					, evalStack				:: ![ProcessId]					// The stack of instances evaluating other instances through workOn
					, parallelStates		:: !Map String Dynamic			// The set of shared state variables used during parallel task execution
					, parallelControls		:: !Map String (!Int,![Control])// The set of controls for manipulating parallel task lists
					, readShares			:: !Maybe [String]				// The IDs of shares from which was read
					, world					:: !*World						// The outside world
					}
:: Control
	= AppendTask		!Int !User !Dynamic /* :: (TaskContainer s) */		// append and additional task to be run in parallel as well
																			// The user that appended the task is used as identity upon the first evaluation of the task 
	| RemoveTask		!Int												// remove the task with indicated index from the set