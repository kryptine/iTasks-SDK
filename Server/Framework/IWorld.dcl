definition module IWorld

from Config			import :: Config
from FilePath		import :: FilePath
from Map			import :: Map
from Maybe			import :: Maybe
from SystemTypes	import :: DateTime, :: User
from Time			import :: Timestamp
from SystemTypes	import :: ProcessId

:: *IWorld		=	{ application			:: !String						// The name of the application	
					, storeDirectory		:: !FilePath					// The generic data store
					, tmpDirectory			:: !FilePath					// The path for temporary files, the garbage collector also works on files in this dir
					, config				:: !Config						// The server configuration
					, timestamp				:: !Timestamp					// The timestamp of the current request
					, latestEvent			:: !Maybe Timestamp				// The timestamp of the last event of the (detached) process
					, localDateTime			:: !DateTime					// The local date & time of the current request
					, currentUser			:: !User						// The currently logged in user
					, currentProcess		:: !ProcessId					// The current process
					, parallelStates		:: !Map String Dynamic			// The set of shared state variables used during parallel task execution
					, parallelControls		:: !Map String (!Int,![Control])// The set of controls for manipulating parallel task lists
					, readShares			:: !Maybe [String]				// The IDs of shares from which was read
					, evalStack				:: ![ProcessId]					// The stack of instances evaluating other instances through workOn
					, world					:: !*World						// The outside world
					}
:: Control
	= AppendTask		!Int !Dynamic /* :: (TaskContainer s) */	// append and additional task to be run in parallel as well
	| RemoveTask		!Int										// remove the task with indicated index from the set