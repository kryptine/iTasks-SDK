definition module IWorld

from FilePath		import :: FilePath
from Map			import :: Map
from Maybe			import :: Maybe
from SystemTypes	import :: DateTime, :: User, :: Config, :: TaskNo, :: TaskId, :: TaskListItem, :: ParallelTaskType 
from Time			import :: Timestamp
from Task			import :: TaskTime
from TaskContext	import :: ParallelItem, :: ParallelControl

:: *IWorld		=	{ application			:: !String									// The name of the application	
					, build					:: !String									// The date/time identifier of the application's build
					, appDirectory			:: !FilePath								// Location of the application's executable
					, sdkDirectory			:: !FilePath								// Location of the iTasks SDK
					, dataDirectory			:: !FilePath								// Location of the applications data files
					, config				:: !Config									// The server configuration
					, taskTime				:: !TaskTime								// The 'virtual' time for the task. Increments at every event
					, timestamp				:: !Timestamp								// The timestamp of the current request
					, localDateTime			:: !DateTime								// The local date & time of the current request
					, latestEvent			:: !Maybe DateTime							// The date & time of the last event of the (detached) process
					, currentUser			:: !User									// The current user
					, nextTaskNo			:: !TaskNo									// The next task number to assign
					, evalStack				:: ![TaskId]								// The stack of instances evaluating other instances through workOn
					, parallelLists			:: !Map String (!Int,![ParallelItem])		// The set of shared tasklist meta data
					, parallelControls		:: !Map String (!Int,![ParallelControl])	// The set of controls for manipulating parallel task lists
					, localShares			:: !Map String (!Int,!Dynamic)				// The set of locally shared values
					, readShares			:: !Maybe [String]							// The IDs of shares from which was read
					, world					:: !*World									// The outside world
					}
