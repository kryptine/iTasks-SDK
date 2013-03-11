definition module IWorld

from FilePath			import :: FilePath
from Void				import :: Void
from Map				import :: Map
from Maybe				import :: Maybe
from SystemTypes		import :: DateTime, :: User, :: Config, :: InstanceNo, :: TaskNo, :: TaskId, :: TaskListItem, :: ParallelTaskType, :: TaskTime, :: SessionId
from UIDefinition		import :: UIDef, :: UIControl
from Time				import :: Timestamp
from TaskState			import :: TaskListEntry
from JSON				import :: JSONNode
from StdFile			import class FileSystem		
from SharedDataSource	import class registerSDSDependency, class registerSDSChangeDetection, class reportSDSChange, :: CheckRes(..), :: BasicShareId, :: Hash

:: *IWorld		=	{ application			:: !String									// The name of the application	
					, build					:: !String									// The date/time identifier of the application's build
					, appDirectory			:: !FilePath								// Location of the application's executable
					, sdkDirectory			:: !FilePath								// Location of the iTasks SDK
					, dataDirectory			:: !FilePath								// Location of the applications data files
					, config				:: !Config									// The server configuration
					, taskTime				:: !TaskTime								// The 'virtual' time for the task. Increments at every event
					, timestamp				:: !Timestamp								// The timestamp of the current request	
					, currentDateTime		:: !DateTime								// The local date & time of the current request
					, currentUser			:: !User									// The current user
					, currentInstance		:: !InstanceNo								// The current evaluated task instance
					, nextTaskNo			:: !TaskNo									// The next task number to assign
					, localShares			:: !Map TaskId JSONNode						// The set of locally shared values
					, localLists			:: !Map TaskId [TaskListEntry]				// The set of local parallel task lists
					, readShares			:: ![String]								// The IDs of shares from which was read
					, sessions				:: !Map SessionId InstanceNo				// Index of sessions to instance numbers
					, uis					:: !Map SessionId (!Int,!UIDef)				// Previous ui versions to optimize output sent to clients
					, workQueue				:: ![(!Work,!Maybe Timestamp)]
					, world					:: !*World									// The outside world
					}

updateCurrentDateTime :: !*IWorld -> *IWorld

queueWork			:: !(!Work, !Maybe Timestamp)	!*IWorld -> *IWorld
queueUrgentEvaluate	:: !InstanceNo					!*IWorld -> *IWorld
dequeueWork			:: 								!*IWorld -> (!DequeueResult, !*IWorld)
dequeueWorkFilter	:: !(Work -> Bool)				!*IWorld -> (![Work], !*IWorld)

getResponseExpiry	:: !InstanceNo					!*IWorld -> (!Maybe Int, !*IWorld) 

:: DequeueResult = Empty | Work !Work | WorkAt !Timestamp

:: Work	= Evaluate !InstanceNo
		| EvaluateUrgent !InstanceNo
		| TriggerSDSChange !BasicShareId
		| CheckSDS !BasicShareId !Hash (*IWorld -> *(!CheckRes, !*IWorld))

instance FileSystem IWorld

instance registerSDSDependency		InstanceNo	IWorld
instance registerSDSChangeDetection				IWorld
instance reportSDSChange			InstanceNo	IWorld
instance reportSDSChange 			Void		IWorld

//Sync work queue to disk (Only used with CGI wrapper)
saveWorkQueue :: !*IWorld -> *IWorld
restoreWorkQueue :: !*IWorld -> *IWorld
