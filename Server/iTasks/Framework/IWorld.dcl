definition module iTasks.Framework.IWorld

from System.FilePath			import :: FilePath
from Data.Void				import :: Void
from Data.Map				import :: Map
from Data.Maybe				import :: Maybe
from System.Time				import :: Timestamp
from iTasks.API.Core.SystemTypes		import :: DateTime, :: User, :: Config, :: InstanceNo, :: TaskNo, :: TaskId, :: TaskListItem, :: ParallelTaskType, :: TaskTime, :: SessionId
from iTasks.Framework.UIDefinition		import :: UIDef, :: UIControl
from iTasks.Framework.UIDiff			import :: UIUpdate
from iTasks.Framework.TaskState			import :: TaskListEntry
from Text.JSON				import :: JSONNode
from StdFile			import class FileSystem		
from Data.SharedDataSource		import class registerSDSDependency, class registerSDSChangeDetection, class reportSDSChange, :: CheckRes(..), :: BasicShareId, :: Hash
from iTasks.Framework.TaskServer	import class HttpServerEnv

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
					, localTasks			:: !Map TaskId Dynamic						// The set of local parallel tasks
					, eventRoute			:: !Map TaskId Int							// Index of parallel branches the event is targeted at
					, readShares			:: ![String]								// The IDs of shares from which was read
					, sessions				:: !Map SessionId InstanceNo				// Index of sessions to instance numbers

					, workQueue				:: ![(!Work,!Maybe Timestamp)]
					, uiUpdates				:: !Map SessionId [UIUpdate]				// Updates for the user interfaces of sessions

					, shutdown				:: !Bool									// Flag that signals the server function to shut down
					, world					:: !*World									// The outside world
					}

updateCurrentDateTime :: !*IWorld -> *IWorld

queueWork			:: !(!Work, !Maybe Timestamp)	!*IWorld -> *IWorld
queueUrgentEvaluate	:: !InstanceNo					!*IWorld -> *IWorld
dequeueWork			:: 								!*IWorld -> (!DequeueResult, !*IWorld)
dequeueWorkFilter	:: !(Work -> Bool)				!*IWorld -> (![Work], !*IWorld)

getResponseExpiry	:: !InstanceNo					!*IWorld -> (!Maybe Int, !*IWorld) 

addUIUpdates		:: !SessionId ![UIUpdate] 		!*IWorld -> *IWorld
getUIUpdates		:: !SessionId					!*IWorld -> (![UIUpdate],!*IWorld)

:: DequeueResult = Empty | Work !Work | WorkAt !Timestamp

:: Work	= Evaluate !InstanceNo
		| EvaluateUrgent !InstanceNo
		| TriggerSDSChange !BasicShareId
		| CheckSDS !BasicShareId !Hash (*IWorld -> *(!CheckRes, !*IWorld))

instance FileSystem IWorld

instance HttpServerEnv IWorld

instance registerSDSDependency		InstanceNo	IWorld
instance registerSDSChangeDetection				IWorld
instance reportSDSChange			InstanceNo	IWorld
instance reportSDSChange 			Void		IWorld

//Sync work queue to disk (Only used with CGI wrapper)
saveWorkQueue :: !*IWorld -> *IWorld
restoreWorkQueue :: !*IWorld -> *IWorld
