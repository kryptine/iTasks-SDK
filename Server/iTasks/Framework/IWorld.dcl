definition module iTasks.Framework.IWorld

from System.FilePath			import :: FilePath
from Data.Void				import :: Void
from Data.Map				import :: Map
from Data.Maybe				import :: Maybe
from System.Time				import :: Timestamp
from iTasks.API.Core.SystemTypes		import :: DateTime, :: User, :: Config, :: InstanceNo, :: TaskNo, :: TaskId, :: TaskListItem, :: ParallelTaskType, :: TaskTime, :: SessionId
from iTasks.Framework.UIDefinition		import :: UIDef, :: UIControl, :: UIEditletOpts
from iTasks.Framework.UIDiff			import :: UIUpdate, :: UIEditletDiffs
from iTasks.Framework.TaskState			import :: TaskListEntry
from Text.JSON				import :: JSONNode
from StdFile			import class FileSystem		
from Data.SharedDataSource		import class registerSDSDependency, class registerSDSChangeDetection, class reportSDSChange, :: CheckRes(..), :: BasicShareId, :: Hash
from iTasks.Framework.Task import :: ConnectionTask, :: BackgroundTask
from Data.SharedDataSource import :: RWShared
from iTasks.Framework.Shared import :: ReadWriteShared, :: Shared

from Data.Set import :: Set
from Sapl.Linker.LazyLinker import :: LoaderState
from Sapl.Linker.SaplLinkerShared import :: LineType, :: FuncTypeMap
from Sapl.Target.Flavour import :: Flavour
from Sapl.SaplParser import :: ParserState
from TCPIP import :: TCP_Listener, :: TCP_Listener_, :: TCP_RChannel_, :: TCP_SChannel_, :: TCP_DuplexChannel, :: DuplexChannel, :: IPAddress, :: ByteSeq

:: *IWorld		=	{ application			:: !String									// The name of the application	
					, build					:: !String									// The date/time identifier of the application's build
					, serverURL				:: !String									// URL of the server like "//any.com:80"
					, config				:: !Config									// The server configuration
                    , systemDirectories     :: !SystemDirectories                       // Filesystem paths that are used by iTasks
					, taskTime				:: !TaskTime								// The 'virtual' time for the task. Increments at every event
					, timestamp				:: !Timestamp								// The timestamp of the current request	
					, currentLocalDateTime	:: !DateTime								// The local date & time of the current request
					, currentUTCDateTime	:: !DateTime								// The local date & time of the current request
					, currentUser			:: !User									// The current user
					, currentInstance		:: !InstanceNo								// The current evaluated task instance
                    , currentSession        :: !Maybe InstanceNo                        // If we are evaluating a task in response to an event from a session
                    , currentAttachment     :: ![TaskId]                                // The current way the evaluated task instance is attached to other instances
					, nextTaskNo			:: !TaskNo									// The next task number to assign
					, localShares			:: !Map TaskId JSONNode						// The set of locally shared values
					, localLists			:: !Map TaskId [TaskListEntry]				// The set of local parallel task lists
					, localTasks			:: !Map TaskId Dynamic						// The set of local parallel tasks
					, eventRoute			:: !Map TaskId Int							// Index of parallel branches the event is targeted at
					, readShares			:: ![String]								// The IDs of shares from which was read

					, exposedShares			:: !Map String (Dynamic, Shared JSONNode)

					, jsCompilerState 		:: (!LoaderState 							// State of the lazy loader
											   ,!FuncTypeMap							// Function name -> source code mapping
											   ,!Flavour								// Clean flavour for JS compilation
											   ,!Maybe ParserState						// Some information collected by the parser for the code generator
											   ,!Map InstanceNo (Set String))			// Per client information of the names of the already generated functions

                    , editletDiffs          :: !UIEditletDiffs

					, workQueue				:: ![(!Work,!Maybe Timestamp)]
					, uiMessages            :: !Map InstanceNo [UIMessage]				// Messages for communicating with the user interfaces of sessions
                    , connectionValues      :: !Map TaskId Dynamic                      // Temporary task values for low-level connection tasks
					, shutdown				:: !Bool									// Flag that signals the server function to shut down
                    , random                :: [Int]                                    // Infinite random stream
                    , loop                  :: !*MainLoop                               // The mainloop
					, world					:: !*World									// The outside world

                    //Experimental database connection cache
                    , resources             :: !*(Maybe *Resource)
                    , onClient				:: !Bool									// "False" on the server, "True" on the client
					}

:: SystemDirectories =
    { appDirectory			:: !FilePath								// Location of the application's executable
	, dataDirectory			:: !FilePath								// Location of the applications data files
	, sdkDirectory			:: !FilePath								// Location of the iTasks SDK
    , publicWebDirectories  :: ![FilePath]                              // List of directories that contain files that are served publicly by the iTask webserver
    }

:: *MainLoop =
    { done :: !*[MainLoopInstance]
    , todo :: !*[MainLoopInstance]
    }

:: *MainLoopInstance
    = ListenerInstance !Int !*TCP_Listener !ConnectionTask
    | ConnectionInstance !IPAddress !*TCP_DuplexChannel !ConnectionTask !Dynamic
    | BackgroundInstance !BackgroundTask

:: *Resource = Resource | .. //Extensible resource type for caching database connections etc...

updateCurrentDateTime :: !*IWorld -> *IWorld

queueWork			:: !(!Work, !Maybe Timestamp)	!*IWorld -> *IWorld
queueUrgentEvaluate	:: !InstanceNo					!*IWorld -> *IWorld
dequeueWork			:: 								!*IWorld -> (!DequeueResult, !*IWorld)
dequeueWorkFilter	:: !(Work -> Bool)				!*IWorld -> (![Work], !*IWorld)

getResponseExpiry	:: !InstanceNo					!*IWorld -> (!Maybe Int, !*IWorld) 

addUIMessage        :: !InstanceNo !UIMessage       !*IWorld -> *IWorld
getUIMessages		:: !InstanceNo                  !*IWorld -> (![UIMessage],!*IWorld)

:: UIMessage = UIUpdates ![UIUpdate] | UIReset !String

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
