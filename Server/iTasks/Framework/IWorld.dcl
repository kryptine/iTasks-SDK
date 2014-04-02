definition module iTasks.Framework.IWorld

from System.FilePath		import :: FilePath
from Data.Void				import :: Void
from Data.Map				import :: Map
from Data.Maybe				import :: Maybe
from Data.Set               import :: Set
from StdFile			                import class FileSystem		
from System.Time				        import :: Timestamp
from Text.JSON				            import :: JSONNode
from iTasks.API.Core.Types		        import :: Date, :: Time, :: DateTime, :: User, :: Config, :: InstanceNo, :: TaskNo, :: TaskId, :: TaskListItem, :: ParallelTaskType, :: TaskTime, :: SessionId
from iTasks.Framework.UIDefinition		import :: UIDef, :: UIControl, :: UIEditletOpts
from iTasks.Framework.UIDiff			import :: UIUpdate, :: UIEditletDiffs
from iTasks.Framework.TaskState			import :: TaskListEntry, :: TIMeta
from iTasks.Framework.Task              import :: TaskValue, :: ConnectionTask, :: BackgroundTask
from iTasks.Framework.SDS import :: SDSNotifyRequest, :: BasicShareId
from iTasks.Framework.SDS import :: RWShared, :: ReadWriteShared, :: Shared, :: JSONShared

from Sapl.Linker.LazyLinker import :: LoaderState
from Sapl.Linker.SaplLinkerShared import :: LineType, :: FuncTypeMap
from Sapl.Target.Flavour import :: Flavour
from Sapl.SaplParser import :: ParserState
from TCPIP import :: TCP_Listener, :: TCP_Listener_, :: TCP_RChannel_, :: TCP_SChannel_, :: TCP_DuplexChannel, :: DuplexChannel, :: IPAddress, :: ByteSeq

:: *IWorld		=	{ server                :: !ServerInfo                              // Static server info, initialized at startup
					, config				:: !Config									// Server configuration
                    , clocks                :: !SystemClocks                            // Server side clocks
                    , current               :: !TaskEvalState                           // Shared state during task evaluation

                    , random                :: [Int]                                    // Infinite random stream

                    , sdsNotifyRequests     :: ![SDSNotifyRequest]                      // Notification requests from previously read sds's
					, exposedShares			:: !Map String (Dynamic, JSONShared)        // Shared source

					, jsCompilerState 		:: (!LoaderState 							// State of the lazy loader
											   ,!FuncTypeMap							// Function name -> source code mapping
											   ,!Flavour								// Clean flavour for JS compilation
											   ,!Maybe ParserState						// Some information collected by the parser for the code generator
											   ,!Map InstanceNo (Set String))			// Per client information of the names of the already generated functions


                    , ti                    :: ![TIMeta]                                // Task instance table
                    , nextInstanceNo        :: !InstanceNo                              // Next task instance number
                    , refreshQueue          :: ![InstanceNo]                            // Instances that need refreshing
					, uiUpdates             :: !Map InstanceNo [UIUpdate]				// (Instance output)

                    , io                    :: !*IOTasks                                // The low-level input/output tasks
                    , ioValues              :: !Map TaskId IOTaskValue                  // Task values of low-level tasks, indexed by the high-level taskid that it is linked to

					, world					:: !*World									// The outside world

                    //Experimental database connection cache
                    , resources             :: !*(Maybe *Resource)
                    , onClient				:: !Bool									// "False" on the server, "True" on the client
					, shutdown				:: !Bool									// Flag that signals the server function to shut down
					}

:: ServerInfo =
    { serverName      :: !String				// The name of the server application
	, serverURL		  :: !String				// URL of the server like "//any.com:80"
	, buildID		  :: !String				// The date/time identifier of the server's build
    , paths           :: !SystemPaths           // Filesystem paths that are used by iTasks
    , customCSS       :: !Bool                  // Does the application use a custom css stylesheet
    }

:: SystemPaths =
    { appDirectory			:: !FilePath		// Location of the application's executable
	, dataDirectory			:: !FilePath		// Location of the applications data files
	, sdkDirectory			:: !FilePath		// Location of the iTasks SDK
    , publicWebDirectories  :: ![FilePath]      // List of directories that contain files that are served publicly by the iTask webserver
    }

:: SystemClocks =
    { localDate             :: !Date
    , localTime             :: !Time
    , utcDate               :: !Date
    , utcTime               :: !Time
    }

:: TaskEvalState =
	{ timestamp				:: !Timestamp								// The timestamp of the current request	
    , taskTime				:: !TaskTime								// The 'virtual' time for the task. Increments at every event
	, taskInstance		    :: !InstanceNo								// The current evaluated task instance
    , sessionInstance       :: !Maybe InstanceNo                        // If we are evaluating a task in response to an event from a session
    , attachmentChain       :: ![TaskId]                                // The current way the evaluated task instance is attached to other instances
    , nextTaskNo			:: !TaskNo									// The next task number to assign
    , user			        :: !User									// The current user
    , localShares			:: !Map TaskId JSONNode						// The set of locally shared values
    , localLists			:: !Map TaskId [TaskListEntry]				// The set of local parallel task lists
    , localTasks			:: !Map TaskId Dynamic						// The set of local parallel tasks
    , eventRoute			:: !Map TaskId Int							// Index of parallel branches the event is targeted at
    , readShares			:: ![String]								// The IDs of shares from which was read
    , editletDiffs          :: !UIEditletDiffs                          // Diffs of editlets
    }

:: *IOTasks =
    { done :: !*[IOTaskInstance]
    , todo :: !*[IOTaskInstance]
    }

:: *IOTaskInstance
    = ListenerInstance !Int !*TCP_Listener !ConnectionTask
    | ConnectionInstance !IPAddress !*TCP_DuplexChannel !ConnectionTask !Dynamic
    | BackgroundInstance !BackgroundTask

:: IOTaskValue
    = IOValue !Dynamic !Bool
    | IOException !String

:: *Resource = Resource | .. //Extensible resource type for caching database connections etc...

//Internally used clock shares
iworldLocalDate :: Shared Date
iworldLocalTime :: Shared Time
iworldUTCDate   :: Shared Date
iworldUTCTime   :: Shared Time

//Update the clock shares
updateClocks    :: !*IWorld -> *IWorld

getResponseExpiry	:: !InstanceNo					!*IWorld -> (!Maybe Int, !*IWorld) 

addUIUpdates    :: !InstanceNo ![UIUpdate]  !*IWorld -> *IWorld
popUIUpdates    :: ![InstanceNo]            !*IWorld -> (![(!InstanceNo,![UIUpdate])],!*IWorld)
clearUIUpdates  :: !InstanceNo              !*IWorld -> *IWorld

instance FileSystem IWorld
