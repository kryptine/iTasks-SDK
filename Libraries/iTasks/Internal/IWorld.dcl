definition module iTasks.Internal.IWorld

from System.FilePath		import :: FilePath
from Data.Map				import :: Map
from Data.Maybe				import :: Maybe
from Data.Error 			import :: MaybeError(..), :: MaybeErrorString(..)
from Data.Set               import :: Set
from StdFile			                import class FileSystem		
from System.Time				        import :: Timestamp
from Text.JSON				            import :: JSONNode
from System.Process         import :: ProcessHandle, :: ProcessIO
from iTasks.UI.Definition				import :: UI, :: UINodeType
from iTasks.Internal.TaskState		import :: ParallelTaskState, :: TIMeta, :: DeferredJSON
from iTasks.Internal.Task             import :: ExternalProcessTask, :: ConnectionTask, :: BackgroundTask
from iTasks.Internal.TaskEval         import :: TaskTime

from iTasks.WF.Definition import :: TaskValue, :: Event, :: TaskId, :: InstanceNo, :: TaskNo
from iTasks.WF.Combinators.Core import :: ParallelTaskType, :: TaskListItem 
from iTasks.SDS.Definition import :: SDS, :: RWShared, :: ReadWriteShared, :: Shared
from iTasks.Internal.SDS import :: SDSNotifyRequest, :: JSONShared 
from iTasks.Extensions.DateTime import :: Time, :: Date, :: DateTime

from Sapl.Linker.LazyLinker import :: LoaderState
from Sapl.Linker.SaplLinkerShared import :: LineType, :: FuncTypeMap
from Sapl.Target.Flavour import :: Flavour
from Sapl.SaplParser import :: ParserState
from TCPIP import :: TCP_Listener, :: TCP_Listener_, :: TCP_RChannel_, :: TCP_SChannel_, :: TCP_DuplexChannel, :: DuplexChannel, :: IPAddress, :: ByteSeq

CLEAN_HOME_VAR	:== "CLEAN_HOME"

:: *IWorld		=	{ server                :: !ServerInfo                              // Static server info, initialized at startup
					, config				:: !Config									// Server configuration
                    , clocks                :: !SystemClocks                            // Server side clocks
                    , current               :: !TaskEvalState                           // Shared state during task evaluation

                    , random                :: [Int]                                    // Infinite random stream

                    , sdsNotifyRequests     :: ![SDSNotifyRequest]                      // Notification requests from previously read sds's
                    , memoryShares          :: !Map (String,String) Dynamic             // Run-time memory shares
                    , cachedShares          :: !ShareCache                              // Cached json file shares
					, exposedShares			:: !Map String (Dynamic, JSONShared)        // Shared source
					, jsCompilerState 		:: !Maybe JSCompilerState 					// Sapl to Javascript compiler state

	                , ioTasks               :: !*IOTasks                                // The low-level input/output tasks
                    , ioStates              :: !IOStates                                // Results of low-level io tasks, indexed by the high-level taskid that it is linked to

					, world					:: !*World									// The outside world

                    //Experimental database connection cache
                    , resources             :: !*(Maybe *Resource)
                    , onClient				:: !Bool									// "False" on the server, "True" on the client
					, shutdown				:: !Maybe Int                               // Signals the server function to shut down, the int will be set as exit code
					}

:: Config =
	{ sessionTime		:: !Int		//* Time (in seconds) before inactive sessions are garbage collected. Default is 3600 (one hour).
	, smtpServer		:: !String	//* The smtp server to use for sending e-mails
	}

:: ServerInfo =
    { serverName      :: !String				// The name of the server application
	, serverURL		  :: !String				// URL of the server like "//any.com:80"
	, buildID		  :: !String				// The date/time identifier of the server's build
    , paths           :: !SystemPaths           // Filesystem paths that are used by iTasks
    }

:: SystemPaths =
    { appDirectory			:: !FilePath		// Location of the application's executable
	, dataDirectory			:: !FilePath		// Location of the application's data files
    , webDirectory          :: !FilePath        // List of directories that contain files that are served publicly by the iTask webserver
	, saplDirectory 		:: !FilePath 		// Location of the application's sapl files
    }

:: SystemClocks =
    { timestamp 			:: !Timestamp
	, localDate             :: !Date
    , localTime             :: !Time
    , utcDate               :: !Date
    , utcTime               :: !Time
    }

// share cached used for json stores (Store.cachedJSONFileStore) and dynamic string stores (Store.cachedDynamicStringFileStore)
:: ShareCache :== Map (String, String) (Dynamic, Bool, Maybe CachedValue)
:: CachedValue = CachedJSONValue DeferredJSON | CachedDynamicValue 

:: JSCompilerState =
	{ loaderState 			:: !LoaderState							// State of the lazy loader
	, functionMap 			:: !FuncTypeMap 						// Function name -> source code mapping
 	, flavour 				:: !Flavour 							// Clean flavour for JS compilation
	, parserState 			:: !Maybe ParserState 					// Some information collected by the parser for the code generator
	, skipMap 				:: !Map InstanceNo (Set String) 		// Per client information of the names of the already generated functions
	}

:: TaskEvalState =
    { taskTime				 :: !TaskTime							// The 'virtual' time for the task. Increments at every event
	, taskInstance		     :: !InstanceNo							// The current evaluated task instance
    , sessionInstance        :: !Maybe InstanceNo                   // If we are evaluating a task in response to an event from a session
    , attachmentChain        :: ![TaskId]                           // The current way the evaluated task instance is attached to other instances
    , nextTaskNo			 :: !TaskNo								// The next task number to assign
    }

:: *IOTasks =
    { done :: !*[IOTaskInstance]
    , todo :: !*[IOTaskInstance]
    }

:: *IOTaskInstance
    = ListenerInstance        !ListenerInstanceOpts !*TCP_Listener
    | ConnectionInstance      !ConnectionInstanceOpts !*TCP_DuplexChannel
    | ExternalProcessInstance !ExternalProcessInstanceOpts !ProcessHandle !ProcessIO
    | BackgroundInstance      !BackgroundInstanceOpts !BackgroundTask

:: ListenerInstanceOpts =
    { taskId                :: !TaskId          //Reference to the task that created the listener
    , nextConnectionId      :: !ConnectionId    
    , port                  :: !Int
    , connectionTask        :: !ConnectionTask
    , removeOnClose         :: !Bool            //If this flag is set, states of connections accepted by this listener are removed when the connection is closed
    }

:: ConnectionInstanceOpts =
    { taskId                :: !TaskId          //Reference to the task that created the connection
    , connectionId          :: !ConnectionId    //Unique connection id (per listener/outgoing connection)
    , remoteHost            :: !IPAddress      
    , connectionTask        :: !ConnectionTask  //The io task definition that defines how the connection is handled
    , removeOnClose         :: !Bool            //If this flag is set, the connection state is removed when the connection is closed
    }

:: ConnectionId             :== Int

:: ExternalProcessInstanceOpts =
    { taskId                :: !TaskId              //Reference to the task that started the external process
    , connectionId          :: !ConnectionId        //Unique connection id (per listener/outgoing connection)     
    , externalProcessTask   :: !ExternalProcessTask //The io task definition that defines how the process IO is handled
    }

:: BackgroundInstanceOpts =
    { bgInstId              :: !BackgroundTaskId
    }

:: BackgroundTaskId         :== Int


:: IOStates :== Map TaskId IOState
:: IOState
    = IOActive      !(Map ConnectionId (!Dynamic,!Bool)) // Bool: stability
    | IODestroyed   !(Map ConnectionId (!Dynamic,!Bool)) // Bool: stability
    | IOException   !String
:: IOConnectionState =
    { connectionTaskState   :: !Dynamic //The persisted local state of the connection task that handles the connection
    , closed                :: !Bool
    }

:: *Resource = Resource | .. //Extensible resource type for caching database connections etc...

//Creation and destruction of the iworld
/**
* Creates and initializes the IWorld state
*
* @param The application's name
* @param The application's path (e.g. to executable).
* @param The path where static web assets can be found (optional)
* @param The path where the iTasks data store is located (optional)
* @param Path to where the applications's SAPL files are stored (optional)
* @param The world
*
* @return An initialized iworld
*/
createIWorld :: !String FilePath !(Maybe FilePath) !(Maybe FilePath) !(Maybe FilePath) !*World -> *IWorld

/**
* Initialize the SAPL->JS compiler state
* 
*/
initJSCompilerState :: *IWorld -> *(!MaybeErrorString (), !*IWorld)

/**
* Destroys the iworld state
*/
destroyIWorld :: !*IWorld -> *World

//Internally used clock shares
iworldTimestamp :: Shared Timestamp
iworldLocalDate :: Shared Date
iworldLocalTime :: Shared Time
iworldUTCDate   :: Shared Date
iworldUTCTime   :: Shared Time

instance FileSystem IWorld
