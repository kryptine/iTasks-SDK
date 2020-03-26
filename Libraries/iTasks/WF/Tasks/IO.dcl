definition module iTasks.WF.Tasks.IO
/**
* This modules provides tasks that support interaction with other systems.
* Either by running external programs, creating network clients and servers, or exchanging files
*/
from TCPIP import :: Timeout
import iTasks.WF.Definition
import iTasks.SDS.Definition
import System.OS
from iTasks.Internal.IWorld import :: ConnectionId
from System.FilePath import :: FilePath
from System.Process import :: ProcessPtyOptions
from Data.Error import :: MaybeError, :: MaybeErrorString

:: ConnectionHandlers l r w =
    { onConnect         :: !(ConnectionId String   r -> (MaybeErrorString l, Maybe w, [String], Bool))
    , onData            :: !(             String l r -> (MaybeErrorString l, Maybe w, [String], Bool))
    , onShareChange     :: !(                    l r -> (MaybeErrorString l, Maybe w, [String], Bool))
    , onDisconnect      :: !(                    l r -> (MaybeErrorString l, Maybe w                ))
    , onDestroy         :: !(                    l   -> (MaybeErrorString l,          [String]      ))
	}

/**
 * On windows, a graceful exit has exitcode 0.
 * On posix, the application can exit gracefully with signal 15 (SIGTERM)
 */
externalProcessGraceful :== IF_POSIX 15 0

/**
 * Execute an external process. Data placed in the stdin sds is sent to the process, data received is placed in the (stdout, stderr) sds.
 *
 * @param Poll rate
 * @param Path to executable
 * @param Command line arguments
 * @param Startup directory
 * @param Exit code (windows) or signal (posix) to send when destroying
 * @param Pseudotty settings
 * @param Stdin queue
 * @param (stdout, stderr) queue
 * @result Task returning the exit code on termination
 */
externalProcess :: !Timespec !FilePath ![String] !(Maybe FilePath) !Int !(Maybe ProcessPtyOptions) !(Shared sds1 [String]) !(Shared sds2 ([String], [String])) -> Task Int | RWShared sds1 & RWShared sds2

/**
* Connect to an external system using TCP. This task's value becomes stable when the connection is closed
* @param Hostname
* @param Port
* @param The timeout (in ms) for opening the connection
* @param A reference to shared data the task has access to
* @param The event handler functions
*/
tcpconnect :: !String !Int !(Maybe Timeout) !(sds () r w) (ConnectionHandlers l r w) -> Task l | iTask l & iTask r & iTask w & RWShared sds
/**
* Listen for connections from external systems using TCP.
* @param Port
* @param Remove closed connections. If this is true, closed connections are removed from the task value, if not they are kept in the list
* @param A reference to shared data the task has access to
* @param Initialization function: function that is called when a new connection is established
* @param Communication function: function that is called when data arrives, the connection is closed or the observed share changes.
*/
tcplisten :: !Int !Bool !(sds () r w) (ConnectionHandlers l r w) -> Task [l] | iTask l & iTask r & iTask w & RWShared sds
