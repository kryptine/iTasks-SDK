definition module iTasks.Internal.TaskServer

from Data.Maybe 		import :: Maybe
from StdFile			import class FileSystem
from TCPIP				import class ChannelEnv, :: IPAddress, :: Timeout
from Internet.HTTP		import :: HTTPRequest, :: HTTPResponse
from System.FilePath    import :: FilePath

from System.Process           import :: ProcessPtyOptions
from Data.Error               import :: MaybeError
from iTasks.WF.Definition     import :: TaskId
from iTasks.Internal.IWorld	  import :: IWorld
from iTasks.Internal.Task     import :: ExternalProcessTask, :: ConnectionTask, :: TaskException
from iTasks.Engine            import :: TaskWrapper

//Core task server loop
serve :: ![TaskWrapper] ![(!Int,!ConnectionTask)] (*IWorld -> (!Maybe Timeout,!*IWorld)) *IWorld -> *IWorld

//Dynamically add a listener
addListener :: !TaskId !Int !Bool !ConnectionTask !*IWorld -> (!MaybeError TaskException (),!*IWorld)

//Dynamically add a connection
addConnection :: !TaskId !String !Int !ConnectionTask !*IWorld -> (!MaybeError TaskException Dynamic,!*IWorld)

//Dynamically add an external process
addExternalProc :: !TaskId !FilePath ![String] !(Maybe FilePath) !ExternalProcessTask (Maybe ProcessPtyOptions) !IWorld -> (!MaybeError TaskException Dynamic, !*IWorld)
