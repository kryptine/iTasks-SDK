definition module iTasks._Framework.TaskServer

from Data.Maybe 		import :: Maybe
from StdFile			import class FileSystem
from TCPIP				import class ChannelEnv, :: IPAddress, :: Timeout
from Internet.HTTP		import :: HTTPRequest, :: HTTPResponse

from System.Time				import :: Timestamp
from Data.Error                 import :: MaybeError
from iTasks.API.Core.Types      import :: TaskId
from iTasks._Framework.IWorld	import :: IWorld
from iTasks._Framework.Task     import :: ConnectionTask, :: BackgroundTask, :: TaskException
from iTasks._Framework.Engine   import :: ConnectionType

//Core task server loop
serve :: !Int !ConnectionTask ![BackgroundTask] (*IWorld -> (!Maybe Timeout,!*IWorld)) *IWorld -> *IWorld

//Dynamically add a listener
addListener :: !TaskId !Int !Bool !ConnectionTask !*IWorld -> (!MaybeError TaskException (),!*IWorld)

//Dynamically add a connection
addConnection :: !TaskId !String !Int !ConnectionTask !*IWorld -> (!MaybeError TaskException (),!*IWorld)
