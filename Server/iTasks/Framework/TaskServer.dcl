definition module iTasks.Framework.TaskServer

from Data.Maybe 		import :: Maybe
from StdFile			import class FileSystem
from TCPIP				import class ChannelEnv, :: IPAddress, :: Timeout
from Internet.HTTP		import :: HTTPRequest, :: HTTPResponse
from System.Time				import :: Timestamp
from iTasks.Framework.IWorld	import :: IWorld
from iTasks.Framework.Task      import :: NetTask, :: BackgroundTask
from iTasks.Framework.Engine    import :: ConnectionType

//Core task server loop
serve :: !Int !NetTask !BackgroundTask (*IWorld -> (!Maybe Timeout,!*IWorld)) *IWorld -> *IWorld

httpService :: !Int !Int ![(!String -> Bool
				,!Bool
				,!(HTTPRequest *IWorld -> (!HTTPResponse,!Maybe ConnectionType, !*IWorld))
				,!(HTTPRequest (Maybe {#Char}) ConnectionType *IWorld -> (!Maybe {#Char}, !Bool, !ConnectionType, !*IWorld))
				,!(HTTPRequest ConnectionType *IWorld -> *IWorld)
				)] -> NetTask
