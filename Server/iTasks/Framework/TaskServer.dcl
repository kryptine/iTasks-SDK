definition module iTasks.Framework.TaskServer

from Data.Maybe 		import :: Maybe
from StdFile			import class FileSystem
from TCPIP				import class ChannelEnv, :: IPAddress, :: Timeout
from Internet.HTTP		import :: HTTPRequest, :: HTTPResponse
from System.Time		import :: Timestamp

// Core server
startServer :: !Int 
				(IPAddress *env -> (loc,*env)) ((Maybe {#Char}) loc *env -> *(Maybe {#Char},!Bool, !loc, !*env)) (loc *env -> *env)
				(*env -> (!Maybe Timeout,!*env)) (*env -> (!Bool,!*env)) *env -> *env | ChannelEnv env

// HTTP Server
class HttpServerEnv env
where
    serverTime :: *env -> (!Timestamp,!*env)

instance HttpServerEnv World

startHTTPServer :: !Int !Int
						[(!(String -> Bool)
						 ,!Bool
						 ,!(HTTPRequest *env -> (!HTTPResponse,!Maybe loc,!*env))
						 ,!(HTTPRequest (Maybe {#Char}) loc *env -> (!Maybe {#Char}, !Bool, loc, !*env))
						 ,!(HTTPRequest loc *env -> *env)
						 )] (*env -> (!Maybe Timeout,!*env)) (*env -> (!Bool,!*env)) *env -> *env | ChannelEnv env & HttpServerEnv env

simpleHTTPResponse ::
	(!(String -> Bool),HTTPRequest *env -> (!HTTPResponse,*env))
	->
	(!(String -> Bool),!Bool,!(HTTPRequest *env -> (HTTPResponse, Maybe loc,*env))
							,!(HTTPRequest (Maybe {#Char}) loc *env -> (!Maybe {#Char}, !Bool, loc, !*env))
							,!(HTTPRequest loc *env -> *env))
