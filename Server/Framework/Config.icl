implementation module Config

import StdFile, Util, Error, File, JSON

derive JSONEncode Config
derive JSONDecode Config
derive bimap Maybe, (,)

defaultConfig :: Config
defaultConfig =
	{ clientPath		= "Client"
	, staticPath		= ".\\Static"
	, rootPassword		= "root"
	, rootEmail			= "root@localhost"
	, sessionTime		= 3600
	, serverPort		= 80
	, serverPath		= "/services"
	, debug				= False
	, smtpServer		= "localhost"
	, generalWorkflows	= False
	}

loadConfig :: !String !*World -> (!Maybe Config, !*World)
loadConfig appName world
	# (res,world) = readFile (appName +++ "-config.json") world
	| isError res = (Nothing, world)
	= (fromJSON (fromString (fromOk res)),world)
	
storeConfig :: !String !Config !*World -> *World
storeConfig appName config world
	# (_, world) = writeFile (appName +++ "-config.json") (toString (toJSON config)) world
	= world
	