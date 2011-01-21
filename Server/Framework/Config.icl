implementation module Config

import Util, JSON, StdMisc

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
	# (content,world)	= readfile (appName +++ "-config.json") world
	| content == ""
		= (Nothing,world)
	| otherwise
		= (fromJSON (fromString content),world)
	
storeConfig :: !String !Config !*World -> *World
storeConfig appName config world
	= writefile (appName +++ "-config.json") (toString (toJSON config)) world
	