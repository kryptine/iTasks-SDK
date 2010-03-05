implementation module Config

import Util, JSON, StdMisc

derive JSONEncode Config
derive JSONDecode Config

derive bimap Maybe, (,)

defaultConfig :: Config
defaultConfig =
	{ clientPath	= "Client"
	, staticPath	= ".\\Static"
	, rootPassword	= "root"
	, sessionTime	= 3600
	, serverPort	= 80
	, serverPath	= "/handlers"
	, debug			= False
	}

loadConfig :: !String !*World -> (!Maybe Config, !*World)
loadConfig appName world
	# (content,world)	= readfile (appName +++ "-config.json") world
	| content == ""
		= (Nothing,world)
	| otherwise
		= (fromJSON content,world)
	
storeConfig :: !String !Config !*World -> *World
storeConfig appName config world
	= writefile (appName +++ "-config.json") (toJSON config) world
	