implementation module Config

import Util, JSON

derive JSONEncode Config
derive JSONDecode Config

defaultConfig :: Config
defaultConfig =
	{ clientPath	= "Client"
	, rootPassword	= ""
	, sessionTime	= 3600
	, debug			= False
	}

loadConfig :: !String !*World -> (!Config, !*World)
loadConfig appName world
	# (content,world)	= readfile configfile world
	= case fromJSON content of
		Just config
			= (config, world)
		Nothing
			# world		= writefile configfile (toJSON defaultConfig) world
			= (defaultConfig, world)
where	
	configfile = appName +++ "-config.json"
	
