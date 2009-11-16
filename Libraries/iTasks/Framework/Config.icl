implementation module Config

import Util, JSON, StdMisc

derive JSONEncode Config
derive JSONDecode Config

defaultConfig :: Config
defaultConfig =
	{ clientPath	= "Client"
	, rootPassword	= ""
	, sessionTime	= 3600
	, serverPort	= 80
	, serverPath	= "/handlers"
	, debug			= False
	}

loadConfig :: !String !*World -> (!Config, !*World)
loadConfig appName world
	# (content,world)	= readfile configfile world
	| content == ""
		# world		= writefile configfile (toJSON defaultConfig) world
		= (defaultConfig, world)
	| otherwise
		= case fromJSON content of
			Just config
				= (config, world)
			Nothing
				= abort ("iTasks: Failed to read configfile: " +++ configfile)
where	
	configfile = appName +++ "-config.json"
	
