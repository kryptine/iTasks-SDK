implementation module GinConfig

import StdMaybe
import Util
import JSON

from clCCall_12 import winFileExists
import GinOSUtils

import GenPrint
import GenParse
from Engine import determineAppName

derive class iTask GinConfig

derive bimap Maybe, (,)

cleanIDE :== "CleanIDE.exe"

ginDefaultConfig :: *World -> (GinConfig, *World)
ginDefaultConfig world
#config = { cleanPath = "c:\\clean"
          , iTasksPath = "c:\\clean\\iTasks-SDK"
          , projectPath   = "C:\\clean\\iTasks-SDK\\Examples\\Gin"
          }
=(config, world)

ginLoadConfig :: !*World -> (!Maybe GinConfig, !*World)
ginLoadConfig world
	#(filename, world) = ginConfigFilename world
	#(content,world) = readfile filename world
	| content == ""
	= (Nothing,world)
	| otherwise
	= (fromJSON (fromString content),world)
	
ginStoreConfig :: !GinConfig !*World -> *World
ginStoreConfig config world
	#(filename, world) = ginConfigFilename world
	= writefile filename (toString (toJSON config)) world

ginConfigFilename :: *World -> (!String, *World) 
ginConfigFilename world
#(appName, world) = determineAppName world
=(appName +++ "-gin-config.json", world)

ginCheckConfig :: !GinConfig !*World -> (Bool, *World)
ginCheckConfig config world = (winFileExists (config.cleanPath +/+ cleanIDE), world)


