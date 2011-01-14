implementation module GinConfig

import StdMaybe
import StdFile
import GenPrint
import GenParse

from GinOSUtils import qualified ::Path, appendTrailingSeparator
from Directory import ::Path(..), getCurrentDirectory, pd_StringToPath, pathToPD_String
from clCCall_12 import winFileExists

import CommandLine
from Engine import determineAppName
import iTasks

derive gVisualize GinConfig
derive gUpdate    GinConfig
derive JSONEncode GinConfig
derive JSONDecode GinConfig
	
derive bimap Maybe, (,)

ginDefaultConfig :: *World -> (GinConfig, *World)
ginDefaultConfig world
#(cleanPath, world)   = getCleanPath world
#(iTasksPath, world)  = getITasksPath world
#(projectPath, world) = getProjectPath world
#config = { cleanPath     = cleanPath
          , iTasksPath    = iTasksPath
          , projectPath   = projectPath
          }
=(config, world)
where
	getCleanPath :: *World -> (String, *World)
	getCleanPath world
	# (args,world) = getCommandLine world
	# appPath = hd args
    # ((_, (AbsolutePath diskname steps)), world) = pd_StringToPath appPath world
	# cleanPath = (AbsolutePath diskname (take (length steps - 4) steps))
	= pathToPD_String cleanPath world

	getITasksPath :: *World -> (String, *World)
	getITasksPath world
	#(projectPath, world) = getCurrentDirectory world
	#(AbsolutePath diskname steps) = projectPath
	#iTasksPath = (AbsolutePath diskname (take (length steps - 2) steps))
	= pathToPD_String iTasksPath world

	getProjectPath :: *World -> (String, *World)
	getProjectPath world
	#(projectPath, world) = getCurrentDirectory world
	= pathToPD_String projectPath world

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
# (appName, world) = determineAppName world
= (appName +++ "-gin-config.json", world)

gVerify{|GinConfig|} val vst = worldVerify check val vst
where
	check Nothing iworld = (Nothing, Nothing, iworld)
	check (Just config) iworld =: { world }
	# (mbError, world) = ginCheckConfig config world
	= (Nothing, mbError, { iworld & world = world } )

ginCheckConfig :: !GinConfig !*World -> (Maybe String, *World)
ginCheckConfig config world
| not (winFileExists ('GinOSUtils'.appendTrailingSeparator config.cleanPath +++ "CleanIDE.exe"))
  = (Just "Clean path incorrect", world) 
| not (winFileExists ('GinOSUtils'.appendTrailingSeparator config.iTasksPath +++ "Server\\iTasks.dcl"))
  = (Just "iTasksPath incorrect", world)
= (Nothing, world)

