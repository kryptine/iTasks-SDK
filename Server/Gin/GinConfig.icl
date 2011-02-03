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

derive gEq        	GinConfig	
derive gVisualize 	GinConfig
derive gUpdate    	GinConfig
derive gDefaultMask	GinConfig
derive JSONEncode 	GinConfig
derive JSONDecode 	GinConfig
	
derive bimap Maybe, (,)

ginDefaultConfig :: *World -> (GinConfig, *World)
ginDefaultConfig world
# (cleanPath, world)	= getCleanPath world
# (iTasksPath, world)	= getITasksPath world
# (tempPath, world)		= getTempPath world
# config = 	{ cleanPath		= cleanPath
          	, iTasksPath	= iTasksPath
			, tempPath		= tempPath
			}
= (config, world)
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
	# (currentPath, world) = getCurrentDirectory world
	# (AbsolutePath diskname steps) = currentPath
	# iTasksPath = (AbsolutePath diskname (take (length steps - 2) steps))
	= pathToPD_String iTasksPath world

	getTempPath :: *World -> (String, *World)
	getTempPath world
	# (currentPath, world) = getCurrentDirectory world
	# (AbsolutePath diskname steps) = currentPath
	= pathToPD_String (AbsolutePath diskname (steps ++ [PathDown "Temp"])) world

ginLoadConfig :: !*World -> (!Maybe GinConfig, !*World)
ginLoadConfig world
	# (filename, world) = ginConfigFilename world
	# (content,world) = readfile filename world
	| content == ""
	= (Nothing,world)
	| otherwise
	= (fromJSON (fromString content),world)
	
ginStoreConfig :: !GinConfig !*World -> *World
ginStoreConfig config world
	# (filename, world) = ginConfigFilename world
	= writefile filename (toString (toJSON config)) world

ginConfigFilename :: *World -> (!String, *World) 
ginConfigFilename world
# (appName, world) = determineAppName world
= (appName +++ "-gin-config.json", world)

gVerify{|GinConfig|} val vst = customWorldVerify Nothing check val vst
where
	check config iworld =: { world }
	# (mErr,world) = ginCheckConfig config world 
	# wpr = case mErr of
		Nothing  -> WPRValid Nothing
		Just err -> WPRInvalid err
	= (wpr, { iworld & world = world } )

ginCheckConfig :: !GinConfig !*World -> (Maybe String, *World)
ginCheckConfig config world
| not (winFileExists ('GinOSUtils'.appendTrailingSeparator config.cleanPath +++ "CleanIDE.exe"))
  = (Just "Clean path incorrect", world) 
| not (winFileExists ('GinOSUtils'.appendTrailingSeparator config.iTasksPath +++ "Server\\iTasks.dcl"))
  = (Just "iTasks path incorrect", world)
| not (winFileExists config.tempPath)
  = (Just "Temp path incorrect", world)
= (Nothing, world)