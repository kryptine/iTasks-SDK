definition module GinConfig

import StdMaybe
import GenEq, GenPrint, GenParse, GenVisualize, GenUpdate, GenMerge 

:: GinConfig =
	{ cleanPath		:: !String
	, iTasksPath    :: !String
	, projectPath	:: !String
	}
	
derive class iTask GinConfig
	
ginDefaultConfig :: *World -> (GinConfig, *World)
ginLoadConfig :: !*World -> (!Maybe GinConfig, !*World)
ginStoreConfig :: !GinConfig !*World -> *World
ginCheckConfig :: !GinConfig !*World -> (Bool, *World)



