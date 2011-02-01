definition module GinConfig

import StdMaybe
import GenEq, GenPrint, GenParse, GenVisualize, GenUpdate

:: GinConfig =
	{ cleanPath		:: !String
	, iTasksPath    :: !String
	, tempPath		:: !String
	}

derive gEq        GinConfig	
derive gVisualize GinConfig
derive gUpdate    GinConfig
derive gVerify    GinConfig
derive JSONEncode GinConfig
derive JSONDecode GinConfig

ginDefaultConfig :: *World -> (GinConfig, *World)
ginLoadConfig :: !*World -> (!Maybe GinConfig, !*World)
ginStoreConfig :: !GinConfig !*World -> *World
ginCheckConfig :: !GinConfig !*World -> (Maybe String, *World)


