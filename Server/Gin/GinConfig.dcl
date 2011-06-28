definition module GinConfig

import Maybe

from iTasks import ::JSONNode, ::VerSt, ::UpdateMask, ::USt, ::UpdateMode, ::VSt, ::Visualization
from iTasks import class iTask, generic gVisualize, generic gUpdate, generic gDefaultMask, generic gVerify, generic JSONEncode, generic JSONDecode, generic gEq

:: GinConfig =
	{ cleanPath		:: !String
	, iTasksPath    :: !String
	, userPath		:: !String
	, searchPaths	:: ![String]
	}

derive gEq        	GinConfig	
derive gVisualize 	GinConfig
derive gUpdate    	GinConfig
derive gDefaultMask	GinConfig
derive gVerify    	GinConfig
derive JSONEncode 	GinConfig
derive JSONDecode 	GinConfig

ginDefaultConfig :: !*World -> (GinConfig, *World)
ginLoadConfig :: !*World -> (!Maybe GinConfig, !*World)
ginStoreConfig :: !GinConfig !*World -> *World
ginCheckConfig :: !GinConfig !*World -> (Maybe String, *World)
