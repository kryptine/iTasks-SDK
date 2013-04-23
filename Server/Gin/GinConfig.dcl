definition module GinConfig

import Maybe, HTML, UIDefinition

import iTasks
from iTasks import class iTask, generic gVisualizeText, generic gVisualizeEditor, generic gUpdate, generic gVerify, generic JSONEncode, generic JSONDecode, generic gEq

:: GinConfig =
	{ cleanPath		:: !String
	, iTasksPath    :: !String
	, userPath		:: !String
	, searchPaths	:: ![String]
	}

derive gEq        		GinConfig	
derive gVisualizeText 	GinConfig
//derive gVisualizeHtml 	GinConfig TODO
derive gVisualizeEditor	GinConfig
derive gUpdate    		GinConfig
//derive gDefaultMask		GinConfig TODO
derive gVerify    		GinConfig
derive JSONEncode 		GinConfig
derive JSONDecode 		GinConfig

ginDefaultConfig :: !*World -> (GinConfig, *World)
ginLoadConfig :: !*World -> (!Maybe GinConfig, !*World)
ginStoreConfig :: !GinConfig !*World -> *World
ginCheckConfig :: !GinConfig !*World -> (Maybe String, *World)
