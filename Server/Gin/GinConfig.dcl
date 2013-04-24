definition module GinConfig

from Maybe import :: Maybe

from SystemTypes import :: InteractionMask, :: VerifyMask, :: VerifyOptions, :: ConsPos, :: StaticVisualizationMode, :: VSt, :: VisualizationResult

//from GinTypes import :: GTypeExpression, :: GTypeDefinition

from GenVisualize import generic gVisualizeText, generic gVisualizeEditor
from JSON import generic JSONEncode, generic JSONDecode, :: JSONNode
from GenEq import generic gEq
from GenUpdate import generic gUpdate, generic gDefault, generic gHeaders, generic gGridRows
from GenVerify import generic gVerify

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
