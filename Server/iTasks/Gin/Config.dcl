definition module iTasks.Gin.Config

from Data.Maybe import :: Maybe

from iTasks.API.Core.SystemTypes import :: InteractionMask, :: VerifyMask, :: VerifyOptions, :: ConsPos, :: StaticVisualizationMode, :: VSt, :: VisualizationResult

//from GinTypes import :: GTypeExpression, :: GTypeDefinition

from iTasks.Framework.GenVisualize import generic gVisualizeText, generic gVisualizeEditor
from Text.JSON import generic JSONEncode, generic JSONDecode, :: JSONNode
from GenEq import generic gEq
from iTasks.Framework.GenUpdate import generic gUpdate, generic gDefault, generic gHeaders, generic gGridRows
from iTasks.Framework.GenVerify import generic gVerify

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
