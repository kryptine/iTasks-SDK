definition module iTasks

/**
* Main iTask module exporting all end user iTask modules 
*/
import	iTasks._Framework.Engine				// iTasks engine
    // iTasks API
    ,   iTasks.SDS.Definition
    ,   iTasks.SDS.Combinators.Core
    ,   iTasks.SDS.Combinators.Common
    ,   iTasks.SDS.Sources.Core
    ,   iTasks.SDS.Sources.Store
    ,   iTasks.SDS.Sources.System
    ,   iTasks.API
	
	//	Miscellaneous machinery
	,	Text.JSON							// JSON is used for serializing/deserializing strings
	,	iTasks._Framework.Generic			// Generic foundation modules
	,   iTasks.UI.Prompt 					// Standard for creating prompts
	,   iTasks.UI.Layout.Common 			// Standard layout patterns
	
	//	API extensions for user  & workflow management
	,	iTasks.API.Extensions.Admin.UserAdmin
	,	iTasks.API.Extensions.Admin.WorkflowAdmin
	
	//StdEnv modules
	,	StdInt
	,	StdBool
	,	StdString
	,	StdList
	,	StdOrdList
	,	StdTuple
	,	StdEnum
	,	StdOverloaded

from StdFunc import id, const, o
from Data.List import instance Functor []
