definition module iTasks

/**
* Main iTask module exporting all end user iTask modules 
*/
import	iTasks.Framework.Engine						// basic iTask system creator
	,	iTasks.Framework.EngineWrapperStandalone	// standalone wrapper
	//,	iTasks.Framework.EngineWrapperCGI			// CGI wrapper
	
	,	iTasks.Framework.SerializationGraphCopy		// use serialization via graph_copy
	//,	iTasks.Framework.SerializationDynamicLinker	// use serialization via dynamic linker

    // iTasks API
    ,   iTasks.API
	
	//	Miscellaneous machinery
	,	Text.JSON							// Functions for serializing/deserializing strings
	, 	iTasks.Framework.Shared				// Shared data sources
	,	iTasks.Framework.GenVisualize		// Functions for generating GUIs
	,	iTasks.Framework.GenUpdate			// Functions for updating arbitrary values
	,	iTasks.Framework.GenVerify			// Functions for appending errors and hints to form values
	,	iTasks.Framework.GenRecord			// Functions for manipulating records
	
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
