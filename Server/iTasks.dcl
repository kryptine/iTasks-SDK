definition module iTasks

/**
* Main iTask module exporting all end user iTask modules 
*/
import	iTasks.Framework.Engine						// basic iTask system creator
	,	iTasks.Framework.EngineWrapperStandalone	// standalone wrapper
	//,	iTasks.Framework.EngineWrapperCGI			// CGI wrapper
	
	,	iTasks.Framework.SerializationGraphCopy		// use serialization via graph_copy
	//,	iTasks.Framework.SerializationDynamicLinker	// use serialization via dynamic linker

	//	System data
	,	iTasks.API.Core.SystemData
	,	iTasks.API.Core.SystemTypes

	//	Basic tasks
	,	iTasks.API.Core.CoreTasks					// Core basic tasks
	,	iTasks.API.Common.InteractionTasks			// Tasks for interaction with users
	,	iTasks.API.Common.DBTasks						// convenience wrapper functions for databases with multiple values of type a
		
	,	iTasks.API.Common.ImportTasks					// tasks for importing external data
	,	iTasks.API.Common.ExportTasks					// tasks for exporting data	
	,	iTasks.API.Core.IntegrationTasks			// Tasks for integration with other systems
	
	//	Task combinators
	,	iTasks.API.Core.CoreCombinators				// The core iTask combinators
	,	iTasks.API.Common.CommonCombinators			// Set of derived useful iTask combinators
	
	//	Layout tuning
	,	iTasks.API.Core.LayoutCombinators
	
	//	Miscellaneous machinery
	,	JSON								// Functions for serializing/deserializing strings
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
from List import instance Functor []
