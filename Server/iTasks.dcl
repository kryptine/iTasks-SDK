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
	,	SystemData

	//	Basic tasks
	,	CoreTasks					// Core basic tasks
	,	InteractionTasks			// Tasks for interaction with users
	,	DBTasks						// convenience wrapper functions for databases with multiple values of type a
		
	,	ImportTasks					// tasks for importing external data
	,	ExportTasks					// tasks for exporting data	
	,	IntegrationTasks			// Tasks for integration with other systems
	
	//	Task combinators
	,	CoreCombinators				// The core iTask combinators
	,	CommonCombinators			// Set of derived useful iTask combinators
	
	//	Layout tuning
	,	LayoutCombinators
	
	//	Miscellaneous machinery
	,	JSON								// Functions for serializing/deserializing strings
	, 	iTasks.Framework.Shared				// Shared data sources
	,	iTasks.Framework.GenVisualize		// Functions for generating GUIs
	,	iTasks.Framework.GenUpdate			// Functions for updating arbitrary values
	,	iTasks.Framework.GenVerify			// Functions for appending errors and hints to form values
	,	iTasks.Framework.GenRecord			// Functions for manipulating records
	
	//	API extensions for user  & workflow management
	,	UserAdmin
	,	WorkflowAdmin
	
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

//Types
import SystemTypes
from List import instance Functor []
