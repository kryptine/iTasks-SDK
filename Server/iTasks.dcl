definition module iTasks

/**
* Main iTask module exporting all end user iTask modules 
*/
import	Engine						// basic iTask system creator
	,	EngineWrapperStandalone		// standalone wrapper
	//,	EngineWrapperCGI			// CGI wrapper
	
	,	SerializationGraphCopy		// use serialization via graph_copy
	//,	SerializationDynamicLinker	// use serialization via dynamic linker

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
	,	CommonCombinators			// Set of additional useful iTask combinators
	,	TuningCombinators			// Fine tuning of tasks
	
	//	Miscellaneous machinery
	,	JSON						// Functions for serializing/deserializing strings
	,	GenVisualize				// Functions for generating GUIs
	,	GenUpdate					// Functions for updating arbitrary values
	,	GenVerify					// Functions for appending errors and hints to form values
	
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
import SharedCombinators, SystemTypes, GenRecord
from Task import :: Change(..), :: ChangeLifeTime(..)
from List import instance Functor []