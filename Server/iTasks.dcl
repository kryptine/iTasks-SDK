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
	,	InteractionTasks			// tasks for interaction with users
	,	DBTasks						// convenience wrapper functions for databases with multiple values of type a
	
	,	UserDBTasks					// tasks for accessing the user database
	,	ProcessDBTasks				// tasks for accessing the process database
	
	,	ImportTasks					// tasks for importing external data
	,	ExportTasks					// tasks for exporting data
	
	,	IntegrationTasks			// Tasks for integration with other systems
	
	//	Task combinators
	,	CoreCombinators				// The core iTask combinators
	,	CommonCombinators			// Set of additional useful iTask combinators
	,	ExceptionCombinators		// Handling exceptional situations
	,	TuningCombinators			// Fine tuning of tasks
	
	//	Miscellaneous machinery
	,	JSON						// Functions for serializing/deserializing strings
	,	GenVisualize				// Functions for generating GUIs
	,	GenUpdate					// Functions for updating arbitrary values
	,	GenVerify					// Functions for appending errors and hints to form values
	
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
import Types, GenRecord
from TSt				import :: Workflow{..}
from Task				import :: Change(..), :: ChangeLifeTime(..)
from Shared				import :: Shared, :: ReadOnlyShared, :: SymmetricShared, mapShared, toReadOnlyShared, >+<, |+<, >+|, |+|, >&<, symmetricLens