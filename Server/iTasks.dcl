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
	,	InteractionTasks			// core tasks for interaction with users
	,	OutputTasks					// tasks for outputting informating to users
	,	InputTasks					// tasks for letting users input information
	,	UpdateTasks					// tasks for letting users update information
	,	SharedTasks					// tasks for accessing the generic store
	
	,	UserDBTasks					// tasks for accessing the user database
	,	ProcessDBTasks				// tasks for accessing the process database
	
	,	ImportTasks					// tasks for importing external data
	,	ExportTasks					// tasks for exporting data
	
	,	ChangeTasks					// Tasks for changing existing workflows
	
	,	IntegrationTasks			// Tasks for integration with other systems
	
	//	Task combinators
	,	CoreCombinators				// The core iTask combinators
	,	CommonCombinators			// Set of additional useful iTask combinators
	,	LiftingCombinators			// Lifting of other domains (e.g. World) to the iTask domain
	,	ExceptionCombinators		// Handling exceptional situations
	,	TuningCombinators			// Fine tuning of tasks
	
	//	Miscellaneous machinery
	,	Util
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
	,	StdArray
	,	StdGeneric
	,	StdDynamic

from StdFunc import id, const, o

//Types
import Types, GenRecord
from TSt				import :: Workflow{..}
from Task				import :: Change(..), :: ChangeLifeTime(..)
from Shared				import :: Shared, :: ReadOnlyShared, :: SymmetricShared, mapShared, toReadOnlyShared, >+<, |+<, >+|, |+|, >&<, symmetricLens