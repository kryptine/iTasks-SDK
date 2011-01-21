definition module iTasks
/**
* Main iTask module exporting all end user iTask modules 
*/
import	Engine						// basic iTask system creator
	,	EngineWrapperStandalone		// standalone wrapper
	//,	EngineWrapperCGI			// CGI wrapper

	//	Basic tasks
	,	InteractionTasks			// tasks for interaction with users
	,	SystemTasks					// tasks for interaction with the iTasks system itself
	,	StoreTasks					// tasks for accessing the generic store
	
	,	UserDBTasks					// tasks for accessing the user database
	,	SessionDBTasks				// tasks for accessing the session database
	,	ProcessDBTasks				// tasks for accessing the process database
	
	,	ImportTasks					// tasks for importing external data
	,	ExportTasks					// tasks for exporting data
	
	,	DateTimeTasks				// tasks triggered by date and time
	,	ChangeTasks					// Tasks for changing existing workflows
	
	,	OSTasks						// tasks for OS operations like reading/writing files or calling external processes
	
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

from StdFunc import id, const

//Types
import Types
from TSt		import :: Workflow{..}
from TaskTree	import :: TaskParallelType(..), :: Change(..), :: ChangeLifeTime(..)
