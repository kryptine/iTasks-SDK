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
	
	,	DateTimeTasks				// tasks triggered by date and time
	,	ChangeTasks					// Tasks for changing existing workflows
	
	//	Task combinators
	,	CoreCombinators				// The core iTask combinators
	,	CommonCombinators			// Set of additional useful iTask combinators
	,	LiftingCombinators			// Lifting of other domains (e.g. World) to the iTask domain
	,	ExceptionCombinators		// Handling exceptional situations
	,	TuningCombinators			// Fine tuning of tasks
	
	//	Miscellaneous machinery
	,	GenBimap
	,	Util
	,	GenVisualize				// Functions for generating GUIs
	,	GenUpdate					// Functions for updating arbitrary values
	
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

	//Generic modules
	,	GenPrint
	,	GenParse

from StdFunc import id, const

//Types
import Types
from TSt		import :: Workflow{..}, :: Change(..), :: ChangeLifeTime(..)

//iTask context restriction
class iTask a
	| gPrint {|*|}
	, gParse {|*|}
	, gVisualize{|*|}
	, gUpdate{|*|}
	, TC a
