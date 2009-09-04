definition module iTasks
/**
* Main iTask module exporting all end user iTask modules 
*/
import	Engine						// basic iTask system creator
	,	EngineWrapperStandalone		// standalone wrapper
	//,	EngineWrapperCGI			// CGI wrapper

	,	InteractionTasks			// tasks for interaction with users
	,	UserTasks					// tasks for accessing the user database		
	,	TimeAndDateTasks			// iTasks triggered by time and date 
	,	StoreTasks					// iTasks store access
	,	ChangeTasks					// Tasks for changing existing workflows
			
	,	CoreCombinators				// The core iTask combinators

	,	CommonCombinators			// Set of additional useful iTask combinators
	,	LiftingCombinators			// Lifting of other domains (e.g. World) to the iTask domain
	,	ProcessCombinators			// Creation an handling of dynamic sub processes

	,	ExceptionCombinators		// Handling exceptional situations
	,	ClientCombinators			// Delegating tasks to the client
	,	TuningCombinators			// Fine tuning of tasks
	,	GenBimap
	,	Util
	,	GUICore						// Functions for generating GUIS
	
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
from TaskTree	import :: TaskCombination(..)

//iTask context restriction
class iTask a
	| gPrint {|*|}
	, gParse {|*|}
	, gVisualize{|*|}
	, gUpdate{|*|}
	, TC a
