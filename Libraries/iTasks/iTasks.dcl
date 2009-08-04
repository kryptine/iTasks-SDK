definition module iTasks
/**
* Main iTask module exporting all end user iTask modules 
*/
import	Engine						// basic iTask system creator
	,	EngineWrapperStandalone		// standalone wrapper
	//,	EngineWrapperCGI			// CGI wrapper

	,	UITasks						// basic html editors for any type
	,	UserTasks					// tasks for accessing the user database		
	,	TimeAndDateTasks			// iTasks triggered by time and date 
	,	iTasksDB					// iTask simple DB access
			
	,	CoreCombinators				// The core iTask combinators

	,	CommonCombinators			// Set of additional useful iTask combinators
	,	PromptingCombinators		// Display of messages and prompts
	,	LiftingCombinators			// Lifting of other domains (e.g. iData) to the iTask domain
	,	ProcessCombinators			// Creation an handling of dynamic sub processes

	,	ExceptionCombinators		// Handling exceptional situations
	,	ChangeCombinators			// Changing flows at runtime
	,	ClientCombinators			// Delegating tasks to the client
	,	TuningCombinators			// Fine tuning of tasks
	,	GenBimap
	,	Util
	,	GUICore						// Functions for generating GUIS
	,	GUIWidgets					// Additional GUI widgets
	
	//StdEnv modules
	,	StdInt
	,	StdString
	,	StdList
	,	StdOrdList
	,	StdTuple
	,	StdEnum
	,	StdOverloaded
	,	StdGeneric

	//Generic modules
	,	GenPrint
	,	GenParse

from StdFunc import id, const

//Types
import Types
from TSt		import :: Workflow{..}, :: Change(..), :: ChangeLifeTime(..)
from Time		import :: Time(..)
from TaskTree	import :: TaskCombination(..)

//iTask context restriction
class iTask a
	| gPrint {|*|}
	, gParse {|*|}
	, gVisualize{|*|}
	, gUpdate{|*|} a
