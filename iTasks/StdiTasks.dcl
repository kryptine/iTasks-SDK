definition module StdiTasks
/**
* Main iTask module exporting all end user iTask modules 
*/
import
		Engine						// basic iTask system creator
	,	iTasksProcessHandling		// creation of iTask Workflow Processes

	,	EditTasks					// basic html editors for any type
	,	UserTasks					// tasks for accessing the user database		
	,	TimeAndDateTasks			// iTasks triggered by time and date 
	,	iTasksDB					// iTask simple DB access
			
	,	BasicCombinators			// basic iTask combinators

	,	Combinators					// handy set of additional iTask combinators
	,	PromptingCombinators		// html prompting
	,	LiftingCombinators			// lifting other domains (e.g. iData) to the iTask domain

	,	ExceptionCombinators		// for handling exceptional situations

	,	iTasksSettings				// font settings