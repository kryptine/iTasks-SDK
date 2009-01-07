definition module StdiTasks

// *********************************************************************************************************************************
// Main iTask pass thru module exporting all End User iTask modules 
// *********************************************************************************************************************************
// iTask & iData Concept and Implementation: (c) 2006,2007,2008 - Rinus Plasmeijer
// *********************************************************************************************************************************
//
import

// iTask End User modules:

			Startup						// basic iTask system creator
		,	iTasksProcessHandling		// creation of iTask Workflow Processes

		,	iTasksEditors				// basic html editors for any type
		,	UserTasks					// tasks for accessing the user database		
		
		,	BasicCombinators			// basic iTask combinators

		,	Combinators					// handy set of additional iTask combinators
		,	PromptingCombinators		// html prompting
		,	LiftingCombinators			// lifting other domains (e.g. iData) to the iTask domain

		
		,	iTasksTimeAndDateHandling	// iTasks triggered by time and date 
		,	iTasksExceptionHandling		// for handling exceptional situations

		,	iTasksDB					// iTask simple DB access
		,	iTasksSettings				// font settings

