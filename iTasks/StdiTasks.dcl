definition module StdiTasks

// *********************************************************************************************************************************
// Main iTask pass thru module exporting all End User iTask modules 
// *********************************************************************************************************************************
// iTask & iData Concept and Implementation: (c) 2006,2007,2008 - Rinus Plasmeijer
// *********************************************************************************************************************************
//

import

// iTask End User modules:

			iTasksHandler				// iTask main module and core engine
		,	iTasksProcessHandling		// creation of iTask Workflow Processes

		,	iTasksEditors				// basic html editors for any type
		,	iTasksHtmlSupport			// html prompting
		
		,	iTasksBasicCombinators		// basic iTask combinators
		,	iTasksCombinators			// handy set of additional iTask combinators
		,	iTasksTimeAndDateHandling	// iTasks triggered by time and date 
		,	iTasksExceptionHandling		// for handling exceptional situations
		,	iTasksLiftingCombinators	// lifting other domains (e.g. iData) to the iTask domain

		,	iTasksDB					// iTask simple DB access
		,	iTasksSettings				// font settings

