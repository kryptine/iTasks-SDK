definition module TaskTreeFilters

// *********************************************************************************************************************************
// This module contains filters for filtering information from a TaskTree
// *********************************************************************************************************************************
// iTask & iData Concept and Implementation: (c) 2006,2007,2008 - Rinus Plasmeijer
// *********************************************************************************************************************************

import iTasksTypes

/*
noFilter 		:: Without applying any filter it fetches all html code out of the HtmlTree
Filter 			:: Filters out the html code for a specific user
					Arguments:	Bool True if a whole new page has to be generated,
								First UserId is id of user logged in,
								Second UserId is id of user that owns the thread (if it is a thread),
								The HtmlTree to inspects,
								The HSt for generating navigation buttons.
				   Returns: 	Whole new page html code for the task (if a whole page is demanded), 
				   				header indicating chosen task, buttons to choose a main task, buttons to choose a subtask)
				   				Html code of a chosen task if not a whole page is demanded
*/


noFilter 		:: !HtmlTree -> HtmlCode
Filter 			:: !Bool !UserId !UserId !HtmlTree !*HSt -> *(![BodyTag],![BodyTag],![BodyTag],![BodyTag],![BodyTag],!*HSt)


collectTaskList :: !UserId !UserId !HtmlTree -> [(UserId,TaskNr,TaskName)] 	// returns who created the task, the tasknr, and taskname
