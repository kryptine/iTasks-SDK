definition module TaskTreeFilters

import iTasksTypes

:: TaskStatus = TaskFinished | TaskActivated | TaskDeleted

instance == TaskStatus

determineTaskList 		:: !UserId 			 	!HtmlTree -> [TaskDescription] 	
determineTaskForTab 	:: !UserId !TaskNrId 	!HtmlTree -> (!TaskStatus,![HtmlTag],![InputId])

mkFilteredTaskTree 		:: !UserId !UserId 		!HtmlTree -> (![HtmlTag],![InputId])
mkUnfilteredTaskTree 	:: 				   		!HtmlTree -> (![HtmlTag],![InputId])

// Showing Trace from Task Tree 

getFullTraceFromTaskTree:: 						!HtmlTree -> HtmlTag
getTraceFromTaskTree 	:: !UserId !TaskNrId 	!HtmlTree -> HtmlTag	


