definition module TaskTreeFilters

import iTasksTypes

:: TaskStatus = TaskFinished | TaskActivated | TaskDeleted

instance == TaskStatus

determineTaskList 		:: !UserId 			 	!HtmlTree -> [([Bool],Bool,TaskDescription)] 	
determineTaskForTab 	:: !UserId !TaskNrId 	!HtmlTree -> (!TaskStatus,![HtmlTag],![InputId])

// Showing Trace from Task Tree 

getFullTraceFromTaskTree:: 						!HtmlTree -> HtmlTag
getTraceFromTaskTree 	:: !UserId !TaskNrId 	!HtmlTree -> HtmlTag	


