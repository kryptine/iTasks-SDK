definition module TaskTreeFilters

import iTasksTypes

:: TaskStatus = TaskFinished | TaskActivated | TaskDeleted

instance == TaskStatus

determineTaskList 		:: !UserId 			 	!HtmlTree -> [TaskDescription] 	
determineTaskForTab 	:: !UserId !TaskNrId 	!HtmlTree -> (!TaskStatus,![HtmlTag],![InputId])

mkFilteredTaskTree 		:: !UserId !UserId 		!HtmlTree -> (![HtmlTag],![InputId])
mkUnfilteredTaskTree 	:: 				   		!HtmlTree -> (![HtmlTag],![InputId])

filterTaskTree 			:: 						!HtmlTree -> HtmlTag
filterTaskTreeOfTask 	:: !UserId !TaskNrId 	!HtmlTree -> HtmlTag	


