definition module TaskTreeFilters

import iTasksTypes

determineTaskList 		:: !UserId 			 	!HtmlTree -> [TaskDescription] 	
determineTaskForTab 	:: !UserId !TaskNrId 	!HtmlTree -> (!Bool,![HtmlTag],![InputId])

mkFilteredTaskTree 		:: !UserId !UserId 		!HtmlTree -> (![HtmlTag],![InputId])
mkUnfilteredTaskTree 	:: 				   		!HtmlTree -> (![HtmlTag],![InputId])

filterTaskTree 			:: 						!HtmlTree -> HtmlTag
filterTaskTreeOfTask 	:: !UserId !TaskNrId 	!HtmlTree -> HtmlTag	


