definition module TaskTreeFilters

import iTasksTypes

determineTaskList 		:: !UserId 			 !HtmlTree -> [TaskDescription] 	
determineTaskForTab 	:: !UserId !TaskNrId !HtmlTree -> (!Bool,![HtmlTag],![InputId])

mkFilteredTaskTree 		:: !UserId !UserId !HtmlTree -> (![HtmlTag],![InputId])
mkUnfilteredTaskTree 	:: 				   !HtmlTree -> (![HtmlTag],![InputId])



//TODO: merge trace information into the task tree 
showTaskTreeOfTask	:: !TaskNrId !(Maybe [Trace]) -> HtmlTag
showTaskTree 		:: !(Maybe [Trace]) -> HtmlTag
