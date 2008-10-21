definition module TaskTreeFilters

// *********************************************************************************************************************************
// This module contains filters for filtering information from a TaskTree
// *********************************************************************************************************************************
// iTask & iData Concept and Implementation: (c) 2006,2007,2008 - Rinus Plasmeijer
// *********************************************************************************************************************************

import iTasksTypes


noFilter 		:: HtmlTree -> HtmlCode
Filter 			:: !Bool !UserId !UserId !HtmlTree !*HSt -> *(![BodyTag],![BodyTag],![BodyTag],![BodyTag],![BodyTag],!*HSt)
collect 		:: !UserId !UserId ![(!ProcessNr,!WorkflowLabel,!TaskLabel,![BodyTag])] !HtmlTree -> (![BodyTag],![(!ProcessNr,!WorkflowLabel,!TaskLabel,![BodyTag])])

