definition module TaskTree

// *********************************************************************************************************************************
// This module contains the functions for calculating the TaskTree
// *********************************************************************************************************************************
// iTask & iData Concept and Implementation: (c) 2006,2007,2008 - Rinus Plasmeijer
// *********************************************************************************************************************************

import iTasksTypes

calculateTaskTree :: !UserId !Bool !Bool !Bool !(Task a) !*HSt  
						-> (!Bool,!HtmlTree,!Maybe String,!Maybe [HtmlTag],!Maybe [HtmlTag],!*HSt) | iData a
