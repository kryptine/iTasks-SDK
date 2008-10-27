definition module TaskTree

// *********************************************************************************************************************************
// This module contains the functions for calculating the TaskTree
// *********************************************************************************************************************************
// iTask & iData Concept and Implementation: (c) 2006,2007,2008 - Rinus Plasmeijer
// *********************************************************************************************************************************

import iTasksTypes

calculateTaskTree :: !UserId !(Task a) !*HSt  -> (!Bool,!HtmlTree,!Maybe String,!*HSt) | iData a
