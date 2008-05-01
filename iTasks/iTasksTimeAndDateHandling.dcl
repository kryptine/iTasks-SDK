definition module iTasksTimeAndDateHandling

// *********************************************************************************************************************************
// iTasks for Time and Date Handling
// *********************************************************************************************************************************
// iTask & iData Concept and Implementation: (c) 2006,2007,2008 - Rinus Plasmeijer
// *********************************************************************************************************************************
//
import iDataButtons
import iTasksHandler

/*
waitForTimeTask	:: Task is done when time has come
waitForDateTask	:: Task is done when date has come
*/

waitForTimeTask	:: !HtmlTime								-> Task HtmlTime
waitForDateTask	:: !HtmlDate								-> Task HtmlDate