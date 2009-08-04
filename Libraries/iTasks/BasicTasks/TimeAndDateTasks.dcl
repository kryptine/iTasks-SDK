definition module TimeAndDateTasks
/**
* iTasks for Time and Date Handling
*/
from TSt 			import :: Task
from GUIWidgets		import :: HtmlTime, :: HtmlDate
/*
waitForTimeTask	:: Task is done when time has come
waitForDateTask	:: Task is done when date has come
*/

waitForTimeTask	:: !HtmlTime								-> Task HtmlTime
waitForDateTask	:: !HtmlDate								-> Task HtmlDate