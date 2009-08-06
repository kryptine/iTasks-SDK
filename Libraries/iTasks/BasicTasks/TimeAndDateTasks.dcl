definition module TimeAndDateTasks
/**
* iTasks for Time and Date Handling
*/
from TSt 			import :: Task
from CommonDomain	import :: Date, :: Time

/*
waitForTimeTask	:: Task is done when time has come
waitForDateTask	:: Task is done when date has come
*/

waitForTimeTask	:: !Time								-> Task Time
waitForDateTask	:: !Date								-> Task Date

waitForTimerTask :: !Time								-> Task Time