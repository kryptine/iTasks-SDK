definition module DateTimeTasks
/**
* iTasks for Date and Time Handling
*/
from TSt 			import :: Task
from CommonDomain	import :: Date, :: Time

/**
* Task completes at specified time.
*/
waitForTimeTask		:: !Time			-> Task Time
/**
* Task completes at specified date.
*/
waitForDateTask		:: !Date			-> Task Date
/**
* Task completes after specified amount of time has passed
* since the creation of the task.
*/
waitForTimerTask	:: !Time			-> Task Time