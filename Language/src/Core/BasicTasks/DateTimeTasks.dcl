definition module DateTimeTasks
/**
* iTasks for Date and Time Handling
*/
from TSt 			import :: Task
from Void			import :: Void
from CommonDomain	import :: Date, :: Time, :: DateTime

/**
* Returns the current time
*/
getCurrentTime	:: Task Time
/**
* Returns the current date
*/
getCurrentDate	:: Task Date
/**
* Returns the current datetime combination
*/
getCurrentDateTime :: Task DateTime
/**
* Task completes at specified time.
*/
waitForTime		:: !Time			-> Task Void
/**
* Task completes at specified date.
*/
waitForDate		:: !Date			-> Task Void
/**
* Task completes after specified amount of time has passed
* since the creation of the task.
*/
waitForTimer	:: !Time			-> Task Void