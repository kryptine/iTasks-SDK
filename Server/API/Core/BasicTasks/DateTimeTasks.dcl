definition module DateTimeTasks
/**
* iTasks for Date and Time Handling
*/
from TSt 			import :: Task
from Void			import :: Void
from Types			import :: Date, :: Time, :: DateTime
from Shared			import :: ReadOnlyShared, :: Shared

/**
* Retrieves the current time
*
* @return The current system time
*/
getCurrentTime	:: Task Time
/**
* Retrieves the current date
* 
* @return The current system date
*/
getCurrentDate	:: Task Date
/**
* Retrieves the current datetime combination
*
* @return The current system's date and time
*/
getCurrentDateTime :: Task DateTime
/**
* Creates a task which blocks a workflow until a specified time.
*
* @param The specified time at which the task should complete
*
* @return Void
*/
waitForTime		:: !Time			-> Task Void
/**
* Creates a task which blocks a workflow until a specified date.
*
* @param The specified date at which the task should complete
*
* @return Void
*/
waitForDate		:: !Date			-> Task Void
/**
* Task completes after specified amount of time has passed
* since the creation of the task.
*
* @param The time to wait before the task should complete
*
* @return Void
*/
waitForTimer	:: !Time			-> Task Void

sharedCurrentDateTime	:: ReadOnlyShared DateTime
sharedCurrentTime		:: ReadOnlyShared Time
sharedCurrentDate		:: ReadOnlyShared Date
