definition module OutputTasks
/*
* This module provides means to output information to the user
*/
from Task				import class descr, :: Task
from HTML				import :: HtmlTag
from Shared				import :: Shared
from StdFunc			import id, const
from InteractionTasks	import :: Verified, :: PredAction
from Types				import :: Date, :: Time, :: DateTime
import iTaskClass

/*
* Show a basic message to the user. The user can end the task after reading the message. 
*
* @param description 		A description of the task to display to the user
* @param [Action]			A list of buttons or menus, through which the user can submit the value
							All actions are always possible since there is no changing state
* @param a OR about			The value is just passed for convenience or shown as about value
*
* @return					A copy of value a with (optionally) chosen action
*/
showMessage			:: !d							!a		-> Task a					| descr d & iTask a
showMessageA		:: !d ![Action]					!a		-> Task (!Action,!a)		| descr d & iTask a
showMessageAbout	:: !d							!about	-> Task about				| descr d & iTask about
showMessageAboutA	:: !d !(about -> v)	![Action]	!about	-> Task (!Action,!about)	| descr d & iTask about & iTask v

/* 
* Shows a instruction to the user. The user can dismiss the instruction.
*
* @param String				A short descriptive subject
* @param instruction		The instruction
* @param a					The value that is returned when the task is finished		
*
* @return					a
*/
showInstruction 			:: !String !instruction	!a		-> Task a		| html instruction & iTask a
showInstructionAbout 		:: !String !instruction !about 	-> Task about	| html instruction & iTask about

/**
* Monitors a shared state using a functional view.
* A predicate determines when to continue.
*
* @param A description of the task to display to the user
* @param A view function
* @param A predicate determining when to continue
* @param A flag indicating if to finish the task automatically if condition is true or let the user press a continue-button OR a list of actions the user can take before the predicate holds
* @param Possibly additional information to display
* @param A reference to the shared state
* @return The last value of the monitored state with (optionally) chosen action
*/
monitor			:: !d !(r -> v) !(r -> Bool) !Bool					!(Shared r w) -> Task r						| descr d & iTask r & iTask v & iTask w
monitorA		:: !d !(r -> v) !(r -> Bool) ![PredAction r]		!(Shared r w) -> Task (!Maybe Action,!r)	| descr d & iTask r & iTask v & iTask w
monitorAbout	:: !d !(r -> v) !(r -> Bool) !Bool !about			!(Shared r w) -> Task r						| descr d & iTask r & iTask v & iTask w & iTask about
monitorAboutA	:: !d !(r -> v) !(r -> Bool) ![PredAction r] !about	!(Shared r w) -> Task (!Maybe Action,!r)	| descr d & iTask r & iTask v & iTask w & iTask about

/**
* Waits until a shared Maybe-state contains a value.
*
* @param A description of the task to display to the user
* @param A reference to the shared state
* @param Possibly additional information to display
* @return The last value of the monitored state
*/
wait		:: !d			!(Shared (Maybe r) w) -> Task r | descr d & iTask r & iTask w
waitAbout	:: !d !about	!(Shared (Maybe r) w) -> Task r | descr d & iTask r & iTask w & iTask about

/**
* Waits until a predicate on a shared state holds.
*
* @param A description of the task to display to the user
* @param A predicate on the monitored state
* @param Possibly additional information to display
* @param A reference to the shared state
* @return The last value of the monitored state
*/
waitUntil		:: !d !(r -> Bool)			!(Shared r w) -> Task r | descr d & iTask r & iTask w
waitUntilAbout	:: !d !(r -> Bool) !about	!(Shared r w) -> Task r | descr d & iTask r & iTask w & iTask about

/**
* Creates a task which blocks a workflow until a specified time.
*
* @param The specified time at which the task should complete
*
* @return The time to wait for
*/
waitForTime		:: !Time			-> Task Time
/**
* Creates a task which blocks a workflow until a specified date.
*
* @param The specified date at which the task should complete
*
* @return The date to wait for
*/
waitForDate		:: !Date			-> Task Date
/**
* Task completes after specified amount of time has passed
* since the creation of the task.
*
* @param The time to wait before the task should complete
*
* @return The time the timer went off
*/
waitForTimer	:: !Time			-> Task Time
