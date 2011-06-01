definition module InteractionTasks

from StdFunc	import id, const
import CoreTasks

:: InteractionOption r w	= E.v: About	!v						& iTask v
							| E.v: View		!(!r -> v,!v r -> w)	& iTask v
							| E.v: Get		!(r -> v)				& iTask v
							| E.v: Putback	!(v r -> w)				& iTask v
:: LocalInteractionOption a :== InteractionOption a a

/*** General input/update/output tasks ***/

/*
* Ask the user to enter information.
*
* @param description 									A description of the task to display to the user
*
* @return 												Value yielded by the terminating action
*/
enterInformation :: !d ![LocalInteractionOption a] -> Task a | descr d & iTask a

/*
* Ask the user to update predefined information. 
*
* @param description									A description of the task to display to the user
* @param m												The value to update
*
* @return 												Value yielded by the terminating action
*/
updateInformation :: !d ![LocalInteractionOption m] m -> Task m | descr d & iTask m

/*
* Show a basic message to the user. The user can end the task after reading the message. 
*
* @param description 									A description of the task to display to the user
*
* @return												Value yielded by the terminating action
*/
showMessage :: !d ![LocalInteractionOption m] !m -> Task m | descr d & iTask m

/*
* Ask the user to enter information which is written to a shared.
*
* @param description 									A description of the task to display to the user
* @param (Shared r w)									Reference to shared data the input is written to
*
* @return 												Value yielded by the terminating action
* @throws												SharedException
*/
enterSharedInformation :: !d ![InteractionOption r w] !(Shared r w) -> Task r | descr d & iTask r & iTask w

/*
* Ask the user to update predefined shared information.
*
* @param description 									A description of the task to display to the user
* @param (Shared r w)									Reference to the shared state to update
*
* @return 												Value yielded by the terminating action
* @throws												SharedException
*/
updateSharedInformation :: !d ![InteractionOption r w] !(Shared r w) -> Task r | descr d & iTask r & iTask w

/**
* Monitors a shared state using a functional view.
* A predicate determines when to continue.
*
* @param description									A description of the task to display to the user
* @param (Shared r w)									A reference to the shared state
*
* @return												The last value of the monitored state with (optionally) chosen action
*/
monitor :: !d ![InteractionOption r w] !(Shared r w) -> Task r | descr d & iTask r & iTask w


/*** Special tasks for choices ***/

/*
* Ask the user to select one item from a list of options.
*
* @param description 									A description of the task to display to the user
* @param [o]											A list of options
*
* @return 												Value yielded by the terminating action
*/
enterChoice :: !d ![LocalInteractionOption o] ![o] -> Task o | descr d & iTask o

/*
* Ask the user to select one item from a list of options with already one option pre-selected.
*
* @param description 									A description of the task to display to the user
* @param [o]											A list of options
* @param o												The value of the item which should be pre-selected, if it's not member of the option list no item is selected
*
* @return 												Value yielded by the terminating action
*/
updateChoice :: !d ![LocalInteractionOption o] ![o] o -> Task o | descr d & iTask o

/*
* Ask the user to select one item from a list of shared options.
*
* @param description 									A description of the task to display to the user
* @param (Shared [o] w)									A reference to the shared options
*
* @return 												Value yielded by the terminating action
* @throws												SharedException
*/
enterSharedChoice :: !d ![InteractionOption o w] !(Shared [o] w) -> Task o | descr d & iTask o & iTask w

/*
* Ask the user to select one item from a list of shared options with already one option pre-selected.
*
* @param description 									A description of the task to display to the user
* @param (Shared [o] w)									A reference to the shared options
* @param o												The value of the item which should be pre-selected, if it's not member of the option list no item is selected
*
* @return 												Value yielded by the terminating action
* @throws												SharedException
*/
updateSharedChoice :: !d ![InteractionOption o w] !(Shared [o] w) o -> Task o | descr d & iTask o & iTask w

/*
* Ask the user to select a number of items from a list of options
*
*
* @param description 									A description of the task to display to the user
* @param [o]											A list of options
*
* @return 												Value yielded by the terminating action
*/
enterMultipleChoice :: !d ![LocalInteractionOption o] ![o] -> Task [o] | descr d & iTask o

/*
* Ask the user to select a number of items from a list of options with already a number of options pre-selected.
*
*
* @param description 									A description of the task to display to the user
* @param [o]											A list of options
* @param [o]											The values of the items which should be pre-selected if they are present in the option list
*
* @return 												Value yielded by the terminating action
*/
updateMultipleChoice :: !d ![LocalInteractionOption o] ![o] [o] -> Task [o] | descr d & iTask o

/*
* Ask the user to select a number of items from a list of shared options.
*
* @param description 									A description of the task to display to the user
* @param (Shared [a] w)									A reference to the shared options
*
* @return 												Value yielded by the terminating action
* @throws												SharedException
*/
enterSharedMultipleChoice :: !d ![InteractionOption o w] !(Shared [o] w) -> Task [o] | descr d & iTask o & iTask w

/*
* Ask the user to select one item from a list of shared options with already a number of options pre-selected.
*
* @param description 									A description of the task to display to the user
* @param (Shared [o] w)									A reference to the shared options
* @param [o]											The values of the items which should be pre-selected if they are present in the option list
*
* @return 												Value yielded by the terminating action
*/
updateSharedMultipleChoice :: !d ![InteractionOption o w] !(Shared [o] w) [o] -> Task [o] | descr d & iTask o & iTask w


/*** Special wait tasks ***/

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


/*** Special tasks for choosing actions ***/

/*
* Ask the user to choose an action. 
*
* @param [(Action,a)]							A list of actions the user can choose from. Each actions yields the given result if it's chosen. 
*
* @return 										Value associated with chosen action.
*/
chooseAction :: ![(!Action,a)] -> Task a | iTask a

/*
* Ask the user to choose an action. The list of actions is calculated dynamically.
*
* @param (r -> [(Action,Maybe a)]) 				A function generating a list of actions the user can choose from. Each actions yields the given result if it's chosen & result is present (Just). Otherwise (Nothing) action is disabled.
* @param (Shared r w)							The shared value to use. 
*
* @return 										Value associated with chosen action.
*/						
chooseActionDyn :: !(Shared r w) !(r -> [(!Action,Maybe a)]) -> Task a | iTask a & iTask r & iTask w

/**
* A derived version of 'interact' which only uses a local state.
*
* @param A description of the task to display to the user
* @param A function (on the current local state) dynamically generating the interaction parts shown to the user (parts can change the local state (l))
* @param The initial local state
* @param A function (on the current local state) dynamically calculating the terminators of the task
*
* @return A result determined by the terminators
*/
interactLocal :: !d !(l -> [InteractionPart l]) l -> Task l | descr d & iTask l