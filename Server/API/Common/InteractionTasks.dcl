definition module InteractionTasks

from StdFunc import id, const
import CoreTasks

:: InformationState s =	{ modelValue	:: !s		// the value of the data model the editor is working on
						, localValid	:: !Bool	// a flag indicating if the editor's local view is valid
						}
					
class editorState a b :: (a b) -> Maybe b

instance editorState Maybe a
instance editorState InformationState a

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
interactLocal :: !d !(l -> [InteractionPart l]) !l !(l -> InteractionTerminators a) -> Task a | descr d & iTask l & iTask a

// A function dynamically generating actions from input i for terminating an interaction task.
// The output consists of an action, which is either enabled and yields a result (Just o) or disabled (Nothing).
:: ActionFunc i o :== i -> [(!Action,!Maybe o)]

/*
* Ask the user to enter information.
*
* @param description 									A description of the task to display to the user
* @param about (optional)								Additional information to display
* @param (ActionFunc (Maybe v) a) (optional)			A function (on the current local value (if present)) dynamically calculating actions for the task
*														If not specified an ok action is provided which is enabled if the editor is valid and yields the current local value
*
* @return 												Value yielded by the terminating action
*/
enterInformation			:: !d 						    			-> Task a | descr d & iTask a
enterInformationAbout		:: !d !about								-> Task a | descr d & iTask a & iTask about
enterInformationA			:: !d 			!(ActionFunc (Maybe v) a)	-> Task a | descr d & iTask a & iTask v
enterInformationAboutA		:: !d !about	!(ActionFunc (Maybe v) a)	-> Task a | descr d & iTask a & iTask v & iTask about

/*
* Ask the user to enter information which is written to a shared.
*
* @param description 									A description of the task to display to the user
* @param (v r -> w)										A view for type v is generated; This function defines how to map view v back to a value of type w
* @param about (optional)								Additional information to display
* @param (Shared r w)									Reference to shared data the input is written to
* @param (ActionFunc (InformationState r) a) (optional)	A function (on the current information task state) dynamically calculating actions for the task
*														If not specified an ok action is provided which is enabled if the editor is valid and yields the current shared value
*
* @return 												Value yielded by the terminating action
* @throws												SharedException
*/
enterSharedInformation			:: !d !(v r -> w)			!(Shared r w)										-> Task r | descr d & iTask r & iTask v & iTask w
enterSharedInformationAbout		:: !d !(v r -> w) !about	!(Shared r w)										-> Task r | descr d & iTask r & iTask v & iTask w & iTask about
enterSharedInformationA			:: !d !(v r -> w) 			!(Shared r w) !(ActionFunc (InformationState r) a)	-> Task a | descr d & iTask a & iTask r & iTask v & iTask w
enterSharedInformationAboutA	:: !d !(v r -> w) !about	!(Shared r w) !(ActionFunc (InformationState r) a)	-> Task a | descr d & iTask a & iTask r & iTask v & iTask w & iTask about

/*
* Ask the user to select one item from a list of options.
*
* @param description 									A description of the task to display to the user
* @param (o -> v) (optional)							A view for options of type o is generated; This function defines how to map an option to a view value of type v
*														If not specified, o = v, and the view is the identity
* @param about (optional)								Additional information to display
* @param (ActionFunc (Maybe o) a) (optional)			A function (on the currently selected option (if present)) dynamically calculating actions for the task
														If not specified an ok action is provided which is enabled if an option is chosen and yields that option

* @param [o]											A list of options
*
* @return 												Value yielded by the terminating action
* @throws												ChoiceException
*/
enterChoice					:: !d 					![o]							-> Task o | descr d & iTask o
enterChoiceAbout			:: !d 			!about	![o]							-> Task o | descr d & iTask o & iTask about
enterChoiceA				:: !d !(o -> v)  		![o] !(ActionFunc (Maybe o) a)	-> Task a | descr d & iTask a & iTask v
enterChoiceAboutA			:: !d !(o -> v)	!about	![o] !(ActionFunc (Maybe o) a)	-> Task a | descr d & iTask a & iTask v & iTask about

/*
* Ask the user to select one item from a list of shared options.
*
* @param description 									A description of the task to display to the user
* @param (o -> v) (optional)							A view for options of type o is generated; This function defines how to map an option to a view value of type v
														If not specified, o = v, and the view is the identity
* @param about (optional)								Additional information to display
* @param (Shared [a] w)									A reference to the shared options
* @param (ActionFunc (Maybe o) a) (optional)			A function (on the currently selected option (if present)) dynamically calculating actions for the task
*														If not specified an ok action is provided which is enabled if an option is chosen and yields that option
*
* @return 												Value yielded by the terminating action
* @throws												SharedException
*/
enterSharedChoice			:: !d					!(Shared [o] w)								-> Task o | descr d & iTask o & iTask w
enterSharedChoiceAbout		:: !d			!about	!(Shared [o] w)								-> Task o | descr d & iTask o & iTask w & iTask about
enterSharedChoiceA			:: !d !(o -> v) 		!(Shared [o] w) !(ActionFunc (Maybe o) a)	-> Task a | descr d & iTask a & iTask w & iTask v
enterSharedChoiceAboutA		:: !d !(o -> v)	!about	!(Shared [o] w) !(ActionFunc (Maybe o) a)	-> Task a | descr d & iTask a & iTask w & iTask v & iTask about

/*
* Ask the user to select a number of items from a list of options
*
*
* @param description 									A description of the task to display to the user
* @param (o -> v) (optional)							A view for options of type o is generated; This function defines how to map an option to a view value of type v
*														If not specified, o = v, and the view is the identity
* @param about (optional)								Additional information to display
* @param [o]											A list of options
* @param (ActionFunc [o] a) (optional)					A function (on the currently selected options) dynamically calculating actions for the task
*														If not specified an ok action is provided which always enabled and yields the currently selected options
*
* @return 												Value yielded by the terminating action
*/
enterMultipleChoice			:: !d 					![o]						-> Task [o]	| descr d & iTask o
enterMultipleChoiceAbout	:: !d 			!about	![o]						-> Task [o]	| descr d & iTask o	& iTask about
enterMultipleChoiceA		:: !d !(o -> v) 		![o] !(ActionFunc [o] a)	-> Task a	| descr d & iTask a & iTask v
enterMultipleChoiceAboutA	:: !d !(o -> v)	!about	![o] !(ActionFunc [o] a)	-> Task a	| descr d & iTask a & iTask v & iTask about

/*
* Ask the user to select a number of items from a list of shared options.
*
* @param description 									A description of the task to display to the user
* @param (o -> v) (optional)							A view for options of type o is generated; This function defines how to map an option to a view value of type v
*														If not specified, o = v, and the view is the identity
* @param about (optional)								Additional information to display
* @param (Shared [a] w)									A reference to the shared options
* @param (ActionFunc [o] a) (optional)					A function (on the currently selected options) dynamically calculating actions for the task
*														If not specified an ok action is provided which always enabled and yields the currently selected options
*
* @return 												Value yielded by the terminating action
* @throws												SharedException
*/
enterSharedMultipleChoice		:: !d					!(Shared [o] w)						-> Task [o]	| descr d & iTask o & iTask w
enterSharedMultipleChoiceAbout	:: !d			!about	!(Shared [o] w)						-> Task [o]	| descr d & iTask o & iTask w & iTask about
enterSharedMultipleChoiceA		:: !d !(o -> v) 		!(Shared [o] w)	!(ActionFunc [o] a)	-> Task a	| descr d & iTask a & iTask v & iTask w
enterSharedMultipleChoiceAboutA	:: !d !(o -> v)	!about	!(Shared [o] w)	!(ActionFunc [o] a)	-> Task a	| descr d & iTask a & iTask v & iTask w & iTask about

// A view mapping an input from a shared data source (r)
// to a view shown to the user (v)
// and finally data changed by the user back to the data source (w).
:: View				r v w	:== (!r -> v,!v r -> w)
// A view on a SymmetricShared, having the same read and write type.
:: SymmetricView	m v		:== View m v m
// The identity view
idView :== (id,const)

/*
* Ask the user to update predefined information. 
*
* @param description									A description of the task to display to the user
* @param (SymmetricView m v) (optional)					View defining how to convert m to the demanded view v and backwards to m
*														If not specified, m = v, and the view is the identity view
* @param about (optional)								Additional information to display
* @param (ActionFunc (InformationState m) a) (optional)	A function (on the current information task state) dynamically calculating actions for the task
*														If not specified an ok action is provided which is enabled if the editor is valid and yields the current value of the updated information
* @param m												The value to update
*
* @return 												Value yielded by the terminating action
*/
updateInformation			:: !d								!m										-> Task m | descr d & iTask m
updateInformationAbout		:: !d						!about	!m										-> Task m | descr d & iTask m & iTask about
updateInformationA			:: !d !(SymmetricView m v) 			!m !(ActionFunc (InformationState m) a)	-> Task a | descr d & iTask a & iTask m & iTask v
updateInformationAboutA		:: !d !(SymmetricView m v) 	!about	!m !(ActionFunc (InformationState m) a)	-> Task a | descr d & iTask a & iTask m & iTask v & iTask about

/*
* Ask the user to update predefined shared information.
*
* @param description 									A description of the task to display to the user
* @param (View r v w)									View defining how to convert r to the demanded view v and backwards to w
* @param about (optional)								Additional information to display
* @param (Shared r w)									Reference to the shared state to update
* @param (ActionFunc (InformationState r) a) (optional)	A function (on the current information task state) dynamically calculating actions for the task
*														If not specified an ok action is provided which is enabled if the editor is valid and yields the current shared value
*
* @return 												Value yielded by the terminating action
* @throws												SharedException
*/
updateSharedInformation			:: !d !(View r v w)			!(Shared r w)										-> Task r | descr d & iTask r & iTask v & iTask w
updateSharedInformationAbout	:: !d !(View r v w) !about	!(Shared r w)										-> Task r | descr d & iTask r & iTask v & iTask w & iTask about
updateSharedInformationA		:: !d !(View r v w) 		!(Shared r w) !(ActionFunc (InformationState r) a)	-> Task a | descr d & iTask a & iTask r & iTask v & iTask w
updateSharedInformationAboutA	:: !d !(View r v w) !about	!(Shared r w) !(ActionFunc (InformationState r) a)	-> Task a | descr d & iTask a & iTask r & iTask v & iTask w & iTask about

/*
* Ask the user to select one item from a list of options with already one option pre-selected.
*
* @param description 									A description of the task to display to the user
* @param (o -> v) (optional)							A view for options of type o is generated; This function defines how to map an option to a view value of type v
*														If not specified, o = v, and the view is the identity
* @param about (optional)								Additional information to display
* @param [o]											A list of options
* @param Int											The index of the item which should be pre-selected
* @param (ActionFunc (Maybe o) a) (optional)			A function (on the currently selected option (if present)) dynamically calculating actions for the task
*														If not specified an ok action is provided which is enabled if an option is chosen and yields that option
*
* @return 												Value yielded by the terminating action
* @throws												ChoiceException
*/
updateChoice				:: !d					![o] !Int							-> Task o | descr d & iTask o
updateChoiceAbout			:: !d 			!about	![o] !Int							-> Task o | descr d & iTask o & iTask about
updateChoiceA 				:: !d !(o -> v) 		![o] !Int !(ActionFunc (Maybe o) a)	-> Task a | descr d & iTask a & iTask v
updateChoiceAboutA			:: !d !(o -> v)	!about	![o] !Int !(ActionFunc (Maybe o) a)	-> Task a | descr d & iTask a & iTask v & iTask about

/*
* Ask the user to select one item from a list of shared options with already one option pre-selected.
*
* @param description 									A description of the task to display to the user
* @param (o -> v) (optional)							A view for options of type o is generated; This function defines how to map an option to a view value of type v
*														If not specified, o = v, and the view is the identity
* @param about (optional)								Additional information to display
* @param (Shared [o] w)									A reference to the shared options
* @param Int											The index of the item which should be pre-selected
* @param (ActionFunc (Maybe o) a) (optional)			A function (on the currently selected option (if present)) dynamically calculating actions for the task
*														If not specified an ok action is provided which is enabled if an option is chosen and yields that option
*
* @return 												Value yielded by the terminating action
* @throws												SharedException
*/
updateSharedChoice			:: !d					!(Shared [o] w) !Int							-> Task o | descr d & iTask o & iTask w
updateSharedChoiceAbout		:: !d			!about	!(Shared [o] w) !Int							-> Task o | descr d & iTask o & iTask w & iTask about
updateSharedChoiceA 		:: !d !(o -> v) 		!(Shared [o] w)	!Int !(ActionFunc (Maybe o) a)	-> Task a | descr d & iTask a & iTask v & iTask w
updateSharedChoiceAboutA	:: !d !(o -> v)	!about	!(Shared [o] w)	!Int !(ActionFunc (Maybe o) a)	-> Task a | descr d & iTask a & iTask v & iTask w & iTask about

/*
* Ask the user to select a number of items from a list of options with already a number of options pre-selected.
*
*
* @param description 									A description of the task to display to the user
* @param (o -> v) (optional)							A view for options of type o is generated; This function defines how to map an option to a view value of type v
*														If not specified, o = v, and the view is the identity
* @param about (optional)								Additional information to display
* @param [o]											A list of options
* @param [Int]											The indexes of the items which should be pre-selected
* @param (ActionFunc [o] a) (optional)					A function (on the currently selected options) dynamically calculating actions for the task
*														If not specified an ok action is provided which always enabled and yields the currently selected options
*
* @return 												Value yielded by the terminating action
*/
updateMultipleChoice		:: !d 					![o] ![Int] 					-> Task [o]	| descr d & iTask o
updateMultipleChoiceAbout	:: !d 			!about	![o] ![Int] 					-> Task [o]	| descr d & iTask o & iTask about
updateMultipleChoiceA		:: !d !(o -> v) 		![o] ![Int] !(ActionFunc [o] a)	-> Task a	| descr d & iTask a & iTask v
updateMultipleChoiceAboutA	:: !d !(o -> v) !about	![o] ![Int] !(ActionFunc [o] a)	-> Task a	| descr d & iTask a & iTask v & iTask about

/*
* Ask the user to select one item from a list of shared options with already a number of options pre-selected.
*
* @param description 									A description of the task to display to the user
* @param (o -> v) (optional)							A view for options of type o is generated; This function defines how to map an option to a view value of type v
														If not specified, o = v, and the view is the identity
* @param about (optional)								Additional information to display
* @param (Shared [o] w)									A reference to the shared options
* @param [Int]											The indexes of the items which should be pre-selected
* @param (ActionFunc [o] a) (optional)					A function (on the currently selected options) dynamically calculating actions for the task
*														If not specified an ok action is provided which always enabled and yields the currently selected options
*
* @return 												Value yielded by the terminating action
*/
updateSharedMultipleChoice			:: !d					!(Shared [o] w) ![Int]						-> Task [o]	| descr d & iTask o & iTask w
updateSharedMultipleChoiceAbout		:: !d			!about	!(Shared [o] w) ![Int]						-> Task [o]	| descr d & iTask o & iTask w & iTask about
updateSharedMultipleChoiceA			:: !d !(o -> v) 		!(Shared [o] w) ![Int] !(ActionFunc [o] a)	-> Task a	| descr d & iTask a & iTask v & iTask w
updateSharedMultipleChoiceAboutA	:: !d !(o -> v) !about	!(Shared [o] w) ![Int] !(ActionFunc [o] a)	-> Task a	| descr d & iTask a & iTask v & iTask w & iTask about

/*
* Show a basic message to the user. The user can end the task after reading the message. 
*
* @param description 									A description of the task to display to the user
* @param (about -> v) (optional)						A view on the about value, defining how it is shown to the user
														If not specified, v = about, and the view is the identity
* @param about (optional)								Additional information to display
* @param [Action] OR a (optional)						A list of actions, through which the user can submit the value
														All actions are always possible since there is no changing state
														If not specified an ok action yielding the about OR the given value is provided
*
* @return												Value yielded by the terminating action
*/
showMessage			:: !d						!a				-> Task a		| descr d & iTask a
showMessageAbout	:: !d				!about					-> Task about	| descr d & iTask about
showMessageA		:: !d						![(!Action,!a)]	-> Task a		| descr d & iTask a
showMessageAboutA	:: !d !(about -> v)	!about	![(!Action,!a)]	-> Task a		| descr d & iTask a & iTask v

/* 
* Shows a instruction to the user. The user can dismiss the instruction.
*
* @param String											A short descriptive subject
* @param instruction									The instruction
* @param a or about										The value is just passed for convenience or shown as context information		
*
* @return												A copy of value a or about
*/
showInstruction 			:: !String !instruction	!a		-> Task a		| html instruction & iTask a
showInstructionAbout 		:: !String !instruction !about 	-> Task about	| html instruction & iTask about

/**
* Monitors a shared state using a functional view.
* A predicate determines when to continue.
*
* @param description									A description of the task to display to the user
* @param (r -> v)										A view function
* @param (r -> Bool) (optional)							A predicate determining when to continue
* @param Bool (optional)								A flag indicating if to finish the task automatically if condition is true or let the user trigger a continue-action
* @param about (optional)								Additional information to display
* @param (Shared r w)									A reference to the shared state
* @param (r -> InteractionTerminators a) (optional)		A function computing the terminators for the task
*														If not specified the task automatically continues or provides a continue-action, depending on the predicate and continue-flag argument; in this case the current shared value is yielded
*
* @return												The last value of the monitored state with (optionally) chosen action
*/
monitor			:: !d !(r -> v) !(r -> Bool) !Bool			!(Shared r w)									-> Task r | descr d & iTask r & iTask v & iTask w
monitorAbout	:: !d !(r -> v) !(r -> Bool) !Bool 	!about	!(Shared r w)									-> Task r | descr d & iTask r & iTask v & iTask w & iTask about
monitorA		:: !d !(r -> v) 							!(Shared r w) !(r -> InteractionTerminators a)	-> Task a | descr d & iTask a & iTask v & iTask w
monitorAboutA	:: !d !(r -> v) 					!about	!(Shared r w) !(r -> InteractionTerminators a)	-> Task a | descr d & iTask a & iTask v & iTask w & iTask about

/**
* Waits until a shared Maybe-state contains a value.
*
* @param description									A description of the task to display to the user
* @param about (optional)								Additional information to display
* @param (Shared (Maybe r) w)							A reference to the shared Maybe-state
*
* @return												The last value of the monitored state
*/
wait		:: !d			!(Shared (Maybe r) w) -> Task r | descr d & iTask r & iTask w
waitAbout	:: !d !about	!(Shared (Maybe r) w) -> Task r | descr d & iTask r & iTask w & iTask about

/**
* Waits until a predicate on a shared state holds.
*
* @param description									A description of the task to display to the user
* @param (r -> Bool)									A predicate on the monitored state
* @param about (optional)								Additional information to display
* @param (Shared r w)									A reference to the shared state
*
* @return												The last value of the monitored state
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

/*
* Ask the user to choose an action. 
*
* @param [(Action,a)]							A list of actions the user can choose from. Each actions yields the given result if it's chosen. 
*
* @return 										Value associated with chosen action.
*/
chooseAction		:: ![(!Action,a)]								-> Task a | iTask a

/*
* Ask the user to choose an action. The list of actions is calculated dynamically.
*
* @param (r -> [(Action,Maybe a)]) 				A function generating a list of actions the user can choose from. Each actions yields the given result if it's chosen & result is present (Just). Otherwise (Nothing) action is disabled.
* @param (Shared r w)							The shared value to use. 
*
* @return 										Value associated with chosen action.
*/						
chooseActionDyn 	:: !(Shared r w) !(ActionFunc r a)	-> Task a | iTask a & iTask w

noActions		:: ActionFunc a Void
noActionsMsg	:: [(!Action,!Maybe Void)]