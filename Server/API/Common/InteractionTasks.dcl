definition module InteractionTasks

from StdFunc import id, const
import CoreTasks

/**
* A derived version of 'interact' which only uses a local state.
*
* @param A description of the task to display to the user
* @param A function (on the current local state) dynamically generating the interaction parts shown to the user (parts can change the local state (l))
* @param A function (on the current local state) dynamically calculating the terminators of the task
* @param The initial local state
*
* @return A result determined by the terminators
*/
interactLocal :: !d !(l -> [InteractionPart l]) !(l -> InteractionTerminators a) !l -> Task a | descr d & iTask l & iTask a

/*
* Ask the user to enter information.
*
* @param description 							A description of the task to display to the user
* @param (v -> a) (optional)					A view for type v is generated; This function defines how to map view v back to a value of type a
*												If not specified, v = a, and the map back is the identity
* @param [PredAction (Verified a)] (optional)	A list of action/predicate pairs (mapped to buttons or menus) through which the user can submit the value.
*												If not specified an ok button is provided which is enabled if the editor is valid
* @param about (optional)						Additional information to display
*
* @return 										Resulting value or chosen action with the resulting value if editor was in valid state
*/
enterInformation			:: !d 						    					-> Task a					| descr d & iTask a
enterInformationA			:: !d !(v -> a) ![PredAction (Verified a)]			-> Task (!Action,!Maybe a)	| descr d & iTask a & iTask v
enterInformationAbout		:: !d 										!about	-> Task a					| descr d & iTask a & iTask about
enterInformationAboutA		:: !d !(v -> a) ![PredAction (Verified a)]	!about	-> Task (!Action,!Maybe a)	| descr d & iTask a & iTask about & iTask v

/*
* Ask the user to enter information which is written to a shared.
*
* @param description 							A description of the task to display to the user
* @param (v -> a)								A view for type v is generated; This function defines how to map view v back to a value of type w
* @param [PredAction (Valid,r)]					A list of action/predicate pairs (mapped to buttons or menus) through which the user can submit the value
* @param about (optional)						Additional information to display
* @param (Shared r w)							Reference to shared data the input is written to
*
* @return 										Chosen action with the current shared value
* @throws										SharedException
*/
enterSharedInformationA			:: !d !(v -> w) ![PredAction (!Valid,!r)]			!(Shared r w) -> Task (!Action,!r) | descr d & iTask r & iTask v & iTask w
enterSharedInformationAboutA	:: !d !(v -> w) ![PredAction (!Valid,!r)] !about	!(Shared r w) -> Task (!Action,!r) | descr d & iTask r & iTask about & iTask v & iTask w

/*
* Asks the user to confirm or decline a question.
*
* @param description 							A description of the task to display to the user
* @param about (optional)						Additional information to display
*
* @return 										A boolean indicating 'accepted' (True) or 'declined' (False)
*/
requestConfirmation			:: !d			-> Task Bool | descr d 
requestConfirmationAbout	:: !d !about	-> Task Bool | descr d & iTask about

/*
* Ask the user to select one item from a list of options.
*
* @param description 							A description of the task to display to the user
* @param (a -> v) (optional)					A view for options of type a is generated; This function defines how to map an option to a view value of type v
*												If not specified, a = v, and the view is the identity
* @param [PredAction (Verified a)] (optional)	A list of action/predicate pairs (mapped to buttons or menus) through which the user can submit the value
												If not specified an ok button is provided which is enabled if an option is chosen
* @param about (optional)						Additional information to display
* @param [a]									A list of options
*
* @return 										Chosen option or chosen action with the chosen option if present
* @throws										ChoiceException
*/
enterChoice					:: !d 					  							![a] -> Task a					| descr d & iTask a
enterChoiceA				:: !d !(a -> v) ![PredAction (Verified a)] 			![a] -> Task (!Action,!Maybe a)	| descr d & iTask a & iTask v
enterChoiceAbout			:: !d 										!about	![a] -> Task a					| descr d & iTask a	& iTask about
enterChoiceAboutA			:: !d !(a -> v) ![PredAction (Verified a)]	!about	![a] -> Task (!Action,!Maybe a)	| descr d & iTask a & iTask about & iTask v

/*
* Ask the user to select one item from a list of shared options.
*
* @param description 							A description of the task to display to the user
* @param (a -> v)								A view for options of type a is generated; This function defines how to map an option to a view value of type v
* @param [PredAction (Verified a)]				A list of action/predicate pairs (mapped to buttons or menus) through which the user can submit the value
* @param about (optional)						Additional information to display
* @param (Shared [a] w)							A reference to the shared options
*
* @return 										Chosen action with the chosen option if present
* @throws										SharedException
*/
enterSharedChoice			:: !d												!(Shared [a] w) -> Task a					| descr d & iTask a & iTask w
enterSharedChoiceA			:: !d !(a -> v) ![PredAction (Verified a)]			!(Shared [a] w) -> Task (!Action, Maybe a)	| descr d & iTask a & iTask v & iTask w
enterSharedChoiceAboutA		:: !d !(a -> v) ![PredAction (Verified a)] !about	!(Shared [a] w) -> Task (!Action, Maybe a)	| descr d & iTask a & iTask about & iTask v & iTask w

/*
* Ask the user to select a number of items from a list of options
*
*
* @param description 							A description of the task to display to the user
* @param (a -> v) (optional)					A view for options of type a is generated; This function defines how to map an option to a view value of type v
*												If not specified, a = v, and the view is the identity
* @param [PredAction [a]] (optional)			A list of action/predicate pairs (mapped to buttons or menus) through which the user can submit the value
												If not specified an ok button is provided
* @param about (optional)						Additional information to display
* @param [a]									A list of options
*
* @return 										Chosen values or chosen action with the chosen options
*/
enterMultipleChoice			:: !d 					  					![a] -> Task [a]			| descr d & iTask a
enterMultipleChoiceA		:: !d !(a -> v) ![PredAction [a]]			![a] -> Task (!Action,![a])	| descr d & iTask a & iTask v
enterMultipleChoiceAbout	:: !d 								!about	![a] -> Task [a]			| descr d & iTask a	& iTask about
enterMultipleChoiceAboutA	:: !d !(a -> v) ![PredAction [a]]	!about	![a] -> Task (!Action,![a])	| descr d & iTask a & iTask about & iTask v


/*
* Ask the user to select a number of items from a list of shared options.
*
* @param description 							A description of the task to display to the user
* @param (a -> v)								A view for options of type a is generated; This function defines how to map an option to a view value of type v
* @param [PredAction [a]]						A list of action/predicate pairs (mapped to buttons or menus) through which the user can submit the value
* @param about (optional)						Additional information to display
* @param (Shared [a] w)							A reference to the shared options
*
* @return 										Chosen action with the chosen options
* @throws										SharedException
*/
enterSharedMultipleChoiceA		:: !d !(a -> v) ![PredAction [a]]			!(Shared [a] w)	-> Task (!Action, [a]) | descr d & iTask a & iTask v & iTask w
enterSharedMultipleChoiceAboutA	:: !d !(a -> v) ![PredAction [a]] !about	!(Shared [a] w)	-> Task (!Action, [a]) | descr d & iTask a & iTask about & iTask v & iTask w

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
* @param description							A description of the task to display to the user
* @param (SymmetricView a v) (optional)			View defining how to convert a to the demanded view v and backwards to a
*												If not specified, a = v, and the view is the identity view
* @param [PredAction (Verified a)]				A list of action/predicate pairs (mapped to buttons or menus) through which the user can submit the value
*												If not specified an ok button is provided which is enabled if the editor is valid
* @param about (optional)						Additional information to display
* @param a										The initial value to use
*
* @return 										Resulting value or chosen action with the resulting value if editor was in valid state.
*/
updateInformation			:: !d															!a -> Task a					| descr d & iTask a
updateInformationA			:: !d !(SymmetricView a v)	![PredAction (Verified a)]			!a -> Task (!Action, !Maybe a)	| descr d & iTask a & iTask v
updateInformationAbout		:: !d !about													!a -> Task a					| descr d & iTask a & iTask about
updateInformationAboutA		:: !d !(SymmetricView a v)	![PredAction (Verified a)] !about	!a -> Task (!Action, !Maybe a)	| descr d & iTask a & iTask about & iTask v

/*
* Ask the user to update predefined shared information.
*
* @param description 							A description of the task to display to the user
* @param (View r v w)							View defining how to convert r to the demanded view v and backwards to w
* @param [PredAction (Valid,r)]					A list of action/predicate pairs (mapped to buttons or menus) through which the user can submit the value
*												If not specified an ok button is provided which is enabled if the editor is valid
* @param about (optional)						Additional information to display
* @param (Shared r w)							Reference to the shared state to update
*
* @return 										Chosen action with the value of the shared state at the moment the task stopped
* @throws										SharedException
*/
updateSharedInformationA		:: !d !(View r v w) ![PredAction (Valid,r)]			!(Shared r w) -> Task (!Action,!r)	| descr d & iTask r & iTask v & iTask w
updateSharedInformationAboutA	:: !d !(View r v w) ![PredAction (Valid,r)] !about	!(Shared r w) -> Task (!Action,!r)	| descr d & iTask r & iTask v & iTask w & iTask about

/*
* Ask the user to select one item from a list of options with already one option pre-selected.
*
* @param description 							A description of the task to display to the user
* @param (a -> v) (optional)					A view for options of type a is generated; This function defines how to map an option to a view value of type v
*												If not specified, a = v, and the view is the identity
* @param [PredAction (Verified a)] (optional)	A list of action/predicate pairs (mapped to buttons or menus) through which the user can submit the value
												If not specified an ok button is provided which is enabled if an option is chosen
* @param about (optional)						Additional information to display
* @param [a]									A list of options
* @param Int									The index of the item which should be pre-selected
*
* @return 										Chosen option or chosen action with the chosen option if present
* @throws										ChoiceException
*/
updateChoice				:: !d												![a] !Int -> Task a						| descr d & iTask a
updateChoiceA 				:: !d !(a -> v) ![PredAction (Verified a)]			![a] !Int -> Task (!Action, Maybe a)	| descr d & iTask a & iTask v
updateChoiceAbout			:: !d 										!about	![a] !Int -> Task a						| descr d & iTask a	& iTask about
updateChoiceAboutA			:: !d !(a -> v) ![PredAction (Verified a)]	!about	![a] !Int -> Task (!Action, Maybe a)	| descr d & iTask a & iTask about & iTask v

/*
* Ask the user to select one item from a list of shared options with already one option pre-selected.
*
* @param description 							A description of the task to display to the user
* @param (a -> v)								A view for options of type a is generated; This function defines how to map an option to a view value of type v
* @param [PredAction (Verified a)]				A list of action/predicate pairs (mapped to buttons or menus) through which the user can submit the value
* @param about (optional)						Additional information to display
* @param (Shared [a] w)							A reference to the shared options
* @param Int									The index of the item which should be pre-selected
*
* @return 										Chosen action with the chosen option if present
* @throws										SharedException
*/
updateSharedChoiceA 		:: !d !(a -> v) ![PredAction (Verified a)]			!(Shared [a] w)	!Int -> Task (!Action, Maybe a)	| descr d & iTask a & iTask v & iTask w
updateSharedChoiceAboutA	:: !d !(a -> v) ![PredAction (Verified a)] !about	!(Shared [a] w)	!Int -> Task (!Action, Maybe a)	| descr d & iTask a & iTask about & iTask v & iTask w

/*
* Ask the user to select a number of items from a list of options with already a number of options pre-selected.
*
*
* @param description 							A description of the task to display to the user
* @param (a -> v) (optional)					A view for options of type a is generated; This function defines how to map an option to a view value of type v
*												If not specified, a = v, and the view is the identity
* @param [PredAction [a]] (optional)			A list of action/predicate pairs (mapped to buttons or menus) through which the user can submit the value
												If not specified an ok button is provided
* @param about (optional)						Additional information to display
* @param [a]									A list of options
* @param [Int]									The indexes of the items which should be pre-selected
*
* @return 										Chosen values or chosen action with the chosen options
*/
updateMultipleChoice		:: !d 										![a] ![Int] -> Task [a]				| descr d & iTask a
updateMultipleChoiceA		:: !d !(a -> v) ![PredAction [a]]			![a] ![Int] -> Task (!Action, [a])	| descr d & iTask a & iTask v
updateMultipleChoiceAbout	:: !d 								!about	![a] ![Int] -> Task [a]				| descr d & iTask a & iTask about
updateMultipleChoiceAboutA	:: !d !(a -> v) ![PredAction [a]]	!about	![a] ![Int] -> Task (!Action, [a])	| descr d & iTask a & iTask about & iTask v

/*
* Ask the user to select one item from a list of shared options with already a number of options pre-selected.
*
* @param description 							A description of the task to display to the user
* @param (a -> v)								A view for options of type a is generated; This function defines how to map an option to a view value of type v
* @param [PredAction (Verified a)]				A list of action/predicate pairs (mapped to buttons or menus) through which the user can submit the value
* @param about (optional)						Additional information to display
* @param (Shared [a] w)							A reference to the shared options
* @param [Int]									The indexes of the items which should be pre-selected
*
* @return 										Chosen action with the chosen option if present
* @throws										SharedException
*/
updateSharedMultipleChoiceA			:: !d !(a -> v) ![PredAction [a]]			!(Shared [a] w) ![Int] -> Task (!Action, [a]) | descr d & iTask a & iTask v & iTask w
updateSharedMultipleChoiceAboutA	:: !d !(a -> v) ![PredAction [a]] !about	!(Shared [a] w) ![Int] -> Task (!Action, [a]) | descr d & iTask a & iTask about & iTask v & iTask w

/*
* Show a basic message to the user. The user can end the task after reading the message. 
*
* @param description 							A description of the task to display to the user
* @param (about -> v) (optional)				A view on the about value, defining how it is shown to the user
												If not specified, v = a, and the view is the identity
* @param [Action] (optional)					A list of buttons or menus, through which the user can submit the value
												All actions are always possible since there is no changing state
												If not specified an ok button is provided
* @param a OR about								The value is just passed for convenience or shown as context information
*
* @return										A copy of value a or about with (optionally) chosen action
*/
showMessage			:: !d							!a		-> Task a					| descr d & iTask a
showMessageA		:: !d ![Action]					!a		-> Task (!Action,!a)		| descr d & iTask a
showMessageAbout	:: !d							!about	-> Task about				| descr d & iTask about
showMessageAboutA	:: !d !(about -> v)	![Action]	!about	-> Task (!Action,!about)	| descr d & iTask about & iTask v

/* 
* Shows a instruction to the user. The user can dismiss the instruction.
*
* @param String									A short descriptive subject
* @param instruction							The instruction
* @param a or about								The value is just passed for convenience or shown as context information		
*
* @return										A copy of value a or about
*/
showInstruction 			:: !String !instruction	!a		-> Task a		| html instruction & iTask a
showInstructionAbout 		:: !String !instruction !about 	-> Task about	| html instruction & iTask about

/**
* Monitors a shared state using a functional view.
* A predicate determines when to continue.
*
* @param description							A description of the task to display to the user
* @param (r -> v)								A view function
* @param (r -> Bool)							A predicate determining when to continue
* @param Bool OR [PredAction r]					A flag indicating if to finish the task automatically if condition is true or let the user press a continue-button
												OR
												a list of actions the user can take before the predicate holds
* @param about (optional)						Additional information to display
* @param (Shared r w)							A reference to the shared state
*
* @return										The last value of the monitored state with (optionally) chosen action
*/
monitor			:: !d !(r -> v) !(r -> Bool) !Bool					!(Shared r w) -> Task r						| descr d & iTask r & iTask v & iTask w
monitorA		:: !d !(r -> v) !(r -> Bool) ![PredAction r]		!(Shared r w) -> Task (!Maybe Action,!r)	| descr d & iTask r & iTask v & iTask w
monitorAbout	:: !d !(r -> v) !(r -> Bool) !Bool !about			!(Shared r w) -> Task r						| descr d & iTask r & iTask v & iTask w & iTask about
monitorAboutA	:: !d !(r -> v) !(r -> Bool) ![PredAction r] !about	!(Shared r w) -> Task (!Maybe Action,!r)	| descr d & iTask r & iTask v & iTask w & iTask about

/**
* Waits until a shared Maybe-state contains a value.
*
* @param description							A description of the task to display to the user
* @param about (optional)						Additional information to display
* @param (Shared (Maybe r) w)					A reference to the shared Maybe-state
*
* @return										The last value of the monitored state
*/
wait		:: !d			!(Shared (Maybe r) w) -> Task r | descr d & iTask r & iTask w
waitAbout	:: !d !about	!(Shared (Maybe r) w) -> Task r | descr d & iTask r & iTask w & iTask about

/**
* Waits until a predicate on a shared state holds.
*
* @param description							A description of the task to display to the user
* @param (r -> Bool)							A predicate on the monitored state
* @param about (optional)						Additional information to display
* @param (Shared r w)							A reference to the shared state
*
* @return										The last value of the monitored state
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
* @param description 							A description of the task to display to the user
* @param [(Action,a)]							A list of actions the user can choose from. Each actions yields the given result if it's chosen. 
*
* @return 										Value associated with chosen action.
*/
chooseAction		:: !d ![(!Action,a)]								-> Task a | descr d & iTask a

/*
* Ask the user to choose an action. The list of actions is calculated dynamically.
*
* @param description 							A description of the task to display to the user
* @param (r -> [(Action,Maybe a)]) 				A list of actions the user can choose from. Each actions yields the given result if it's chosen & result is present (Just). Otherwise (Nothing) action is disabled.
* @param (Shared r w)							The shared value to use. 
*
* @return 										Value associated with chosen action.
*/						
chooseActionDyn 	:: !d !(r -> [(!Action,!Maybe a)]) !(Shared r w)	-> Task a | descr d & iTask a & iTask w