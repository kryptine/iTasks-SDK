definition module InputTasks
/*
* This module provides means to let the user input information
*/
from Task				import class descr, :: Task
from HTML				import :: HtmlTag
from Shared				import :: Shared
from StdFunc			import id, const
from InteractionTasks	import :: PredAction, :: Verified
import iTaskClass

/*
* Ask the user to enter information.
*
* @param description 		A description of the task to display to the user
* @param (v -> a)			A view for type v is generated; This function defines how to map view v back to a value of type a. 
*							If not specified, v = a, and the map back is the identity.
* @param [TaskAction a]		A list of buttons or menus, through which the user can submit the value. 
*
* @param a					Additional information to display
* @return 					Resulting value or chosen action with the resulting value if editor was in valid state.
*/
enterInformation			:: !d 						    					-> Task a					| descr d & iTask a
enterInformationA			:: !d !(v -> a) ![PredAction (Verified a)]			-> Task (!Action,!Maybe a)	| descr d & iTask a & iTask v
enterInformationAbout		:: !d 										!about	-> Task a					| descr d & iTask a & iTask about
enterInformationAboutA		:: !d !(v -> a) ![PredAction (Verified a)]	!about	-> Task (!Action,!Maybe a)	| descr d & iTask a & iTask about & iTask v

/*
* Asks the user to confirm or decline a question.
*
* @param description 		A description of the task to display to the user
*
* @return 					A boolean indicating 'accepted' (True) or 'declined' (False)
*/
requestConfirmation			:: !d			-> Task Bool | descr d 
requestConfirmationAbout	:: !d !about	-> Task Bool | descr d & iTask about

/*
* Ask the user to select one item from a list of options
*
* @param description 		A description of the task to display to the user
* @param (a -> v)			A view for options of type a is generated; This function defines how to map an option to a view value of type v. 
*							If not specified, a = v, and the view is the identity.
* @param [TaskAction a]		A list of buttons or menus, through which the user can submit the value. 
* @param [a]				A list of (shared) options
*
* @return 					Chosen value or chosen action with the chosen value if editor was in valid state.
* @throws					ChoiceException, SharedException (enterSharedChoiceA only)
*/
enterChoice					:: !d 					  							![a] -> Task a					| descr d & iTask a
enterChoiceA				:: !d !(a -> v) ![PredAction (Verified a)] 			![a] -> Task (!Action,!Maybe a)	| descr d & iTask a & iTask v
enterChoiceAbout			:: !d 										!about	![a] -> Task a					| descr d & iTask a	& iTask about
enterChoiceAboutA			:: !d !(a -> v) ![PredAction (Verified a)]	!about	![a] -> Task (!Action,!Maybe a)	| descr d & iTask a & iTask about & iTask v

/*
* Ask the user to select one item from a list of options, given some context information
*
* @param description 		A description of the task to display to the user
* @param (a -> v)			A view for options of type a is generated; This function defines how to map an option to a view value of type v. 
*							If not specified, a = v, and the view is the identity.
* @param [TaskAction a]		A list of buttons or menus, through which the user can submit the value. 
* @param b					Additional information to display
* @param [a]				A list of (shared) options
*
* @return 					Chosen value or chosen action with the chosen value if editor was in valid state.
* @throws					ChoiceException, SharedException (enterSharedChoiceAboutA only)
*/

enterSharedChoiceA			:: !d !(a -> v) ![PredAction (Verified a)] !(Shared [a] w)	-> Task (!Action, Maybe a)	| descr d & iTask a & iTask v
//enterSharedChoiceAboutA		:: !d !(a -> v) ![TaskAction a] !about !(Shared [a] w)	-> Task (!Action, Maybe a)	| descr d & iTask a & iTask about & iTask v

/*
* Ask the user to select a number of items from a list of options
*
*
* @param description 		A description of the task to display to the user
* @param (a -> v)			A view for options of type a is generated; This function defines how to map an option to a view value of type v. 
*							If not specified, a = v, and the view is the identity.
* @param [TaskAction a]		A list of buttons or menus, through which the user can submit the value. 
* @param [a]				A list of (shared) options
*
* @return 					Chosen values or chosen action with the chosen values.
* @throws					SharedException (enterSharedMultipleChoiceA only)
*/
enterMultipleChoice			:: !d 					  			![a]			-> Task [a]				| descr d & iTask a
enterMultipleChoiceA		:: !d !(a -> v) ![PredAction [a]]	![a]			-> Task (!Action,![a])	| descr d & iTask a & iTask v
enterMultipleChoiceAbout		:: !d 								!about ![a]				-> Task [a]				| descr d & iTask a	& iTask about
enterMultipleChoiceAboutA		:: !d !(a -> v) ![PredAction [a]]	!about ![a]				-> Task (!Action,![a])	| descr d & iTask a & iTask about & iTask v


/*
* Ask the user to select a number of items from a list of options, given additional context information
*
* @param description 		A description of the task to display to the user
* @param (a -> v)			A view for options of type a is generated; This function defines how to map an option to a view value of type v. 
*							If not specified, a = v, and the view is the identity.
* @param [TaskAction a]		A list of buttons or menus, through which the user can submit the value. 
* @param b					Additional information to display
* @param [a]				A list of (shared) options
*
* @return 					Chosen values or chosen action with the chosen values.
* @throws					SharedException (enterSharedMultipleChoiceAboutA only)
*/
//enterSharedMultipleChoiceA	:: !d !(a -> v) ![TaskAction [a]]	!(Shared [a] w)	-> Task (!Action, [a])	| descr d & iTask a & iTask v
//enterSharedMultipleChoiceAboutA	:: !d !(a -> v) ![TaskAction [a]]	!about !(Shared [a] w)	-> Task (!Action, [a])	| descr d & iTask a & iTask about & iTask v