definition module InputTasks
/*
* This module provides means to let the user input information
*/
from Task				import class descr, :: Task
from HTML				import :: HtmlTag
from Shared				import :: Shared
from StdFunc			import id, const
from InteractionTasks	import :: PredAction, :: Verified, :: Valid
import iTaskClass

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

enterSharedChoiceA			:: !d !(a -> v) ![PredAction (Verified a)]			!(Shared [a] w) -> Task (!Action, Maybe a) | descr d & iTask a & iTask v & iTask w
enterSharedChoiceAboutA		:: !d !(a -> v) ![PredAction (Verified a)] !about	!(Shared [a] w) -> Task (!Action, Maybe a) | descr d & iTask a & iTask about & iTask v & iTask w

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