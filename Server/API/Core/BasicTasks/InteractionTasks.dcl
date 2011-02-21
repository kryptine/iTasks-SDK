definition module InteractionTasks
/*
* This module provides means to interact with users
*/
from Task				import class descr, :: Task
from HTML				import :: HtmlTag
from Shared				import :: Shared
from StdFunc			import id, const
from InteractiveTasks	import :: View, ::SymmetricView, :: TaskAction, :: ActionEvent, :: Verified
import iTaskClass

//*** Input collection tasks ***//
/*
* Ask the user to enter information.
*
* @param description 		A description of the task to display to the user
* @param (v -> a)			A view for type v is generated; This function defines how to map view v back to a value of type a. 
*							If not specified, v = a, and the map back is the identity.
* @param [TaskAction a]		A list of buttons or menus, through which the user can submit the value. 
*
* @return 					Resulting value or chosen action with the resulting value if editor was in valid state.
*/
enterInformation			:: !d 						    -> Task a						| descr d & iTask a
enterInformationA			:: !d !(v -> a) ![TaskAction a] -> Task (!ActionEvent, Maybe a)	| descr d & iTask a & iTask v

/*
* Ask the user to enter information, given some additional context information
*
* @param description 		A description of the task to display to the user
* @param (v -> a)			A view for type v is generated; This function defines how to map view v back to a value of type a. 
*							If not specified, v = a, and the map back is the identity.
* @param [TaskAction a]		A list of buttons or menus, through which the user can submit the value. 
* @param a					Additional information to display
*
* @return 					Resulting value or chosen action with the resulting value if editor was in valid state.
*/
enterInformationAbout		:: !d  				  		    !about -> Task a						| descr d  & iTask a & iTask about
enterInformationAboutA		:: !d !(v -> a) ![TaskAction a] !about -> Task (!ActionEvent, Maybe a)	| descr d  & iTask a & iTask about & iTask v

/*
* Ask the user to update predefined information. 
*
* @param description 		A description of the task to display to the user
* @param View i v o			View defining how to convert i to the demanded view v and backwards to o.
*							If not specified, i = v = o, and the view is the identity view.
* @param [TaskAction a]		A list of buttons or menus, through which the user can submit the value. 
* @param a or (Shared a)	The initial value or shared value to use. 
*
* @return 					Resulting value or chosen action with the resulting value if editor was in valid state.
*/
updateInformation			:: !d										!a				-> Task a							| descr d & iTask a
updateInformationA			:: !d !(SymmetricView a v)	![TaskAction a]	!a				-> Task (!ActionEvent, !Maybe a)	| descr d & iTask a & iTask v
updateSharedInformationA	:: !d !(View i v o)			![TaskAction i]	!(Shared i o)	-> Task (!ActionEvent, !Maybe i)	| descr d & iTask i & iTask v & iTask o

/*
* Ask the user to update predefined information, given some additonal context information
*
* @param description 		A description of the task to display to the user
* @param View i v o			View defining how to convert i to the demanded view v and backwards to o.
*							If not specified, i = v = o, and the view is the identity view.
* @param [TaskAction a]		A list of buttons or menus, through which the user can submit the value. 
* @param b					Additional information to display
* @param a or (Shared a)	The initial value or shared value to use. 
*
* @return 					Resulting value or chosen action with the resulting value if editor was in valid state.
*/
updateInformationAbout			:: !d 										!about !a				-> Task a							| descr d & iTask a & iTask about
updateInformationAboutA			:: !d !(SymmetricView a v)	![TaskAction a]	!about !a				-> Task (!ActionEvent, !Maybe a)	| descr d & iTask a & iTask about & iTask v
updateSharedInformationAboutA	:: !d !(View i v o)			![TaskAction i] !about !(Shared i o)	-> Task (!ActionEvent, !Maybe i)	| descr d & iTask i & iTask v & iTask o & iTask about

/*
* Asks the user to confirm or decline a question.
*
* @param description 		A description of the task to display to the user
*
* @return 					A boolean indicating 'accepted' (True) or 'declined' (False)
*/
requestConfirmation			:: !d -> Task Bool	| descr d 

/*
* Asks the user to accept or decline a question, given some additional context information
*
* @param description 		A description of the task to display to the user
* @param a					Additional context information to show to the user
*
* @return 					A boolean indiciating 'accepted' (True) or 'declined' (False)
*/
requestConfirmationAbout	:: !d !about -> Task Bool	| descr d & iTask about

/*
* Ask the user to select one item from a list of options
*
* @param description 		A description of the task to display to the user
* @param (a -> v)			A view for options of type a is generated; This function defines how to map an option to a view value of type v. 
*							If not specified, a = v, and the view is the identity.
* @param [TaskAction a]		A list of buttons or menus, through which the user can submit the value. 
* @param [a]				A list of (shared) options
*
* @return 					Resulting value or chosen action with the resulting value if editor was in valid state.
*/
enterChoice					:: !d 					  		![a]			-> Task a						| descr d & iTask a
enterChoiceA				:: !d !(a -> v) ![TaskAction a] ![a]			-> Task (!ActionEvent, Maybe a)	| descr d & iTask a & iTask v
//enterSharedChoiceA			:: !d !(a -> v) ![TaskAction a] !(Shared [a] w)	-> Task (!ActionEvent, Maybe a)	| descr d & iTask a & iTask v

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
* @return 					Resulting value or chosen action with the resulting value if editor was in valid state.
*/
enterChoiceAbout			:: !d 							!about ![a]				-> Task a						| descr d & iTask a	& iTask about
enterChoiceAboutA			:: !d !(a -> v) ![TaskAction a] !about ![a]				-> Task (!ActionEvent, Maybe a)	| descr d & iTask a & iTask about & iTask v
//enterSharedChoiceAboutA		:: !d !(a -> v) ![TaskAction a] !b !(Shared [a] w)	-> Task (!ActionEvent, Maybe a)	| descr d & iTask a & iTask b & iTask v

/*
* Ask the user to select one item from a list of options with already one option pre-selected
*
* @param description 		A description of the task to display to the user
* @param (a -> v)			A view for options of type a is generated; This function defines how to map an option to a view value of type v. 
*							If not specified, a = v, and the view is the identity.
* @param [TaskAction a]		A list of buttons or menus, through which the user can submit the value. 
* @param [a]				A list of (shared) options
* @param Int				The index of the item which should be pre-selected
*
* @return 					Resulting value or chosen action with the resulting value if editor was in valid state.
*/
updateChoice				:: !d							![a]			!Int -> Task a							| descr d & iTask a	
updateChoiceA 				:: !d !(a -> v) ![TaskAction a]	![a]			!Int -> Task (!ActionEvent, Maybe a)	| descr d & iTask a & iTask v 
//updateSharedChoiceA 		:: !d !(a -> v) ![TaskAction a] !(Shared [a] w)	!Int -> Task (!ActionEvent, Maybe a)	| descr d & iTask a & iTask v


/*
* Ask the user to select one item from a list of options with already one option pre-selected, given some context information
*
* @param description 		A description of the task to display to the user
* @param (a -> v)			A view for options of type a is generated; This function defines how to map an option to a view value of type v. 
*							If not specified, a = v, and the view is the identity.
* @param [TaskAction a]		A list of buttons or menus, through which the user can submit the value. 
* @param b					Additional information to display
* @param [a]				A list of (shared) options
* @param Int				The index of the item which should be pre-selected
*
* @return 					Resulting value or chosen action with the resulting value if editor was in valid state.
*/
updateChoiceAbout			:: !d 							!about ![a]				!Int -> Task a							| descr d & iTask a	& iTask about
updateChoiceAboutA			:: !d !(a -> v) ![TaskAction a] !about ![a]				!Int -> Task (!ActionEvent, Maybe a)	| descr d & iTask a & iTask about & iTask v
//updateSharedChoiceAboutA	:: !d !(a -> v) ![TaskAction a] !b !(Shared [a] w)	!Int -> Task (!ActionEvent, Maybe a)	| descr d & iTask a & iTask b & iTask v

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
* @return 					Resulting value or chosen action with the resulting value if editor was in valid state.
*/
enterMultipleChoice			:: !d 					  			![a]			-> Task [a]							| descr d & iTask a
enterMultipleChoiceA		:: !d !(a -> v) ![TaskAction [a]]	![a]			-> Task (!ActionEvent, Maybe [a])	| descr d & iTask a & iTask v
//enterSharedMultipleChoiceA	:: !d !(a -> v) ![TaskAction [a]]	!(Shared [a] w)	-> Task (!ActionEvent, Maybe [a])	| descr d & iTask a & iTask v

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
* @return 					Resulting value or chosen action with the resulting value if editor was in valid state.
*/
enterMultipleChoiceAbout		:: !d 								!about ![a]				-> Task [a]							| descr d & iTask a	& iTask about
enterMultipleChoiceAboutA		:: !d !(a -> v) ![TaskAction [a]]	!about ![a]				-> Task (!ActionEvent, Maybe [a])	| descr d & iTask a & iTask about & iTask v
//enterSharedMultipleChoiceAboutA	:: !d !(a -> v) ![TaskAction [a]]	!b !(Shared [a] w)	-> Task (!ActionEvent, Maybe [a])	| descr d & iTask a & iTask b & iTask v

/*
* Ask the user to select a number of items from a list of options with already some options pre-selected
*
* @param description 		A description of the task to display to the user
* @param (a -> v)			A view for options of type a is generated; This function defines how to map an option to a view value of type v. 
*							If not specified, a = v, and the view is the identity.
* @param [TaskAction a]		A list of buttons or menus, through which the user can submit the value. 
* @param [a]				A list of (shared) options
* @param [Int]				The index of the items which should be pre-selected
*
* @return 					Resulting value or chosen action with the resulting value if editor was in valid state.
*/
updateMultipleChoice		:: !d 								![a]			![Int] -> Task [a]							| descr d & iTask a
updateMultipleChoiceA		:: !d !(a -> v) ![TaskAction [a]]	![a]			![Int] -> Task (!ActionEvent, Maybe [a])	| descr d & iTask a & iTask v
//updateSharedMultipleChoiceA :: !d !(a -> v) ![TaskAction [a]]	!(Shared [a] w)	![Int] -> Task (!ActionEvent, Maybe [a])	| descr d & iTask a & iTask v

/*
* Ask the user to select a number of items from a list of options with already some options pre-selected, given additional context information
*
* @param description 		A description of the task to display to the user
* @param (a -> v)			A view for options of type a is generated; This function defines how to map an option to a view value of type v. 
*							If not specified, a = v, and the view is the identity.
* @param [TaskAction a]		A list of buttons or menus, through which the user can submit the value. 
* @param b					Additional information to display
* @param [a]				A list of (shared) options
* @param [Int]				The index of the items which should be pre-selected
*
* @return 					Resulting value or chosen action with the resulting value if editor was in valid state.
*/
updateMultipleChoiceAbout		 :: !d 								!about ![a] 			![Int] -> Task [a]							| descr d & iTask a	& iTask about
updateMultipleChoiceAboutA		 :: !d !(a -> v) ![TaskAction [a]]	!about ![a] 			![Int] -> Task (!ActionEvent, Maybe [a])	| descr d & iTask a & iTask about & iTask v
//updateSharedMultipleChoiceAboutA :: !d !(a -> v) ![TaskAction [a]]	!b !(Shared [a] w)	![Int] -> Task (!ActionEvent, Maybe [a])	| descr d & iTask a & iTask b & iTask v

//*** Output tasks ***//

/*
* Show a basic message to the user. The user can end the task after reading the message. 
*
* @param description 		A description of the task to display to the user
* @param message 			A message to display to the user
* @param [TaskAction a]		A list of buttons or menus, through which the user can submit the value. 
* @param a					The value is just passed for convenience
*
* @return					A copy of value a with (optionally) chosen action
*/
showMessage		:: !d					a -> Task a					| descr d & iTask a
showMessageA	:: !d ![TaskAction a]	a -> Task (!ActionEvent, a)	| descr d & iTask a

/*
* Show a basic message and additional context information to the user. The user can end the task after reading the message. 
*
* @param description 		A description of the task to display to the user
* @param message 			A message to display to the user
* @param (a -> v)			Map defining how to convert message b to the demanded view v
*							If not specified, v = a, and the map is the identity.
* @param [TaskAction a]		A list of buttons or menus, through which the user can submit the value. 
* @param a or (Shared a)	Additional (local or shared) information to display
*
* @return					A (copy of) value a with (optionally) chosen action
*/
showMessageAbout	:: !d									!about			-> Task about					| descr d & iTask about
showMessageAboutA	:: !d !(about -> v)	![TaskAction about]	!about			-> Task (!ActionEvent, about)	| descr d & iTask about & iTask v
showMessageSharedA	:: !d !(i -> v)		![TaskAction i]		!(Shared i o)	-> Task (!ActionEvent, i)		| descr d & iTask i & iTask v

/*
* Show a basic message to the user. The user cannot end the task after reading the message. 
*
* @param description 		A description of the task to display to the user
* @param message 			A message to display to the user
* @param a					The value is just passed for convenience
*
* @return					A copy of value a with (optionally) chosen action
*/
showStickyMessage	:: !d	a	-> Task a	| descr d & iTask a

/*
* Show a basic message and some context information to the user. The user cannot end the task after reading the message. 
*
* @param description 		A description of the task to display to the user
* @param message 			A message to display to the user
* @param (a -> v)			Map defining how to convert message b to the demanded view v
*							If not specified, v = a, and the map is the identity.
* @param [TaskAction a]		A list of buttons or menus, through which the user can submit the value. 
* @param a or (DbId a)		Additional (local or shared) information to display
*
* @return					A (copy of) value
*/
showStickyMessageAbout	:: !d			!about			-> Task about	| descr d & iTask about
showStickyMessageShared	:: !d !(i -> v)	!(Shared i o)	-> Task i		| descr d & iTask i & iTask v

//*** Instruction tasks ***//

/* 
* Shows a instruction to the user. The user can dismiss the instruction.
*
* @param String				A short descriptive subject
* @param instruction		The instruction
* @param a					The value that is returned when the task is finished		
*
* @return					a
*/
showInstruction 			:: !String !instruction	a	-> Task a	| html instruction & iTask a

/* 
* Shows a instruction and additional context information to the user. The user can dismiss the instruction.
*
* @param String				A title message
* @param instruction		The instruction
* @param a					Additional context information
*
* @return					a
*/
showInstructionAbout 		:: !String !instruction !about 	-> Task about | html instruction & iTask about
