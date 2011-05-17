definition module UpdateTasks
/*
* This module provides means to let the user update information
*/
from Task				import class descr, :: Task
from HTML				import :: HtmlTag
from Shared				import :: Shared
from StdFunc			import id, const
from CoreTasks			import :: Valid, :: PredAction, :: Verified
import iTaskClass

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
* @param description								A description of the task to display to the user
* @param (SymmetricView a v) (optional)				View defining how to convert a to the demanded view v and backwards to a
*													If not specified, a = v, and the view is the identity view
* @param [PredAction (Verified a)]					A list of action/predicate pairs (mapped to buttons or menus) through which the user can submit the value
*													If not specified an ok button is provided which is enabled if the editor is valid
* @param about (optional)							Additional information to display
* @param a											The initial value to use
*
* @return 											Resulting value or chosen action with the resulting value if editor was in valid state.
*/
updateInformation			:: !d															!a -> Task a					| descr d & iTask a
updateInformationA			:: !d !(SymmetricView a v)	![PredAction (Verified a)]			!a -> Task (!Action, !Maybe a)	| descr d & iTask a & iTask v
updateInformationAbout		:: !d !about													!a -> Task a					| descr d & iTask a & iTask about
updateInformationAboutA		:: !d !(SymmetricView a v)	![PredAction (Verified a)] !about	!a -> Task (!Action, !Maybe a)	| descr d & iTask a & iTask about & iTask v

/*
* Ask the user to update predefined shared information.
*
* @param description 								A description of the task to display to the user
* @param (View r v w)								View defining how to convert r to the demanded view v and backwards to w
* @param [PredAction (Valid,r)]						A list of action/predicate pairs (mapped to buttons or menus) through which the user can submit the value
*													If not specified an ok button is provided which is enabled if the editor is valid
* @param about (optional)							Additional information to display
* @param (Shared r w)								Reference to the shared state to update
*
* @return 											Chosen action with the value of the shared state at the moment the task stopped
* @throws											SharedException
*/
updateSharedInformationA		:: !d !(View r v w) ![PredAction (Valid,r)]			!(Shared r w) -> Task (!Action,!r)	| descr d & iTask r & iTask v & iTask w
updateSharedInformationAboutA	:: !d !(View r v w) ![PredAction (Valid,r)] !about	!(Shared r w) -> Task (!Action,!r)	| descr d & iTask r & iTask v & iTask w & iTask about

/*
* Ask the user to select one item from a list of options with already one option pre-selected
*
* @param description								A description of the task to display to the user
* @param (a -> v)			A view for options of type a is generated; This function defines how to map an option to a view value of type v. 
*							If not specified, a = v, and the view is the identity.
* @param [TaskAction a]		A list of buttons or menus, through which the user can submit the value. 
* @param [a]				A list of (shared) options
* @param Int				The index of the item which should be pre-selected
*
* @return 					Chosen value or chosen action with the chosen value if editor was in valid state.
* @throws					ChoiceException, SharedException (updateSharedChoiceA only)
*/
//updateChoice				:: !d							![a]			!Int -> Task a					| descr d & iTask a	
//updateChoiceA 				:: !d !(a -> v) ![PredAction (Verified a)]	![a]			!Int -> Task (!Action, Maybe a)	| descr d & iTask a & iTask v 
//updateChoiceAbout			:: !d 							!about ![a]				!Int -> Task a					| descr d & iTask a	& iTask about
//updateChoiceAboutA			:: !d !(a -> v) ![PredAction (Verified a)] !about ![a]				!Int -> Task (!Action, Maybe a)	| descr d & iTask a & iTask about & iTask v


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
* @return 					Chosen value or chosen action with the chosen value if editor was in valid state.
* @throws					ChoiceException, SharedException (updateSharedChoiceAboutA only)
*/
//updateSharedChoiceA 		:: !d !(a -> v) ![PredAction (Verified a)] !(Shared [a] w)	!Int -> Task (!Action, Maybe a)	| descr d & iTask a & iTask v
//updateSharedChoiceAboutA	:: !d !(a -> v) ![PredAction (Verified a)] !about !(Shared [a] w)	!Int -> Task (!Action, Maybe a)	| descr d & iTask a & iTask about & iTask v

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
* @return 					Chosen values or chosen action with the chosen values.
* @throws					SharedException (updateSharedMultipleChoiceA only)
*/
updateMultipleChoice		:: !d 								![a]			![Int] -> Task [a]				| descr d & iTask a
//updateMultipleChoiceA		:: !d !(a -> v) ![PredAction [a]]	![a]			![Int] -> Task (!Action, [a])	| descr d & iTask a & iTask v
//updateMultipleChoiceAbout		 :: !d 								!about ![a] 			![Int] -> Task [a]				| descr d & iTask a	& iTask about
//updateMultipleChoiceAboutA		 :: !d !(a -> v) ![PredAction [a]]	!about ![a] 			![Int] -> Task (!Action, [a])	| descr d & iTask a & iTask about & iTask v

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
* @return 					Chosen values or chosen action with the chosen values.
* @throws					SharedException (updateSharedMultipleChoiceAboutA only)
*/
//updateSharedMultipleChoiceA :: !d !(a -> v) ![PredAction [a]]	!(Shared [a] w)	![Int] -> Task (!Action, [a])	| descr d & iTask a & iTask v
//updateSharedMultipleChoiceAboutA :: !d !(a -> v) ![PredAction [a]]	!about !(Shared [a] w)	![Int] -> Task (!Action, [a])	| descr d & iTask a & iTask about & iTask v