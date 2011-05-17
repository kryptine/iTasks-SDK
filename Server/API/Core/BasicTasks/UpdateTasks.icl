implementation module UpdateTasks

import StdTuple, StdList, StdOrdList, StdBool, StdMisc, Functor
import Types, Util, TSt, ExceptionCombinators, CoreCombinators, CommonCombinators, CoreTasks, InteractionTasks
from StdFunc 		import id, const, o
from SharedTasks	import sharedStore, :: SharedStoreId
from SharedTasks	import qualified readShared, writeShared
from GenVerify		import verifyValue

//Local update
updateInformation :: !d !a -> Task a | descr d & iTask a
updateInformation d a = updateInformation` d a voidNothing

updateInformationA :: !d !(SymmetricView a v) ![PredAction (Verified a)] !a -> Task (!Action, !Maybe a) | descr d & iTask a & iTask v
updateInformationA d view actions a = updateInformationA` d view actions a voidNothing

updateInformationAbout :: !d !about !a -> Task a | descr d & iTask a & iTask about
updateInformationAbout d about a = updateInformation` d a (Just about)

updateInformationAboutA :: !d !(SymmetricView a v) ![PredAction (Verified a)] !about !a -> Task (!Action, !Maybe a) | descr d & iTask a & iTask about & iTask v
updateInformationAboutA d view actions about a = updateInformationA` d view actions a (Just about)

updateInformation` d a mbAbout
	= UpdateTask @>> interactLocal
		d
		(\(valid,a) -> addAbout mbAbout [UpdateView (if valid (FormValue a) (Unchanged (FormValue a)),\mbA -> (isJust mbA,fromMaybe a mbA))])
		(\(valid,a) -> okAction (if valid (Just a) Nothing))
		(verifyValue a,a)

updateInformationA` d (get,putback) actions a mbAbout
	= UpdateTask @>> interactLocal
		d
		(\(valid,a) -> addAbout mbAbout [UpdateView (if valid (FormValue (get a)) (Unchanged (FormValue (get a))),\mbV -> (isJust mbV,maybe a (\v -> putback v a) mbV))])
		(fromPredActionsLocal (\(valid,a) -> if valid (Valid a) Invalid) (\action (valid,a) -> (action,if valid (Just a) Nothing)) actions)
		(verifyValue a,a)

//Shared update
updateSharedInformationA :: !d !(View r v w) ![PredAction (Valid,r)] !(Shared r w) -> Task (!Action,!r) | descr d & iTask r & iTask v & iTask w
updateSharedInformationA d view actions shared = updateSharedInformationA` d view actions shared voidNothing

updateSharedInformationAboutA :: !d !(View r v w) ![PredAction (Valid,r)] !about !(Shared r w) -> Task (!Action,!r) | descr d & iTask r & iTask v & iTask w & iTask about
updateSharedInformationAboutA d view actions about shared = updateSharedInformationA` d view actions shared (Just about)

updateSharedInformationA` d (get,putback) actions shared mbAbout
	=  UpdateTask @>> interact
		d
		(\valid r changed -> addAbout mbAbout [UpdateView (if (valid || changed) (FormValue (get r)) (Unchanged (FormValue (get r))),\mbV -> (isJust mbV,fmap (\v -> putback v r) mbV))])
		(fromPredActions (\valid r changed -> ((valid || changed) && verifyValue (get r),r)) (\action _ r _ -> (action,r)) actions)
		True
		shared

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
updateMultipleChoice :: !d ![a] ![Int] -> Task [a] | descr d & iTask a
updateMultipleChoice _ _ _ = undef

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