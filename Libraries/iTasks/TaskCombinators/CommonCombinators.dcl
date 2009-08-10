definition module CommonCombinators
/**
* This module contains a collection of handy iTasks combinators defined in terms of the basic iTask combinators
* with Thanks to Erik Zuurbier for suggesting some of the advanced combinators
*/

import CoreCombinators, TuningCombinators, InteractionTasks
import Either

// A task with a label used for labeling buttons, pulldown menus, and the like
:: LabeledTask a	:== (!String,!Task a)		

//Task composition
(-||-) infixr 3 	:: !(Task a) !(Task a) 						-> Task a 				| iTask a
(-&&-) infixr 4 	:: !(Task a) !(Task b) 						-> Task (a,b) 			| iTask a & iTask b

anyTask				:: ![Task a]								-> Task a				| iTask a
allTasks			:: ![Task a]								-> Task [a]				| iTask a
eitherTask			:: !(Task a) !(Task b) 						-> Task (Either a b)	| iTask a & iTask b	

//Task composition for optional values
(>>?)	infixl 1	:: !(Task (Maybe a)) !(a -> Task (Maybe b))	-> Task (Maybe b) 		| iTask a & iTask b
(-&?&-)	infixr 4	:: !(Task (Maybe a)) !(Task (Maybe b)) 		-> Task (Maybe (a,b)) 	| iTask a & iTask b

//Task delegation
(@:)   infix 3 		:: !UserId !(LabeledTask a)					-> Task a				| iTask a 

/* Handling recursion and loops:
repeatTask		:: repeat Task until predicate is valid
(<|)			:: repeat task (recursively) as long as predicate does not hold, and give error message otherwise
*/
repeatTask		:: !(a -> Task a) !(a -> Bool) a 			-> Task a					| iTask a
(<|)  infixl 6 	:: !(Task a)  !(a -> (Bool, [HtmlTag])) 	-> Task a 					| iTask a


//TO BE OBSOLETE

/**
* Select n tasks to do out of m (n <= m) and execute them in indicated order. First a
* selecting task is executed which determines which tasks have to be done. Then these selected
* tasks are combined by
*
* @param A selecting task that given a list of labeled tasks yields a list of indexes in the list
*        of labeled tasks. The tasks with these indexes will be executed.
* @param A task that combines a list of labeled tasks into a single task
* @return The combined task
*/
selection :: !([LabeledTask a] -> Task [Int]) !([LabeledTask a] -> Task [a]) ![LabeledTask a] -> Task [a] | iTask a

editTask s a :== requestInformationWD "Please edit this value" a 

displayHtml	msg	:== showMessage msg >>| return defaultValue
displayValue x	:== showMessageAbout "" x >>| return defaultValue

//Choose the tasks you want to do on forehand:
//chooseTask		:: ![HtmlTag] ![LabeledTask a] 				-> Task a 		| iTask a
chooseTask msg tasks :== requestChoice msg [t <<@ l \\ (l,t) <- tasks] >>= \task -> task

//mchoiceTasks 	:: ![HtmlTag] ![LabeledTask a] 				-> Task [a] 	| iTask a
mchoiceTasks msg tasks :== requestChoice msg [t <<@ l \\ (l,t) <- tasks] >>= \sel -> sequence "mchoiceTasks" sel


buttonTask		:: !String   !(Task a)						-> Task a 		| iTask a


/* Do m Tasks parallel / interleaved and FINISH as soon as SOME Task completes:

andTasks_mu		:: assign task to indicated users, task completed when all done
*/

andTasks_mu 	:: !String ![(Int,Task a)]					-> Task [a] 	| iTask a
