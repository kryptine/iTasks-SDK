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

(||-) infixr 3		:: !(Task a) !(Task b)						-> Task b				| iTask a & iTask b

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

displayHtml	msg	:== showMessage msg >>| return defaultValue
displayValue x	:== showMessageAbout "" x >>| return defaultValue

//Choose the tasks you want to do on forehand:
//chooseTask		:: ![HtmlTag] ![LabeledTask a] 				-> Task a 		| iTask a
chooseTask msg tasks :== enterChoice msg [t <<@ l \\ (l,t) <- tasks] >>= \task -> task

//mchoiceTasks 	:: ![HtmlTag] ![LabeledTask a] 				-> Task [a] 	| iTask a
mchoiceTasks msg tasks :== enterChoice msg [t <<@ l \\ (l,t) <- tasks] >>= \sel -> sequence "mchoiceTasks" sel

//buttonTask		:: !String   !(Task a)						-> Task a 		| iTask a
buttonTask s t :== enterChoice "" [t <<@ s] >>= \task -> task

//button :: !String !a -> Task a | iTask a
button s a :== enterChoice "" [s] >>| return a

//ok :: Task Void
ok :== showMessage ""

/* Do m Tasks parallel / interleaved and FINISH as soon as SOME Task completes:

andTasks_mu		:: assign task to indicated users, task completed when all done
*/

andTasks_mu 	:: !String ![(Int,Task a)]					-> Task [a] 	| iTask a
