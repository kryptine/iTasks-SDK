definition module CommonCombinators
/**
* This module contains a collection of handy iTasks combinators defined in terms of the basic iTask combinators
* with Thanks to Erik Zuurbier for suggesting some of the advanced combinators
*/

import CoreCombinators, TuningCombinators, InteractionTasks
import Either

from Types import :: User (..)

// A task with a label used for labeling buttons, pulldown menus, and the like
:: LabeledTask a	:== (!String,!Task a)		

//Task composition
(-||-) infixr 3 	:: !(Task a) !(Task a) 						-> Task a 				| iTask a
(-&&-) infixr 4 	:: !(Task a) !(Task b) 						-> Task (a,b) 			| iTask a & iTask b

anyTask				:: ![Task a]								-> Task a				| iTask a
allTasks			:: ![Task a]								-> Task [a]				| iTask a
eitherTask			:: !(Task a) !(Task b) 						-> Task (Either a b)	| iTask a & iTask b	

(||-) infixr 3		:: !(Task a) !(Task b)						-> Task b				| iTask a & iTask b
(-||) infixl 3		:: !(Task a) !(Task b)						-> Task a				| iTask a & iTask b

//Task composition for optional values
(>>?)	infixl 1	:: !(Task (Maybe a)) !(a -> Task (Maybe b))	-> Task (Maybe b) 		| iTask a & iTask b
(-&?&-)	infixr 4	:: !(Task (Maybe a)) !(Task (Maybe b)) 		-> Task (Maybe (a,b)) 	| iTask a & iTask b

//Task delegation
class (@:) infix 3 u :: u !(LabeledTask a) -> Task a | iTask a

instance @: UserId
instance @: User
instance @: String

assignByName		:: !String !String !TaskPriority !(Maybe Timestamp) (Task a) -> Task a	| iTask a

/* Handling recursion and loops:
repeatTask		:: repeat Task until predicate is valid
(<|)			:: repeat task (recursively) as long as predicate does not hold, and give error message otherwise
*/
repeatTask		:: !(a -> Task a) !(a -> Bool) a 			-> Task a					| iTask a
(<|)  infixl 6 	:: !(Task a)  !(a -> (Bool, [HtmlTag])) 	-> Task a 					| iTask a
