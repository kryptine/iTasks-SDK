definition module UITasks

import TSt

/* 
editTask		:: create a task editor to edit a value of given type, and add a button with given name to finish the task
editTaskPred	:: create a task editor (with submit button) to edit a value of given type, finish only if predicate holds 
*/
editTask 		:: !String 	!a 								-> Task a		| iData a 
editTaskPred 	:: 			!a !(a -> (Bool, [HtmlTag]))	-> Task a		| iData a 

/**
* Creates a basic task that displays the given html and never finishes.
*/
displayHtml		:: ![HtmlTag] -> Task a										| iCreateAndPrint a
displayValue	:: !a -> Task b												| iData a & iCreateAndPrint b

/**
* Tasks for offering choices to users
*/
selectWithButtons		:: ![String]	!Bool								-> Task Int			
selectWithPulldown		:: ![String]	!Int								-> Task Int
//selectWithRadioGroup	:: ![[HtmlTag]]	!Int								-> Task Int
selectWithCheckboxes	:: ![(![HtmlTag], !Bool, !(Bool [Bool] -> [Bool]))]	-> Task [Int]	