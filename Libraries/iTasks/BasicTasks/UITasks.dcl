definition module UITasks

import iDataSettings

from TSt			import :: Task
from Html			import :: HtmlTag

/* 
editTask		:: create a task editor to edit a value of given type, and add a button with given name to finish the task
editTaskPred	:: create a task editor (with submit button) to edit a value of given type, finish only if predicate holds 
*/
editTask 		:: !String 	!a 								-> Task a		| iData a 
editTaskPred 	:: 			!a !(a -> (Bool, [HtmlTag]))	-> Task a		| iData a 

/**
* Creates a basic task that displays the given html and never finishes.
*/
displayHtml		:: ![HtmlTag] -> Task a										| iData a
displayValue	:: !a -> Task b												| iData a & iData b

/**
* Tasks for offering choices to users
*/
selectWithButtons		:: ![String]										-> Task Int			
selectWithPulldown		:: ![String] !Int									-> Task Int
selectWithRadiogroup	:: ![[HtmlTag]]	!Int								-> Task Int
selectWithCheckboxes	:: ![(![HtmlTag], !Bool, !(Bool [Bool] -> [Bool]))]	-> Task [Int]	