definition module EditTasks


import TSt

/* 
editTask		:: create a task editor to edit a value of given type, and add a button with given name to finish the task
editTaskPred	:: create a task editor (with submit button) to edit a value of given type, finish only if predicate holds 
*/
editTask 		:: !String 	!a 								-> Task a		| iData a 
editTaskPred 	:: 			!a !(a -> (Bool, [HtmlTag]))	-> Task a		| iData a 

/*
editTaskLabel	:: same as editTask, first label used for tracing...
*/
editTaskLabel 	:: !String !String !a 						-> Task a		| iData a 

/**
* Creates a basic task that displays the given html and never finishes.
*/
displayHtml		:: ![HtmlTag] -> Task a											| iCreateAndPrint a
displayValue	:: !a -> Task a													| iData a

/**
* Tasks for offering choices to users
*/
selectTask_cbox :: ![(!Bool,!(Bool [Bool] -> [Bool]),![HtmlTag])] -> Task [Int]	