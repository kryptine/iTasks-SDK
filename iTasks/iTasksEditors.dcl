definition module iTasksEditors

// *********************************************************************************************************************************
// Basic iTasks Editors
// *********************************************************************************************************************************
// iTask & iData Concept and Implementation: (c) 2006,2007,2008 - Rinus Plasmeijer
// *********************************************************************************************************************************
//
import iTasksHandler

/* 
editTask		:: create a task editor to edit a value of given type, and add a button with given name to finish the task
editTask		:: create a task editor (with submit button) to edit a value of given type, finish only if predicate holds 
*/
editTask 		:: !String 	!a 								-> Task a		| iData a 
editTaskPred 	:: 			!a !(a -> (Bool, HtmlCode))		-> Task a		| iData a 

editTaskLabel 	:: !String !String !a -> (Task a) | iData a 
