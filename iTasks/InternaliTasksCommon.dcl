definition module InternaliTasksCommon

// *********************************************************************************************************************************
// internally used types and utility function
// *********************************************************************************************************************************
// iTask & iData Concept and Implementation: (c) 2006,2007,2008 - Rinus Plasmeijer
// *********************************************************************************************************************************
//
import iTasksSettings
import Time
import Html
import TSt

derive gForm 	Task						
derive gUpd 	Task
derive gPrint 	Task
derive gParse 	Task
derive read 	Task
derive write 	Task


instance == GarbageCollect

// Here follow some commonly used internal functions

/* Support for user defined combinators
mkTask 				:: to promote a function of proper type to a task
mkParSubTask 		:: create a subtask with indicated task nr

iTaskId 			:: generate an id based on the task nr, important for garbage collection and family relation

*/


mkTask 				:: !String !(Task a) 		-> Task a 		| iCreateAndPrint a
mkParSubTask 		:: !String !Int (Task a) 	-> (Task a)  	| iCreateAndPrint a					// two shifts are needed

iTaskId 			:: !Int !TaskNr !String 	-> String

// general iTask store, session store, page store, store but no form generation

cFormId 			:: !Options !String !a -> FormId a
sessionFormId 		:: !Options !String !a -> FormId a
pageFormId 			:: !Options !String !a -> FormId a
storageFormId 		:: !Options !String !a -> FormId a


