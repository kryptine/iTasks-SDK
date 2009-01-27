definition module InternaliTasksCommon

// *********************************************************************************************************************************
// internally used types and utility function
// *********************************************************************************************************************************
// iTask & iData Concept and Implementation: (c) 2006,2007,2008 - Rinus Plasmeijer
// *********************************************************************************************************************************
//
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
iTaskId 			:: generate an id based on the task nr, important for garbage collection and family relation
*/

iTaskId 			:: !Int !TaskNr !String 	-> String

// general iTask store, session store, page store, store but no form generation

cFormId 			:: !Options !String !a -> FormId a
sessionFormId 		:: !Options !String !a -> FormId a
pageFormId 			:: !Options !String !a -> FormId a
storageFormId 		:: !Options !String !a -> FormId a


