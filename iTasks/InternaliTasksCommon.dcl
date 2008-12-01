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

derive gForm 	TCl						
derive gUpd 	TCl
derive gPrint 	TCl
derive gParse 	TCl
derive read 	TCl
derive write 	TCl


:: TCl a 		= 	TCl !.(Task a)												// task closure, container for a task used for higher order tasks (task which deliver a task)			


instance == GarbageCollect

// Here follow some commonly used internal functions

/* Support for user defined combinators
incNr 				:: increment task number
mkTask 				:: to promote a function of proper type to a task
mkParSubTask 		:: create a subtask with indicated task nr

iTaskId 			:: generate an id based on the task nr, important for garbage collection and family relation
showTaskNr 			:: for identifier generation
deleteAllSubTasks 	:: collects all related tasks
*/

incNr 				:: !TaskNr 					-> TaskNr
mkTask 				:: !String !(Task a) 		-> Task a 		| iCreateAndPrint a
mkParSubTask 		:: !String !Int (Task a) 	-> (Task a)  	| iCreateAndPrint a					// two shifts are needed

iTaskId 			:: !Int !TaskNr !String 	-> String
showTaskNr 			:: !TaskNr 					-> String
deleteAllSubTasks 	:: ![TaskNr] TSt 			-> TSt

// general iTask store, session store, page store, store but no form generation

cFormId 			:: !Options !String !a -> FormId a
sessionFormId 		:: !Options !String !a -> FormId a
pageFormId 			:: !Options !String !a -> FormId a
storageFormId 		:: !Options !String !a -> FormId a


