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

derive gForm 	TCl						
derive gUpd 	TCl
derive gPrint 	TCl
derive gParse 	TCl
derive read 	TCl
derive write 	TCl

:: Task a		:== *TSt -> *(!a,!*TSt)										// an iTask is state transition function
:: TCl a 		= 	TCl !.(Task a)												// task closure, container for a task used for higher order tasks (task which deliver a task)			

:: *TSt 		=	{ tasknr 		:: !TaskNr									// for generating unique form-id's
					, activated		:: !Bool   									// if true activate task, if set as result task completed	
					, userId		:: !Int										// id of user to which task is assigned
					, workflowLink	:: !WorkflowLink							// process table entry information
					, staticInfo	:: !StaticInfo								// info which does not change during a run
					, html			:: !HtmlTree								// accumulator for html code
					, options		:: !Options									// iData lifespan and storage format
					, trace			:: !Maybe [Trace]							// for displaying task trace
					, hst			:: !HSt										// iData state
					}
:: TaskNr			:== [Int]													// task nr i.j is adminstrated as [j,i]
:: WorkflowLink		:== (Entry,ProcessIds)										// entry in table together with unique id which is used for checking whether the reference is still valid
:: Entry			:== Int
:: ProcessIds		:== (!UserId,!ProcessNr,!WorkflowLabel)						// user id, process id and name given to a workflow process; is used as unique identifier in process table
:: UserId			:== Int														// a user id of an iTask user must be a unique integer value
:: ProcessNr		:== Int
:: WorkflowLabel	:== String
:: StaticInfo	=	{ currentUserId	:: UserId									// id of application user 
					, threadTableLoc:: !Lifespan								// where to store the server thread table, default is Session
					}
:: Options		=	{ tasklife		:: !Lifespan								// default: Session		
					, taskstorage	:: !StorageFormat							// default: PlainString
					, taskmode		:: !Mode									// default: Edit
					, gc			:: !GarbageCollect							// default: Collect
					}
:: GarbageCollect 	
				=	Collect 													// garbage collect iTask administration
				|	NoCollect													// no garbage collection
:: HtmlTree		=	BT [HtmlTag] [InputId]										// simple code with possible event handler definitions
				|	(@@:) infix  0 !TaskDescription !HtmlTree					// code with id of user attached to it
				|	(-@:) infix  0 !UserId 	 !HtmlTree							// skip code with this id if it is the id of the user 
				|	(+-+) infixl 1 !HtmlTree !HtmlTree							// code to be placed next to each other				
				|	(+|+) infixl 1 !HtmlTree !HtmlTree							// code to be placed below each other				
				|	DivCode !String !HtmlTree									// code that should be labeled with a div, used for Ajax and Client technology
:: Trace		=	Trace !TraceInfo ![Trace]									// traceinfo with possibly subprocess
:: TraceInfo	:== Maybe (!Bool,!(!UserId,!TaskNr,!Options,!String,!String))	// Task finished? who did it, task nr, task name (for tracing) value produced
:: TaskDescription
				=	{ delegatorId	:: !UserId									// id of the work delegator
					, taskWorkerId	:: !UserId									// id of worker on the task
					, taskNrId		:: !TaskNrId								// tasknr as string
					, processNr		:: !ProcessNr								// entry in process table
					, worflowLabel	:: !WorkflowLabel							// name of the workflow
					, taskLabel		:: !String									// name of the task
					, timeCreated	:: !Time
					, taskPriority	:: !TaskPriority
					}
					
:: TaskNrId		:== String
:: TaskPriority	=	HighPriority | NormalPriority | LowPriority

instance == GarbageCollect

// Here follow some commonly used internal functions

/* Support for user defined combinators
incNr 				:: increment task number
mkTask 				:: to promote a function of proper type to a task
mkParSubTask 		:: create a subtask with indicated task nr

iTaskId 			:: generate an id based on the task nr, important for garbage collection and family relation
showTaskNr 			:: for identifier generation
deleteAllSubTasks 	:: collects all related tasks

printTrace2 		:: show task tree trace
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


