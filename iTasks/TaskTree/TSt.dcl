definition module TSt
/**
* This module defines the core task state data structure
* which is transformed by tasks
*/
import StdMaybe
import Time, Html
import HSt

// The task state
:: *TSt 		=	{ tasknr 		:: !TaskNr									// for generating unique form-id's
					, activated		:: !Bool   									// if true activate task, if set as result task completed	
					, userId		:: !UserId									// id of user to which task is assigned
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

// Trace information
:: Trace		=	Trace !TraceInfo ![Trace]									// traceinfo with possibly subprocess
:: TraceInfo	:== Maybe (!Bool,!(!UserId,!TaskNr,!Options,!String,!String))	// Task finished? who did it, task nr, task name (for tracing) value produced


// Task meta information
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


//Tasks are packed TSt transition functions
:: Task a = Task !(*TSt -> *(!a,!*TSt))

//Initialization
mkTst :: !UserId !Lifespan !Lifespan !*HSt -> *TSt

//Apply a task state transition
appTaskTSt :: !(Task a) !*TSt -> (!a,!*TSt)



