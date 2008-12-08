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
					, trace			:: !Bool									// default: False
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
				|	(+-+) infixl 1 !HtmlTree !HtmlTree							// code to be placed next to each other				
				|	(+|+) infixl 1 !HtmlTree !HtmlTree							// code to be placed below each other				
				|	CondAnd !String !Int [(!CondAndDescription,!HtmlTree)]		// list of subtasks to display in different tabs by worklist handler 
				|	DivCode !String !HtmlTree									// code that should be labeled with a div, used for Ajax and Client technology
				|	TaskTrace TraceInfo !HtmlTree								// trace information used for displaying the task tree

:: TraceInfo 	=	{ trTaskNr		:: !TaskNrId									// tasknr 
					, trTaskName	:: !String									// name of the combinator
					, trActivated	:: !Bool									// is the task finshed or not
					, trUserId		:: !UserId									// who is performing the task (can also be determined from the contect)
					, trValue		:: !String									// what is the current value of task (serialized to string)
					, trOptions		:: !Options									// options of this task
					}

// Task meta information
:: CondAndDescription
				=	{ caTaskNrId	:: !TaskNrId								// tasknr as string
					, caIndex		:: !Int										// index of and task
					, caStatus		:: !Bool									// is sub task finished
					}

:: TaskDescription
				=	{ delegatorId	:: !UserId									// id of the work delegator
					, taskWorkerId	:: !UserId									// id of worker on the task
					, taskNrId		:: !TaskNrId								// tasknr as string
					, processNr		:: !ProcessNr								// entry in process table
					, worflowLabel	:: !WorkflowLabel							// name of the workflow
					, taskLabel		:: !String									// name of the task
					, timeCreated	:: !Time
					, taskPriority	:: !TaskPriority
					, curStatus		:: !Bool
					}
					
:: TaskNrId		:== String
:: TaskPriority	=	HighPriority | NormalPriority | LowPriority


//Tasks are packed TSt transition functions
:: Task a = Task !(*TSt -> *(!a,!*TSt))

//Initialization
mkTst :: !UserId !Lifespan !Lifespan !*HSt -> *TSt

//Apply a task state transition
appTaskTSt :: !(Task a) !*TSt -> (!a,!*TSt)



