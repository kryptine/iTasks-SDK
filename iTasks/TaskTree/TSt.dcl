definition module TSt
/**
* This module defines the core task state data structure which is transformed by tasks.
* 
* Additionally it provides utility functions to manipulate the state.
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

:: TraceInfo 	=	{ trTaskNr		:: !String									// tasknr 
					, trTaskName	:: !String									// name of the combinator
					, trActivated	:: !Bool									// is the task finshed or not
					, trUserId		:: !UserId									// who is performing the task (can also be determined from the contect)
					, trValue		:: !String									// what is the current value of task (serialized to string)
					, trOptions		:: !Options									// options of this task
					}

// Task meta information
:: CondAndDescription
				=	{ caTaskNrId	:: !String									// tasknr as string
					, caIndex		:: !Int										// index of and task
					, caStatus		:: !Bool									// is sub task finished
					}

:: TaskDescription
				=	{ delegatorId	:: !UserId									// id of the work delegator
					, taskWorkerId	:: !UserId									// id of worker on the task
					, taskNrId		:: !String									// tasknr as string
					, processNr		:: !ProcessNr								// entry in process table
					, workflowLabel	:: !WorkflowLabel							// name of the workflow
					, taskLabel		:: !String									// name of the task
					, timeCreated	:: !Time
					, taskPriority	:: !TaskPriority
					, curStatus		:: !Bool
					}
					
:: TaskPriority	=	HighPriority | NormalPriority | LowPriority

// The task monad
:: Task a = Task !(*TSt -> *(!a,!*TSt))

// a task with a label used for labeling buttons, pull down menu, and the like
:: LabeledTask a	:== (!String,!Task a)		


/**
* Creates an initial task state.
*
* @param The user id of the current user
* @param The default storage location of task states
* @param The default storage location of threads
* @param The iData HSt state for creating editors and doing IO
*
* @return a TSt iTask state
*/
mkTst :: !UserId !Lifespan !Lifespan !*HSt -> *TSt

/**
* Applies a task to the task state.
*
* @param The task that is applied
* @param The task state
*
* @return The value produced by the task
* @return The modified task state
*/
appTaskTSt 			:: !(Task a) !*TSt			-> (!a,!*TSt)

/**
* Deletes iData states for all subtasks of the given task number.
* This function can be used for ad-hoc garbage collection.
* 
* @param The task number of the task which subtasks must be deleted
* @param The task state
*
* @return The task state
*/
deleteAllSubTasks 	:: ![TaskNr] TSt 			-> TSt


/**
* Utility function to increment the last segment a task number
*
* @param The original task number
*
* @return The incremented task number
*/ 
incTaskNr 			:: !TaskNr 					-> TaskNr

/**
* Converts a task number to its dotted string representation
*
* @param The task number as integer list
*
* @return The formatted task number
*/
taskNrToString		:: !TaskNr 					-> String

/**
* Parses a formatted task number to its integer list representation
*
* @param The task nr as formatted string
*
* @return The task nr as integer list
*/
taskNrFromString 	:: !String 					-> TaskNr