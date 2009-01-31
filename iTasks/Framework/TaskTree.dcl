definition module TaskTree
/**
* This module contains data types and utility functions for
* creating and manipulating task trees. The actual construction of
* task tree data structures is performed by the basic tasks and
* task combinators.
*/
import StdMaybe
import Types
import Html, Time
from   iDataForms import :: InputId {..}, ::UpdateEvent, :: Mode, :: StorageFormat, :: Lifespan

// New experimental task tree data strucure
:: TaskTree			= TTBasicTask		TaskInfo [HtmlTag] [InputId]					//Smallest unit of work that has to be performed by a user
					| TTSequenceTask	TaskInfo [TaskTree]								//A task that is composed of a number of sequentially executed subtasks
					| TTParallelTask	TaskInfo TaskCombination [TaskTree]				//A task that is composed of a number of parallel executed subtasks
					| TTProcess			ProcessInfo [TaskTree]							//The top node of a task tree is a process 

				
:: TaskInfo	=		{ taskId		:: TaskId											//Task number in string format
					, taskLabel		:: String											//Descriptive label of the task
					, userId		:: UserId											//User that has to do this task
					, active		:: Bool												//Is the task active?
					, finished		:: Bool												//Is the task finished?
					, priority		:: TaskPriority										//How important is the task
					}

:: TaskCombination	= TTSplit [HtmlTag]													//Treat the tasks as separate units of work
					| TTVertical														//Group the tasks and display them below each other
					| TTHorizontal 														//Group the tasks and display them next to each other
					| TTCustom	([[HtmlTag]] -> [HtmlTag])								//Group the tasks and display them with a custom function

:: ProcessInfo =	{ processId		:: ProcessId
					, processLabel	:: String
					, userId		:: UserId
					, finished		:: Bool
					}

/**
* Finds the sub tree with the given task number
*/
locateSubTaskTree	:: !TaskId !TaskTree -> Maybe TaskTree

// "Old" Task tree data structure
:: HtmlTree		=	BT [HtmlTag] [InputId]												// simple code with possible event handler definitions
				|	(@@:) infix  0 !TaskDescription !HtmlTree							// code with id of user attached to it
				|	(+-+) infixl 1 !HtmlTree !HtmlTree									// code to be placed next to each other				
				|	(+|+) infixl 1 !HtmlTree !HtmlTree									// code to be placed below each other				
				|	CondAnd !String !Int [(!CondAndDescription,!HtmlTree)]				// list of subtasks to display in different tabs by worklist handler 
				|	DivCode !String !HtmlTree											// code that should be labeled with a div, used for Ajax and Client technology
				|	TaskTrace TraceInfo !HtmlTree										// trace information used for displaying the task tree


// Task meta information
:: CondAndDescription
				=	{ caTaskNrId	:: !String									// tasknr as string
					, caTaskLabel	:: !String									// label of subtask
					, caIndex		:: !Int										// index of current task in the set
					, caNumSiblings	:: !Int										// number of siblings in the set
					, caStatus		:: !Bool									// is subtask finished
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

:: TraceInfo 	=	{ trTaskNr		:: !String									// tasknr 
					, trTaskName	:: !String									// name of the combinator
					, trActivated	:: !Bool									// is the task finshed or not
					, trUserId		:: !UserId									// who is performing the task (can also be determined from the contect)
					, trValue		:: !String									// what is the current value of task (serialized to string)
					, trOptions		:: !Options									// options of this task
					}
:: Options		=	{ tasklife		:: !Lifespan								// default: Session		
					, taskstorage	:: !StorageFormat							// default: PlainString
					, taskmode		:: !Mode									// default: Edit
					, gc			:: !GarbageCollect							// default: Collect
					, trace			:: !Bool									// default: False
					}
					
:: WorkflowLabel	:== String