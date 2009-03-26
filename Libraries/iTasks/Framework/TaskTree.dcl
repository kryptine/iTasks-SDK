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
from   iDataForms	import :: InputDefinition {..}, ::UpdateEvent, :: Mode, :: StorageFormat, :: Lifespan
from   ProcessDB	import :: ProcessStatus

// New experimental task tree data strucure
:: TaskTree			= TTBasicTask		TaskInfo [HtmlTag] [InputDefinition]			//Smallest unit of work that has to be performed by a user
					| TTSequenceTask	TaskInfo [TaskTree]								//A task that is composed of a number of sequentially executed subtasks
					| TTParallelTask	TaskInfo TaskCombination [TaskTree]				//A task that is composed of a number of parallel executed subtasks
					| TTProcess			ProcessInfo [TaskTree]							//The top node of a task tree is a process 

				
:: TaskInfo	=		{ taskId		:: TaskId											//Task number in string format
					, taskLabel		:: String											//Descriptive label of the task
					, userId		:: UserId											//User that has to do this task
					, delegatorId	:: UserId											//User that issued this task
					, active		:: Bool												//Is the task active?
					, finished		:: Bool												//Is the task finished?
					, priority		:: TaskPriority										//How important is the task
					, traceValue	:: String											//String representation of value for tracing
					}

:: ProcessInfo =	{ processId		:: ProcessId
					, processLabel	:: String
					, userId		:: UserId
					, delegatorId	:: UserId
					, status		:: ProcessStatus
					}

/**
* Finds the sub tree with the given task number
*/
locateSubTaskTree	:: !TaskId !TaskTree -> Maybe TaskTree
