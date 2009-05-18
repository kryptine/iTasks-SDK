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

:: TaskTree			= TTBasicTask		TaskInfo [HtmlTag] [InputDefinition]			//Smallest unit of work that has to be performed by a user
					| TTSequenceTask	TaskInfo 					[TaskTree]			//A task that is composed of a number of sequentially executed subtasks
					| TTParallelTask	TaskInfo TaskCombination	[TaskTree]			//A task that is composed of a number of parallel executed subtasks
					| TTMainTask		TaskInfo TaskProperties		[TaskTree]			//A task that is treated as a main chunk of work  
								
:: TaskInfo	=		{ taskId		:: TaskId											//Task number in string format
					, taskLabel		:: String											//Descriptive label of the task
					, active		:: Bool												//Is the task active?
					, finished		:: Bool												//Is the task finished?
					, traceValue	:: String											//String representation of value for tracing
					}
					
:: TaskProperties =	{ subject		:: String
					, userId		:: UserId
					, delegatorId	:: UserId
					
					, priority		:: TaskPriority										//What is the current priority of this task
					, issuedAt		:: Time												//When was the task created
					, firstEvent	:: Maybe Time										//When was the first work done on this task
					, latestEvent	:: Maybe Time										//When was the last event on this task				
					}
/**
* Finds the sub tree with the given task number
*/
locateSubTaskTree	:: !TaskId !TaskTree -> Maybe TaskTree
