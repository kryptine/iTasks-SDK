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

from   iDataForms	import :: InputDefinition {..}, :: HtmlState {..}, ::UpdateEvent, :: Mode, :: StorageFormat, :: Lifespan
from   ProcessDB	import :: ProcessStatus
from   JSON 		import :: JSON

:: TaskTree			= TTBasicTask		TaskInfo [HtmlTag] [InputDefinition] [HtmlState]	//Smallest unit of work that has to be performed by a user
					| TTSequenceTask	TaskInfo 					[TaskTree]				//A task that is composed of a number of sequentially executed subtasks
					| TTParallelTask	TaskInfo TaskCombination	[TaskTree]				//A task that is composed of a number of parallel executed subtasks
					| TTMainTask		TaskInfo TaskProperties		[TaskTree]				//A task that is treated as a main chunk of work  
								
:: TaskInfo	=		{ taskId		:: TaskId												//Task number in string format
					, taskLabel		:: String												//Descriptive label of the task
					, active		:: Bool													//Is the task active?
					, finished		:: Bool													//Is the task finished?
					, traceValue	:: String												//String representation of value for tracing
					}
					
:: TaskProperties =	{ processId		:: ProcessId											//READ ONLY: Identifier of the task in the process database
					, subject		:: String												//READ ONLY: A very short description of the task

					, user			:: (UserId,String)										//Who has to do the task?
					, delegator		:: (UserId,String)										//Who has issued the task?
					, priority		:: TaskPriority											//What is the current priority of this task?
					
					, issuedAt		:: Time													//READ ONLY: When was the task created
					, firstEvent	:: Maybe Time											//READ ONLY: When was the first work done on this task
					, latestEvent	:: Maybe Time											//READ ONLY: When was the last event on this task				
					}
/**
* Finds the sub tree with the given task number
*/
locateSubTaskTree	:: !TaskId !TaskTree -> Maybe TaskTree
