definition module TaskTree
/**
* This module contains data types and utility functions for
* creating and manipulating task trees. The actual construction of
* task tree data structures is performed by the basic tasks and
* task combinators.
*/
import StdMaybe, Either
import Types
import Html, Time


from   ProcessDB	import :: ProcessStatus
from   JSON 		import :: JSON
from   ExtJS		import :: ExtJSDef, :: ExtJSUpdate

:: TaskTree			= TTMainTask		TaskInfo TaskProperties		[TaskTree]				//A task that is treated as a main chunk of work
					| TTExtJSTask		TaskInfo (Either ExtJSDef [ExtJSUpdate])			//A task that can be worked on through an ExtJS gui 
					| TTMonitorTask		TaskInfo [HtmlTag]									//A task that upon evaluation monitors a condition and may give status output
					| TTSequenceTask	TaskInfo 					[TaskTree]				//A task that is composed of a number of sequentially executed subtasks
					| TTParallelTask	TaskInfo TaskCombination	[TaskTree]				//A task that is composed of a number of parallel executed subtasks  
					| TTFinishedTask	TaskInfo											//A completed task
							
:: TaskInfo	=		{ taskId		:: TaskId												//Task number in string format
					, taskLabel		:: String												//Descriptive label of the task
					, active		:: Bool													//Is the task active?
					, finished		:: Bool													//Is the task finished?
					, traceValue	:: String												//String representation of value for tracing
					}
					
:: TaskProperties =	{ processId		:: ProcessId											//READ ONLY: Identifier of the task in the process database
					, subject		:: String												//READ ONLY: A very short description of the task

					//Delegator properties
					, user			:: (UserId,String)										//Who has to do the task?
					, delegator		:: (UserId,String)										//Who has issued the task?
					, priority		:: TaskPriority											//What is the current priority of this task?
					, deadline		:: Maybe Timestamp										//When is the task due?
					
					//Worker properties
					, progress		:: TaskProgress											//
					//System properties
					, issuedAt		:: Timestamp											//READ ONLY: When was the task created
					, firstEvent	:: Maybe Timestamp										//READ ONLY: When was the first work done on this task
					, latestEvent	:: Maybe Timestamp										//READ ONLY: When was the last event on this task				
					}
					
:: TaskProgress		= TPActive																//Worker is happily working on the task
					| TPStuck																//Worker is stuck and needs assistence
					| TPWaiting																//Worker is waiting, not actively working on the task
					| TPReject																//Worker does not want to continue working on the task

/**
* Finds the sub tree with the given task number
*/
locateSubTaskTree	:: !TaskId !TaskTree -> Maybe TaskTree
