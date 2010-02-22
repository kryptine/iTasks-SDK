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
import RPC

from   ProcessDB		import :: ProcessStatus, :: Action
from   JSON 			import :: JSON
from   TUIDefinition	import :: TUIDef, :: TUIUpdate

:: TaskTree			= TTMainTask		TaskInfo TaskProperties [TaskTree]		//A task that is treated as a main chunk of work
					| TTInteractiveTask	TaskInfo (Either TUIDef [TUIUpdate]) [(Action,Bool)]	//A task that can be worked on through a gui 
					| TTMonitorTask		TaskInfo [HtmlTag]						//A task that upon evaluation monitors a condition and may give status output
					| TTRpcTask			TaskInfo RPCExecute						//A task that represents an rpc invocation
					| TTSequenceTask	TaskInfo [TaskTree]						//A task that is composed of a number of sequentially executed subtasks
					| TTParallelTask	TaskInfo [TaskTree]						//A task that is composed of a number of parallel executed subtasks  
					| TTFinishedTask	TaskInfo								//A completed task
							
:: TaskInfo	=		{ taskId		:: TaskId									//Task number in string format
					, taskLabel		:: String									//Descriptive label of the task
					, traceValue	:: String									//String representation of value for tracing
					}

:: TaskProperties = { systemProps	:: TaskSystemProperties
					, managerProps	:: TaskManagerProperties
					, workerProps	:: TaskWorkerProperties
					}

:: TaskSystemProperties =
	{ processId			:: ProcessId			// Process table identification
	, manager			:: (UserName,String)	// Who is managing this task
	
	, issuedAt			:: Timestamp		// When was the task created
	, firstEvent		:: Maybe Timestamp	// When was the first work done on this task
	, latestEvent		:: Maybe Timestamp	// When was the latest event on this task	
	, latestExtEvent	:: Maybe Timestamp  // When was the latest event from an external source (e.g. Rpc Daemon)
	}

:: TaskManagerProperties =
	{ worker		:: (UserName, String)	// Who has to do the task? 
	, subject		:: String 				// The subject of the task
	, priority		:: TaskPriority			// What is the current priority of this task?
	, deadline		:: Maybe Timestamp		// When is the task due?
	}
					
:: TaskWorkerProperties =
	{ progress		:: TaskProgress		// Indication of the worker's progress
	}

:: TaskProgress		= TPActive			//Worker is happily working on the task
					| TPStuck			//Worker is stuck and needs assistence
					| TPWaiting			//Worker is waiting, not actively working on the task
					| TPReject			//Worker does not want to continue working on the task
