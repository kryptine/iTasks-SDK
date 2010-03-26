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

from   ProcessDB		import :: ProcessStatus, :: Action, :: Menu, :: MenuItem
from   JSON 			import :: JSON
from   TUIDefinition	import :: TUIDef, :: TUIUpdate

// give definition/updates or determine it after entire tree is build, needed for updateShared, ...
:: InteractiveTask	= Definition (TUIDef,[TUIButton]) [(Action,Bool)]
					| Updates [TUIUpdate] [(Action,Bool)]
					| Func (*TSt -> *(!InteractiveTask, !*TSt))

:: TaskTree			= TTMainTask		TaskInfo TaskProperties (Maybe [Menu]) !(Maybe TaskParallelType) TaskTree	//A task that is treated as a main chunk of work
					| TTInteractiveTask	TaskInfo InteractiveTask													//A task that can be worked on through a gui 
					| TTMonitorTask		TaskInfo [HtmlTag]															//A task that upon evaluation monitors a condition and may give status output
					| TTRpcTask			TaskInfo RPCExecute															//A task that represents an rpc invocation
					| TTSequenceTask	TaskInfo [TaskTree]															//A task that is composed of a number of sequentially executed subtasks
					| TTParallelTask	TaskInfo TaskParallelInfo [TaskTree]										//A task that is composed of a number of parallel executed subprocesses  
					| TTGroupedTask		TaskInfo [TaskTree]															//A task that is composed of a number of grouped subtasks
					| TTFinishedTask	TaskInfo [HtmlTag]															//A completed task
							
:: TaskInfo	=		{ taskId			:: TaskId											//Task number in string format
					, taskLabel			:: String											//Descriptive label of the task
					, traceValue		:: String											//String representation of value for tracing
					, worker			:: UserName				
					, groupedBehaviour	:: GroupedBehaviour
					, taskDescription	:: String
					}

:: TaskProperties = { systemProps	:: TaskSystemProperties
					, managerProps	:: TaskManagerProperties
					, workerProps	:: TaskWorkerProperties
					}

:: TaskSystemProperties =
	{ processId			:: ProcessId		// Process table identification
	, manager			:: UserName			// Who is managing this task
	, issuedAt			:: Timestamp		// When was the task created
	, firstEvent		:: Maybe Timestamp	// When was the first work done on this task
	, latestEvent		:: Maybe Timestamp	// When was the latest event on this task	
	, latestExtEvent	:: Maybe Timestamp  // When was the latest event from an external source (e.g. Rpc Daemon)
	}

:: TaskManagerProperties =
	{ worker			:: UserName					// Who has to do the task? 
	, subject			:: String 					// The subject of the task
	, priority			:: TaskPriority				// What is the current priority of this task?
	, deadline			:: Maybe Timestamp			// When is the task due?
	, tempWorkers		:: [(ProcessId, UserName)] 	// Users who have temporary access to the process. (In case of an open parallel)
	}
					
:: TaskWorkerProperties =
	{ progress		:: TaskProgress		// Indication of the worker's progress
	}

:: TaskProgress		= TPActive			//Worker is happily working on the task
					| TPStuck			//Worker is stuck and needs assistence
					| TPWaiting			//Worker is waiting, not actively working on the task
					| TPReject			//Worker does not want to continue working on the task
	
:: TaskParallelInfo =
	{ type			:: TaskParallelType //Indicating the scope of the parallel. 
	, description	:: String			//Description of the behavior of this parallel. This is also displayed in the overview panel in the interface
	}

:: TaskParallelType = Open 				//Everybody to whom a subtask is assigned can see the full status of this parallel, including the results of others
					| Closed			//Only the manager can see the overview. For assigned users, it just looks like an ordinary task.
	
:: GroupedBehaviour = Fixed 			//The editor is fixed in the window
					| Floating			//The editor can be undocked and made floating