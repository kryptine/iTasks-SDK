definition module TSt
/**
* This module defines the core task state data structure which is transformed by tasks.
* 
* Additionally it provides utility functions to manipulate the state.
*/
import Maybe, Void, TaskTree, Task
from ProcessDB	import :: Process
from Config		import :: Config(..)
from Store		import :: Store(..)
from Time		import :: Timestamp(..)
from HTTP		import :: HTTPRequest

// The task state
:: *TSt 		=	{ taskNr 			:: !TaskNr											// for generating unique form-id's
					, taskInfo			:: !TaskInfo										// task information available to tasks
					, tree				:: !NonNormalizedTree								// accumulator for constructing a task tree
					, newTask			:: !Bool											// does the task run for the first time
					
					, events			:: ![TaskEvent]										// The update events for interactive tasks
																							// (task id, name, value)
																						
					, properties		:: !ProcessProperties								// Properties of the current evaluated process
					
					, staticInfo		:: !StaticInfo										// info which does not change during a run
										
					, currentChange		:: !Maybe (!ChangeLifeTime,!ChangeDyn)				// An active change
					, pendingChanges	:: ![(!ChangeLifeTime,!ChangeDyn)]					// Pending persistent changes
					
					, request			:: !HTTPRequest										// The current http request
					
					, iworld			:: !*IWorld											// The 'iTasks' world
					
					, sharedChanged		:: !Bool											// Is set to true if a Shared is changed
					, sharedDeleted		:: !Bool											// Is set to true if a Shared is deleted
					, triggerPresent	:: !Bool											// Is set to true if an editor with an auto event is present
					, iterationCount	:: !Int												// Number of iterations in the commit phase
					}
					

:: StaticInfo	=	{ currentProcessId	:: !ProcessId									// the id of the current process
					, currentSession	:: !Session										// the current session			
					, staticWorkflows	:: ![Workflow]									// the list of workflows supported by the application				
					}

// A workflow specification
:: Workflow		=	{ path				:: !String										// a unique name of this workflow
					, roles				:: ![String]									// the roles that are allowed to initate this workflow
					, thread			:: !Dynamic										// the thread of the main task of the workflow
					, description		:: !String										// a description of the workflow
					, managerProperties	:: !ManagerProperties							// the initial manager properties of the main task
					, menu				:: !ActionMenu									// the menu of the main task
					}
					
/**
* Creates an initial task state.
*
* @param The application name
* @param The server configuration
* @param The current HTTP request
* @param The workflows available in the application
* @param The generic data store
* @param The path for temporary files
* @param The world
*
* @return a TSt iTask state
*/
mkTSt :: !String !Config! HTTPRequest ![Workflow] !*Store !FilePath !*World -> *TSt

/**
* Initializes the session information.
*
* @param A session identifier
* @param The task state
*
* @return Nothing if the session is initialized correctly, error message otherwise
* @return The modified task state
*/
initSession :: !SessionId !*TSt -> (!Maybe String, !*TSt)

/**
* Creates a dynamic containing a runnable task thread structure.
* It contains the task plus the iTask context restrictions.
*
* @param The workflow title (for param only)
* @param The task that is to be converted to a runnable thread (with ot without parameter)
* 
* @return A dynamic containing the thread
*/
createThread		:: !(Task a)				-> Dynamic	| iTask a
createThreadParam	:: !String !(a -> Task b)	-> Dynamic	| iTask a & iTask b

/**
* Converts a parameterised thread into a non-parameterised one either by using a json encoded value or letting the user enter it.
*/
toNonParamThreadValue	:: !String !Dynamic	-> Maybe Dynamic
toNonParamThreadEnter	:: !Dynamic			-> Dynamic

/**
* Creates an instance of a task definition
* As soon as an instance is created it is immediately evaluated once.
*
* @param A task thread of the task to create an instance of
* @param Start as toplevel, or as subtask of another task (parent information is read from the task state)
* @param Delete the result and process record of the instance after completion
* @param The initial manager properties
* @param The main task's menu
* @param The task state
*
* @return The process id of the new instance
* @return The result of the first run (as dynamic result)
* @return The task tree created at the first run
* @return The modified task state
*/
createTaskInstance :: !Dynamic !Bool !Bool !ManagerProperties !ActionMenu !*TSt -> (!ProcessId, !TaskResult Dynamic, !NonNormalizedTree, !*TSt)

/**
* Removes a running task instance from the list of processes and clears any associated data in the store
*
* @param The process id of the instance
* @param The task state
*
* @return The modified task state
*/
deleteTaskInstance :: !ProcessId !*TSt -> *TSt

/**
* Removes a (finished) process only if it is marked to be deleted when done
*
* @param The process id of the instance
* @param The task state
*
* @return Whether the process has been garbage collected
* @return The modified task state
*/
garbageCollectTaskInstance :: !ProcessId !*TSt -> (!Bool,!*TSt)

/**
* Evaluates an existing task instance
*
* @param Process information from the process database
* @param The type of task tree to build
* @param The value updates to apply
* @param Optionally a new Change that is to be applied to this task instance
* @param Is the instance evaluated as top node, or as subnode while evaluating a parent process
* @param Is the task evaluated for the first time
* @param The task state
*
* @return The modified task state
*/
evaluateTaskInstance :: !Process ![TaskEvent] !(Maybe ChangeInjection) !Bool !Bool !*TSt-> (!TaskResult Dynamic, !NonNormalizedTree, !*TSt)
/**
* Applies a change to a running task process task state.
* 
* @param The process id
* @param The lifetime and change tuple
* @param The task state
*
* @return The modified task state
*/
applyChangeToTaskTree :: !ProcessId !ChangeInjection !*TSt -> *TSt
/**
* Calculates a single task tree for a given process id
*
* @param The task id of the process
* @param The type of task tree to build
* @param The value updates to apply
* @param The task state
*
* @return Just an HtmlTree when the process is found, Nothing on failure
* @return The modified task state
*/
calculateTaskTreeContainer :: !TaskId ![TaskEvent] !*TSt -> (!NonNormalizedTreeContainer, !*TSt)
/**
* Lists which workflows are available
*
* @param The task state
*
* @return The list of workflows
* @return The modified task state
*/
getWorkflows :: !*TSt -> (![Workflow],!*TSt)
/**
* Looks up a specific workflow by name
*
* @param The unique workflow name
* @param The task state
*
* @return Maybe the workflow definition
* @return The modified task state
*/
getWorkflowByName :: !String !*TSt -> (!Maybe Workflow, !*TSt)

/**
* Apply a function on IWorld on a TSt
*/ 
appIWorldTSt :: !.(*IWorld -> *IWorld) !*TSt -> *TSt
/**
* Apply a function yielding a result on IWorld on a TSt
*/
accIWorldTSt :: !.(*IWorld -> *(.a,*IWorld))!*TSt -> (.a,!*TSt)
/**
* Apply a function on World on a TSt
*/ 
appWorldTSt	:: !.(*World -> *World) !*TSt -> *TSt
/**
* Apply a function yielding a result on World on a TSt
*/
accWorldTSt	:: !.(*World -> *(.a,*World))!*TSt -> (.a,!*TSt)
/**
* Get the current session from the TSt
*/
getCurrentSession :: !*TSt 	-> (!Session, !*TSt)
/**
* Get the id of the current process in the TSt
*/
getCurrentProcess :: !*TSt -> (!ProcessId, !*TSt)
/**
* Extract the calculated task forest data structure from the TSt
*/
getTaskTree :: !*TSt	-> (!NonNormalizedTree, !*TSt)

/**
* Get a value from the configuration data
*/
getConfigSetting :: !(Config -> a) !*TSt -> (!a,!*TSt)
//// TASK CREATION

// functions used for the definition of tasks
// there are functions for two separate passes (edit & commit events pass)
:: TaskFunctionEdit		:== *TSt -> *TSt
:: TaskFunctionCommit a	:== *TSt -> *(!TaskResult a,!*TSt)
:: TaskFunctions a		:== (!TaskFunctionEdit,!TaskFunctionCommit a)

/**
* Maps task functions to a different type.
*/
mapTaskFunctions :: !(a -> b) !(TaskFunctions a) -> TaskFunctions b

/**
* Lift a function that uses the TSt to a function that returns
* a TaskResult value of TaskFinished with the value returned by the function.
*
* @param The function on the TSt
*
* @return The task function
*/
mkTaskFunction :: (*TSt -> (!a,!*TSt)) -> TaskFunctionCommit a
/**
* Wrap a function of proper type to create a function that also
* keeps track of the the internal numbering and administration.
* The given task function will add a single basic step to the current
* sequence.
*
* @param A description of the task
* @param The type of the interactive task
* @param The function on the TSt that is the task
*
* @return The newly constructed basic task
*/
mkInteractiveTask	:: !d !InteractiveTaskType !(TaskFunctions a) -> Task a | descr d
/**
* Wrap a function of proper type to create a function that also
* keeps track of the the internal numbering and administration.
* The given task function will add a single instantly computed step to
* the current sequence.
*
* @param A description of the task
* @param The function on the TSt that is the task
*
* @return The newly constructed basic task
*/
mkInstantTask		:: !d !(TaskFunctionCommit a) -> Task a | descr d
/**
* Wraps a function of proper type to create a task that will consist
* of a sequence of subtasks. The given task function will execute in a blank sequence
* and the resulting sequence will be combined in a single sequence node.
*
* @param A description of the task
* @param The function on the TSt that is the task (edit pass)
* @param The function on the TSt that is the task (commit pass)
*
* @return The newly constructed sequence task
*/
mkSequenceTask		:: !d !(TaskFunctions a) -> Task a | descr d
/**
* Wrap a function of proper type to create a function that also
* keeps track of the the internal numbering and administration for
* combining a set of parallel subtasks. Each of the subtasks is wrapped in a 
* separate process. 
*
* @param A description of the task
* @param Some info about the behavior of the parallel task
* @param The function on the TSt that is the task
*
* @return The newly constructed parallel task
*/
mkParallelTask		:: !d !(TaskFunctions a) -> Task a | descr d

//// TASK APPLICATION

/**
* Applies a task to the task state and process all edit events (update local/shared values...)
*
* @param The task that is applied
* @param The task state
*
* @return The modified task state
*/
applyTaskEdit			:: !(Task a) !*TSt -> (!TaskResult a,!*TSt) | iTask a

/**
* Applies a task to the task state and yields the tasks result.
*
* @param The task that is applied
* @param The task state
*
* @return The value produced by the task
* @return The modified task state
*/
applyTaskCommit			:: !(Task a) !(Maybe (!Int,!TaskContainerType)) !*TSt -> (!TaskResult a,!*TSt) | iTask a

//// TASK CONTENT
setInteractiveFuncs	:: !TTNNInteractiveTask !*TSt						-> *TSt // Only for interactive tasks

//EVENTS
//Get edit events for current task of which the name is a datapath
getEditEvents :: !*TSt -> (![(!DataPath,!String)],!*TSt)

//Get value event for current task if present
getValueEvent :: !*TSt -> (!Maybe a,!*TSt) | JSONDecode{|*|} a

//Get action event for current task if present
getActionEvent :: !*TSt -> (!Maybe JSONNode,!*TSt)

/**
* Writes a 'task scoped' value to the store
* These values are copied and garbage collected along with a task
*/
setTaskStore			:: !String !a !*TSt				-> *TSt		| JSONEncode{|*|}, JSONDecode{|*|}, TC a
setTaskStoreFor 		:: !TaskNr !String !a !*IWorld	-> *IWorld	| JSONEncode{|*|}, JSONDecode{|*|}, TC a
/**
* Reads a 'task scoped' value from the store
*/
getTaskStore			:: !String !*TSt				-> (Maybe a, !*TSt)			| JSONEncode{|*|}, JSONDecode{|*|}, TC a
getTaskStoreFor			:: !TaskNr !String !*IWorld		-> (Maybe a, !*IWorld) 		| JSONEncode{|*|}, JSONDecode{|*|}, TC a
/**
* Gets timestamp of 'task scoped' values
*/
getTaskStoreTimestamp		:: !String !*TSt			-> (Maybe Timestamp, !*TSt)
getTaskStoreTimestampFor	:: !TaskNr !String !*IWorld	-> (Maybe Timestamp, !*IWorld)
/**
* Delete 'task scoped' values
*/
deleteTaskStore			:: !String !*TSt			-> *TSt
deleteTaskStoreFor		:: !TaskNr !String !*IWorld	-> *IWorld
/**
* Store and load the result of a workflow instance
*/
loadProcessResult		:: !TaskNr 							!*IWorld -> (!Maybe (TaskResult Dynamic), !*IWorld)
storeProcessResult		:: !TaskNr !(TaskResult Dynamic)	!*IWorld -> *IWorld

/**
* Resets a sequence
*/
resetSequence		::	!*TSt					-> *TSt
/**
* Delete task state (for garbage collection) for a task and its subtasks
* 
* @param Task nr to delete states for
* @param The task state
*
* @return The updated task state
*/
deleteTaskStates	:: !TaskNr !*TSt			-> *TSt
/**
* Copy task state for a task and its subtasks.
*
* @param From task nr
* @param To task nr
* @param The task state
*
* @return The updated task state
*/
copyTaskStates		:: !TaskNr !TaskNr !*TSt	-> *TSt
/**
* Writes all cached values in the store
*/
flushStore			:: !*TSt					-> *TSt
