definition module TSt
/**
* This module defines the core task state data structure which is transformed by tasks.
* 
* Additionally it provides utility functions to manipulate the state.
*/
import StdMaybe
import Types, Void
import TaskTree
import RPC

from SessionDB	import :: Session
from Config		import :: Config(..)
from Store		import :: Store(..)
from Time		import :: Timestamp(..)
from Http		import :: HTTPRequest

from iTasks		import class iTask(..)
import GenPrint, GenParse, GenVisualize, GenUpdate

// The task state
:: *TSt 		=	{ taskNr 		:: !TaskNr											// for generating unique form-id's
					, taskInfo		:: !TaskInfo										// task information available to tasks
					, firstRun		:: !Bool											// Is this task evaluated for the first time
					, curValue		:: !Maybe Dynamic									// Current task value
					, userId		:: !UserId											// id of user to which task is assigned
					, delegatorId	:: !UserId											// id of user who issued the task
					, tree			:: !TaskTree										// accumulator for constructing a task tree			
					, activated		:: !Bool   											// if true activate task, if set as result task completed

					, mainTask		:: !ProcessId										// The id of the current main task 
							
					, options		:: !Options											// options
					, staticInfo	:: !StaticInfo										// info which does not change during a run
					
					, exception		:: !Maybe Dynamic									// Optional, used when raising exceptions
					
					, doChange		:: !Bool											// Apply change
					, changes		:: ![Maybe (!ChangeLifeTime, !DynamicId, !Dynamic)]	// Active changes
					
					, config		:: !Config											// The server configuration
					, request		:: !HTTPRequest										// The current http request
									
					, systemStore	:: !Store											// UserDB
					, dataStore		:: !Store											// Runtime data (Processes, Sessions, Tasks, Dynamics)
					, world			:: !*World											// The world
					}

:: Options		=	{ trace			:: !Bool									// default: False
					, combination	:: !Maybe TaskCombination					// default: TTVertical
					}

:: StaticInfo	=	{ appName			:: String								// the name of the server executable
					, currentProcessId	:: !ProcessId							// the id of the current process
					, currentSession	:: Session								// the current session			
					, staticWorkflows	:: ![Workflow]							// the list of workflows supported by the application				
					}

// The task monad
:: Task a = Task !String !(Maybe TaskNr) !(*TSt -> *(!a,!*TSt))

// A workflow specification
:: Workflow		=	{ name			:: !String									// a unique name of this workflow
					, label			:: !String									// a label that will be used for displaying
					, roles			:: ![String]								// the roles that are allowed to initate this workflow
					, mainTask		:: Task Void								// the main task of the workflow
					}

// A change function which may be used to change tasks at runtime
:: Change a :== (TaskProperties (Task a) (Task a) -> (Maybe TaskProperties, Maybe (Task a), Maybe Dynamic))

// Changes may be applied only once, or persist for future changes
:: ChangeLifeTime	= CLTransient
					| CLPersistent !String

/**
* Creates an initial task state.
*
* @param The application name
* @param The server configuration
* @param The current HTTP request
* @param The session data
* @param The workflows available in the application
* @param The generic data store
* @param The world
*
* @return a TSt iTask state
*/
mkTSt :: String Config HTTPRequest Session ![Workflow] !*Store !*Store !*World -> *TSt

/**
* Creates an instance of a task definition
*
* @param The task
* @param Start as toplevel, or as subtask of another task (parent information is read from the task state)
* @param The task state
*
* @param The process id of the instance
* @param The modified task state
*/
createTaskInstance :: !(Task a) !TaskManagerProperties !Bool !*TSt -> (!ProcessId, !*TSt) | iTask a

/**
* Calculates a single task tree for a given process id
*
* @param The process id
* @param The task state
*
* @return Just an HtmlTree when the process is found, Nothing on failure
* @return The modified task state
*/
calculateTaskTree :: !ProcessId !*TSt -> (!TaskTree, !*TSt)

/**
* Calculates all task trees
*
* @param The task state
*
* @return The list of task trees (task forest)
* @return The modified task state
*/
calculateTaskForest :: !*TSt -> (![TaskTree], !*TSt)


/**
* Applies a change to a running task process task state.
* 
* @param The process id
* @param The change in a dynamic
* @param The change lifetime
* @param The task state
*
* @return The modified task state
*/
applyChangeToTaskTree :: !ProcessId !Dynamic !ChangeLifeTime !*TSt -> *TSt

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
* Extract the user id of the current session in the TSt
*/
getCurrentUser	:: !*TSt 	-> (!UserId, !*TSt)
/**
* Get the id of the current process in the TSt
*/

getCurrentProcess :: !*TSt -> (!ProcessId, !*TSt)
/**
* Extract the calculated task forest data structure from the TSt
*/
getTaskTree :: !*TSt	-> (!TaskTree, !*TSt)

//// TASK CREATION

/**
* Wrap a function of proper type to create a function that also
* keeps track of the the internal numbering and administration.
* The given task function will add a single basic step to the current
* sequence.
*
* @param A name used as the task label
* @param The function on the TSt that is the task
*
* @return The newly constructed basic task
*/
mkInteractiveTask			:: !String !(*TSt -> *(!a,!*TSt)) -> Task a

/**
* Wrap a function of proper type to create a function that also
* keeps track of the the internal numbering and administration.
* The given task function will add a single instantly computed step to
* the current sequence.
*
* @param A name used as the task label
* @param The function on the TSt that is the task
*
* @return The newly constructed basic task
*/
mkInstantTask		:: !String !(*TSt -> *(!a,!*TSt)) -> Task a

/**
* Wrap a function of proper type to create a function that also
* keeps track of the the internal numbering and administration.
* The given task function will add a monitoring task to the 
* the current sequence.
*
* @param A name used as the task label
* @param The function on the TSt that is the task
*
* @return The newly constructed basic task
*/
mkMonitorTask :: !String !(*TSt -> *(!a,!*TSt)) -> Task a


/**
* Creates an rpc task from an rpc call definition.
* A parse function is used to parse the result of the rpc call
*
* @param A name used as the task label
* @param The initial RPC info record
* @param The parse function
*
* @return The constructed RPC task
*/
mkRpcTask :: !String !RPCInfo !(String -> a) -> Task a | gUpdate{|*|} a


/**
* Wraps a function of proper type to create a task that will consist
* of a sequence of subtasks. The given task function will execute in a blank sequence
* and the resulting sequence will be combined in a single sequence node.
*
* @param A name used as the task label
* @param The function on the TSt that is the task
*
* @return The newly constructed sequence task
*/
mkSequenceTask		:: !String !(*TSt -> *(!a,!*TSt)) -> Task a

/**
* Wrap a function of proper type to create a function that also
* keeps track of the the internal numbering and administration for
* combining a set of parallel subtasks
*
* @param A name used as the task label
* @param The function on the TSt that is the task
*
* @return The newly constructed parallel task
*/
mkParallelTask 		:: !String !(*TSt -> *(!a,!*TSt)) -> Task a

/**
* Wrap a function of proper type to create a function that will make a
* main task. This is a sequence node that keeps track of additional information
* such as task status, event times and user/delegate information.
*
* @param A name used as the task label
* @param The task that will run inside the main task
*
* @return The newly constructed sequence task
*/
mkMainTask		:: !String !(*TSt -> *(!a,!*TSt)) -> Task a

//// TASK APPLICATION

/**
* Applies a task to the task state and yields the tasks result.
*
* @param The task that is applied
* @param The task state
*
* @return The value produced by the task
* @return The modified task state
*/
applyTask			:: !(Task a) !*TSt -> (!a,!*TSt) | iTask a


//// TASK CONTENT
setTUIDef			:: !TUIDef !*TSt				-> *TSt	//Only for interactive tasks

setTUIUpdates		:: ![TUIUpdate] !*TSt			-> *TSt //Only for interactive tasks

setStatus			:: ![HtmlTag] !*TSt				-> *TSt	//Only for monitor tasks

getTaskValue		:: !*TSt						-> (Maybe a, !*TSt) | TC a

getUserUpdates		:: !*TSt						-> ([(String,String)],!*TSt)

//loadTaskFunction	:: !TaskNr !*TSt				-> (Maybe (Task a), !*TSt)

/**
* Writes a 'task scoped' value to the store
* These values are copied and garbage collected along with a task
*/
setTaskStore		:: !String !a !*TSt				-> *TSt | iTask a
/**
* Reads a 'task scoped' value from the store
*/
getTaskStore		:: !String !*TSt				-> (Maybe a, !*TSt) | iTask a

/**
* Store and load the result of a workflow instance
*/
loadProcessResult		:: !TaskNr 					!*TSt -> (!Maybe a, !*TSt) | TC a
storeProcessResult		:: !TaskNr !Dynamic			!*TSt -> *TSt

/**
* Removes all events for the current task. This is automatically called by applyTask
* after task evaluation to prevent updates from being applied twice.
*/
clearUserUpdates	:: !*TSt						-> *TSt

/**
* Sets the combination type of the current task (only for ParallelTasks)
*/
setCombination		:: !TaskCombination !*TSt	-> *TSt
/**
* Sets the combination type for the next parallel task
*/
setNextCombination	:: !TaskCombination !*TSt	-> *TSt
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


//// UTILITY

/**
* Parses a formatted task number to its integer list representation
*
* @param The task nr as formatted string
*
* @return The task nr as integer list
*/
taskNrFromString 	:: !String 					-> TaskNr
/**
* Converts a task number to its dotted string representation
*
* @param The task number as integer list
*
* @return The formatted task number
*/
taskNrToString		:: !TaskNr 					-> String

/**
* Extracts the task label of a task
*
* @param The task
* 
* @return The task's label
*/
taskLabel			:: !(Task a)				-> String

//Should not be public!
createTaskThread :: !(Task a) -> (*TSt -> *(!Dynamic,!*TSt)) | iTask a
storeTaskThread :: !TaskNr !(*TSt -> *(!Dynamic,!*TSt)) !*TSt -> *TSt
loadTaskThread :: !TaskNr !*TSt -> (*TSt -> *(!Dynamic,!*TSt), !*TSt)
loadTaskFunctionStatic  :: !TaskNr !*TSt -> (!Maybe (Task a),       !*TSt) | TC a
storeTaskFunctionStatic  :: !TaskNr !(Task a)       !*TSt -> *TSt | TC a