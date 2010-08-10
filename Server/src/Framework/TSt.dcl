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
from ProcessDB	import :: Process, :: Menu
from Config		import :: Config(..)
from Store		import :: Store(..)
from Time		import :: Timestamp(..)
from Http		import :: HTTPRequest
from ProcessDB	import :: Action

from	iTasks import class iTask(..)
import	GenPrint, GenParse, GenVisualize, GenUpdate

// The task state
:: *TSt 		=	{ taskNr 		:: !TaskNr											// for generating unique form-id's
					, taskInfo		:: !TaskInfo										// task information available to tasks
					, tree			:: !TaskTree										// accumulator for constructing a task tree
					, newTask		:: !Bool											// does the task run for the first time
					
					, events		:: ![TaskEvent]									// The update events for interactive tasks
																						// (task id, name, value)
																						
					, properties	:: !TaskProperties									// Properties of the current evaluated process		
					, menus			:: !Maybe [Menu]									// Menu structure of the current task
					, menusChanged	:: !Bool											// Has the menu structure been changed
					
					, staticInfo	:: !StaticInfo										// info which does not change during a run
										
					, currentChange	:: !Maybe (!ChangeLifeTime,!ChangeDyn)				// An active change
					, pendingChanges:: ![(!ChangeLifeTime,!ChangeDyn)]					// Pending persistent changes
					
					
					, request		:: !HTTPRequest										// The current http request
					
					, iworld		:: !*IWorld											// The 'iTasks' world				
					}
					

:: StaticInfo	=	{ appName			:: String										// the name of the server executable
					, currentProcessId	:: !ProcessId									// the id of the current process
					, currentSession	:: Session										// the current session			
					, staticWorkflows	:: ![Workflow]									// the list of workflows supported by the application				
					}

// A workflow specification
:: Workflow		=	{ path			:: !String											// a unique name of this workflow
					, roles			:: ![String]										// the roles that are allowed to initate this workflow
					, thread		:: !Dynamic											// the thread of the main task of the workflow
					}
/**
* Creates an initial task state.
*
* @param The application name
* @param The server configuration
* @param The current HTTP request
* @param The workflows available in the application
* @param The generic data store
* @param The world
*
* @return a TSt iTask state
*/
mkTSt :: String Config HTTPRequest ![Workflow] !*Store !*World -> *TSt

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
* @param The task that is to be converted to a runnable thread
* 
* @return A dynamic containing the thread
*/
createThread :: !(Task a) -> Dynamic	| iTask a

/**
* Creates an instance of a task definition
* As soon as an instance is created it is immediately evaluated once.
*
* @param A task thread of the task to create an instance of
* @param Start as toplevel, or as subtask of another task (parent information is read from the task state)
* @param Whether this process is part of a parallel
* @param Activate the task instance immediately
* @param Delete the result and process record of the instance after completion
* @param The task state
*
* @return The process id of the new instance
* @return The result of the first run (as dynamic result)
* @return The task tree created at the first run
* @return The modified task state
*/
createTaskInstance :: !Dynamic !Bool !(Maybe TaskParallelType) !Bool !Bool !*TSt -> (!ProcessId, !TaskResult Dynamic, !TaskTree, !*TSt)

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
* @param The value updates to apply
* @param Optionally a new Change that is to be applied to this task instance
* @param Is the instance evaluated as top node, or as subnode while evaluating a parent process
* @param Is the task evaluated for the first time
* @param The task state
*
* @return The modified task state
*/
evaluateTaskInstance :: !Process ![TaskEvent] !(Maybe ChangeInjection) !Bool !Bool !*TSt-> (!TaskResult Dynamic, !TaskTree, !*TSt)
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
* @param The value updates to apply
* @param The task state
*
* @return Just an HtmlTree when the process is found, Nothing on failure
* @return The modified task state
*/
calculateTaskTree :: !TaskId ![TaskEvent] !*TSt -> (!TaskTree, !*TSt)
/**
* Render resultpanel from a task which is not process
*
* @param The task id
* @param The task state
*
* @ return The task tree
* @ return The modified task state
**/
calculateTaskResult :: !TaskId !*TSt -> (!TaskTree, !*TSt)
/**
* Calculates all task trees
*
* @param The value updates to apply
* @param The task state
*
* @return The list of task trees (task forest)
* @return The modified task state
*/
calculateTaskForest :: ![TaskEvent] !*TSt -> (![TaskTree], !*TSt)
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
getTaskTree :: !*TSt	-> (!TaskTree, !*TSt)

//// TASK CREATION

/**
* Lift a function that uses the TSt to a function that returns
* a TaskResult value of TaskFinished with the value returned by the function.
*
* @param The function on the TSt
*
* @return The task function
*/
mkTaskFunction :: (*TSt -> (!a,!*TSt)) -> (*TSt -> (!TaskResult a,!*TSt))
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
mkInteractiveTask			:: !String !(*TSt -> *(!TaskResult a,!*TSt)) -> Task a
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
mkInstantTask		:: !String !(*TSt -> *(!TaskResult a,!*TSt)) -> Task a
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
mkMonitorTask :: !String !(*TSt -> *(!TaskResult a,!*TSt)) -> Task a
/**
* Wrap a function of the proper type to create a function that 
* displays an (offline) instruction to the user. The user can only
* dismiss the instruction. No result is returned.
*
* @param A name used as the task label
* @param The function on the TSt that is the task
*
* @return Void
*/
mkInstructionTask :: !String !(*TSt -> *(!TaskResult Void,!*TSt)) -> Task Void
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
mkRpcTask :: !String !RPCExecute !(String -> a) -> Task a | gUpdate{|*|} a
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
mkSequenceTask :: !String !(*TSt -> *(!TaskResult a,!*TSt)) -> Task a
/**
* Wrap a function of proper type to create a function that also
* keeps track of the the internal numbering and administration for
* combining a set of parallel subtasks. Each of the subtasks is wrapped in a 
* separate process. 
*
* @param A name used as the task label
* @param Some info about the behavior of the parallel task
* @param The function on the TSt that is the task
*
* @return The newly constructed parallel task
*/
mkParallelTask :: !String !TaskParallelInfo !(*TSt -> *(!TaskResult a,!*TSt)) -> Task a
/**
* Wrap a function of proper type to create a function that groups a number of 
* tasks together. None of the subtasks is created in a separate process and no
* overview window is shown. It is not considdered good practice to assign any of
* the subtasks to other users. For that purpose the parallel-task has been introduced
*
* @param A name used as the task label
* @param Some info about the behavior of the grouped task
* @param The function on the TSt that is the task
*
* @return The newly constructed grouped task
*/

mkGroupedTask :: !String !(*TSt -> *(!TaskResult a,!*TSt)) -> Task a
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
mkMainTask		:: !String !(*TSt -> *(!TaskResult a,!*TSt)) -> Task a

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
applyTask			:: !(Task a) !*TSt -> (!TaskResult a,!*TSt) | iTask a

/**
* Add a subnode to the current task tree
*
* @param The sub node
* @param The task state
*
* @return The modified task state
*/
addTaskNode 		:: !TaskTree !*TSt -> *TSt

//// TASK CONTENT
setTUIDef			:: !([TUIDef],[TUIButton]) [HtmlTag] ![(Action,Bool)] !*TSt 	-> *TSt //Only for interactive tasks
setTUIUpdates		:: ![TUIUpdate] ![(Action,Bool)] !*TSt							-> *TSt //Only for interactive tasks
setTUIFunc			:: (*TSt -> *(!InteractiveTask, !*TSt)) [HtmlTag] !*TSt			-> *TSt //Only for interactive tasks
setTUIMessage 		:: !([TUIDef],[TUIButton]) [HtmlTag] ![(Action,Bool)] !*TSt		-> *TSt //Only for interactive tasks
setStatus			:: ![HtmlTag] !*TSt												-> *TSt	//Only for monitor tasks
setGroupActions		:: ![(Action, (Either Bool (*TSt -> *(!Bool,!*TSt))))] !*TSt	-> *TSt //Only for group tasks
setFocusCommand		:: !String !*TSt												-> *TSt //Only for group tasks

//EVENTS
/**
* Get the events (name/value pairs) for the current task
* These events are removed from the task state
*
* @param The task state
*
* @return The matched events
* @return The modified task state
*/
getEvents			:: !*TSt						-> ([(!String,!String)],!*TSt)
/**
* Get the group events for a group of tasks
* These events are removed from the task state
*
* @param The task id of the group
* @param The task state
*
* @return The matched group events
* @return The modified task state
*/
getGroupEvents 		:: !TaskId !*TSt				-> ([(!String,!String)],!*TSt)
/**
* Test if there are events for any task during this run.
*
* @param The task state
*
* @return The test results
* @return The modified task state
*/
anyEvents			:: !*TSt						-> (!Bool,!*TSt)

/**
* Writes a 'task scoped' value to the store
* These values are copied and garbage collected along with a task
*/
setTaskStore		:: !String !a !*TSt				-> *TSt | iTask a
setTaskStoreFor 	:: !TaskNr !String !a !*TSt		-> *TSt | iTask a
/**
* Reads a 'task scoped' value from the store
*/
getTaskStore		:: !String !*TSt				-> (Maybe a, !*TSt) | iTask a
getTaskStoreFor		:: !TaskNr !String !*TSt		-> (Maybe a, !*TSt) | iTask a

/**
* Store and load the result of a workflow instance
*/
loadProcessResult		:: !TaskNr 							!*TSt -> (!Maybe (TaskResult Dynamic), !*TSt)
storeProcessResult		:: !TaskNr !(TaskResult Dynamic)	!*TSt -> *TSt

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
* Convert the names in events to data paths
*
* @param The events (name/value list)
*
* @return The converted data paths
*/
events2Paths 		:: ![(!String,!String)] 	-> [DataPath]