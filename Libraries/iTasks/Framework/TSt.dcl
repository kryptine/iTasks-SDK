definition module TSt
/**
* This module defines the core task state data structure which is transformed by tasks.
* 
* Additionally it provides utility functions to manipulate the state.
*/
import StdMaybe
import TaskTree, Types, Void, iDataForms
from HSt		import :: HSt
from SessionDB	import :: Session{..}
from Time		import :: Time(..)

// The task state
:: *TSt 		=	{ taskNr 		:: !TaskNr									// for generating unique form-id's
					, userId		:: !UserId									// id of user to which task is assigned
					, delegatorId	:: !UserId									// id of user who issued the task
					, tree			:: !TaskTree								// accumulator for constructing a task tree			
					, activated		:: !Bool   									// if true activate task, if set as result task completed

					, mainTask		:: !ProcessId								// The id of the current main task 
					, newProcesses	:: ![ProcessId]								// A list of spawned processes for the current user
							
					, options		:: !Options									// iData lifespan and storage format
					, staticInfo	:: !StaticInfo								// info which does not change during a run
					
					, exception		:: !Maybe Dynamic							// Optional, used when raising exceptions
					
					, doChange		:: !Bool									// Apply first change
					, changes		:: ![(!String, !DynamicId, !Dynamic)]		// Persistent changes
					
					//TODO: Remove when new changes are finished					
					, changeRequests											// Optional, used when demanding dynamic changes
									:: ![(!TaskId,!ChangeCondition,!Int,!Dynamic)]	
									
					, hst			:: !*HSt									// iData state
					}

:: Options		=	{ tasklife		:: !Lifespan								// default: Session		
					, taskstorage	:: !StorageFormat							// default: PlainString
					, taskmode		:: !Mode									// default: Edit
					, combination	:: !Maybe TaskCombination					// default: TTVertical
					, trace			:: !Bool									// default: False
					}

:: StaticInfo	=	{ currentUserId		:: !UserId								// id of current user
					, currentProcessId	:: !ProcessId							// the id of the current process
					, currentSession	:: !Session								// the current session
					, threadTableLoc	:: !Lifespan							// where to store the server thread table, default is Session					
					, staticWorkflows	:: ![Workflow]							// the list of workflows supported by the application				
					}

//TODO: Remove when new changes are finished
:: ChangeCondition = CC (*TSt -> *(ChangeResult,*TSt))							// used to pass a list of change predicates down the task tree
:: ChangeResult	=	{ newCondition		:: !Maybe ChangeCondition				// new condition to pass to future handlers	
					, isApplicable		:: !Bool								// True if the change is applicable here; note that the dynamic information pushed should also match
					, applyChange		:: !Bool								// True if the work indeed has to be changed by the alternative defined 		
					}

// The task monad
:: Task a = Task !(Maybe TaskNr) !(*TSt -> *(!a,!*TSt))

// A task with a label used for labeling buttons, pulldown menus, and the like
:: LabeledTask a	:== (!String,!Task a)		

// A workflow specification
:: Workflow		=	{ name			:: !String									// a unique name of this workflow
					, label			:: !String									// a label that will be used for displaying
					, roles			:: ![String]								// the roles that are allowed to initate this workflow
					, mainTask		:: Task Void								// the main task of the workflow
					}

// A change function which may be used to change tasks at runtime
:: Change a = Change (TaskProperties (Task a) (Task a) -> (Maybe TaskProperties, Maybe (Task a), Maybe (Change a)))						

/**
* Creates an initial task state.
*
* @param The default storage location of task states
* @param The default storage location of threads
* @param The session data
* @param The workflows available in the application
* @param The iData HSt state for creating editors and doing IO
* @param The process database
*
* @return a TSt iTask state
*/
mkTSt :: !Lifespan !Lifespan !Session ![Workflow] !*HSt -> *TSt

/**
* Calculates all task trees that are relevant to the current user
*
* @param Calculate debug information
* @param The task state
*
* @return An optional error message
* @return The list of task trees (task forest)
* @return The modified task state
*/
calculateTaskForest :: !Bool !*TSt -> (!Maybe String, ![TaskTree], !*TSt)

/**
* Calculates a single task tree for a given process id
*
* @param The process id
* @param Calculate debug information
* @param The task state
*
* @return An optional error message
* @return Just an HtmlTree when the process is found, Nothing on failure
* @return The modified task state
*/
calculateTaskTree	:: !ProcessId !Bool !*TSt -> (!Maybe String, !Maybe TaskTree, !*TSt)

/**
* Applies a change to a running task process task state.
* 
* @param The process id
* @param A name for identifying the change.
* @param The packed change function. Must be a packed "Change a" //TODO: try to move the dynamic packing into this function
* @param The task state
*
* @return The modified task state
*/
applyChange :: !ProcessId !String !Dynamic !*TSt -> *TSt

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
* Adds the id of a newly created process to the list
* in the task state, such that it can be executed later on.
*
* @param The id of the newly created process
* @param The task state
*
* @return The modified task state
*/
addNewProcess :: !ProcessId !*TSt -> *TSt
/**
* Get the list of newly created processes
*
* @param The task state
*
* @return The list of new process ids
* @return The modified task state
*/
getNewProcesses :: !*TSt -> (![ProcessId], !*TSt)
/**
* Clears the list of newly created processes
*
* @param The task state
*
* @return The modified task state
*/
clearNewProcesses :: !*TSt -> *TSt

/**
* Apply a function on HSt on a TSt
*/ 
appHStTSt	:: !.(*HSt -> *HSt)			!*TSt -> *TSt

/**
* Apply a function yielding a result on HSt on a TSt
*/
accHStTSt	:: !.(*HSt -> *(.a,*HSt))	!*TSt -> (.a,!*TSt)

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

/**
* Extract the editor states that must be stored in the browser
*/
getEditorStates :: !String !*TSt	-> (![HtmlState], !*TSt)

/**
* Check if the last executed task was finished. This is used to
* determine if a process is finished. (A process is finished when
* its main task is finished)
*/
taskFinished :: !*TSt -> (!Bool, !*TSt)

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
mkBasicTask 		:: !String !(*TSt -> *(!a,!*TSt)) -> Task a | iData a

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
mkSequenceTask		:: !String !(*TSt -> *(!a,!*TSt)) -> Task a | iData a

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
mkParallelTask 		:: !String !(*TSt -> *(!a,!*TSt)) -> Task a | iData a

/**
* Wrap a function of proper type to create a function that will make a
* main task. This is a sequence node that keeps track of additional information
* such as task status, event times and user/delegate information.
*
* @param A name used as the task label
* @param The initial task info properties
* @param The task that will run inside the main task
*
* @return The newly constructed sequence task
*/
mkMainTask 		:: !String !TaskProperties !(Task a) -> Task a | iData a


/**
* Sets Html output of the current task
*/
setOutput			:: ![HtmlTag] !*TSt			-> *TSt

/**
* Sets the inputs of the current task (only for BasicTasks)
*/
setInputs			:: ![InputDefinition] !*TSt			-> *TSt

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
applyTask 			:: !(Task a) !*TSt			-> (!a,!*TSt)


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
* Determines the process number part of a task number
*
* @param The task number as integer list
* 
* @return The process number or -1 when the task number is empty
*/
taskNrToProcessNr	:: !TaskNr					-> ProcessNr