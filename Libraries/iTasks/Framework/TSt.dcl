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

// The task state
:: *TSt 		=	{ taskNr 		:: !TaskNr									// for generating unique form-id's
					, userId		:: !UserId									// id of user to which task is assigned
					, delegatorId	:: !UserId									// id of user who issued the task
					, tree			:: !TaskTree								// accumulator for constructing a task tree			
					, activated		:: !Bool   									// if true activate task, if set as result task completed
					, users			:: ![UserId]								// list of all users working on some task (including subtasks)

					, newProcesses	:: ![ProcessId]								// A list of spawned processes for the current user
							
					, options		:: !Options									// iData lifespan and storage format
					, staticInfo	:: !StaticInfo								// info which does not change during a run
					
					, exceptions	:: ![Dynamic]								// Optional exceptions
					, changeDemands	:: ![(RaiseCondition, Dynamic)]				// Optional change for a specific task
									
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
:: RaiseCondition = RC (*TSt -> *(Bool,Maybe RaiseCondition,*TSt))				// used to pass a list of change predicates down the task tree
														


// The task monad
:: Task a = Task !(*TSt -> *(!a,!*TSt))

// a task with a label used for labeling buttons, pulldown menus, and the like
:: LabeledTask a	:== (!String,!Task a)		

// A workflow specification
:: Workflow		=	{ name			:: !String									// a unique name of this workflow
					, label			:: !String									// a label that will be used for displaying
					, roles			:: ![String]								// the roles that are allowed to initate this workflow
					, mainTask		:: Task Void								// the main task of the workflow
					}
					
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
* Resets the TSt for calculating a new task Tree
*
* @param The task state
*
* @return The modified task state
*/
resetTSt :: *TSt -> *TSt

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
calculateTaskTree	:: !Int !Bool !*TSt -> (!Maybe String, !Maybe TaskTree, !*TSt)

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
* Adds an additonal user other than the task owner working on some part of the task
*
* @param the new user id
* @param the task state
*
* @return the modified task state
*/
addAdditionalUser :: !UserId !*TSt -> *TSt

/**
* Retrieves the list of users other than the task owner working on some workflow
*
* @param the task state
* 
* @return The list of users
* @return The modified task state
*/
getAdditionalUsers :: !*TSt -> (![UserId],!*TSt)

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
* Applies a task to the task state without yielding the result of the task.
*
* @param The task that is applied
* @param The task state
*
* @return The modified task state
*/
appTaskTSt :: !(Task a) !*TSt 					-> *TSt

/**
* Applies a task to the task state and yields the tasks result.
*
* @param The task that is applied
* @param The task state
*
* @return The value produced by the task
* @return The modified task state
*/
accTaskTSt 			:: !(Task a) !*TSt			-> (!a,!*TSt)

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
getEditorStates :: !*TSt	-> (![HtmlState], !*TSt)

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
* @param Force once: If true, the function will never be recomputed, but the result will be stored
* @param The function on the TSt that is the task
*
* @return The newly constructed basic task
*/
mkBasicTask 		:: !String !(*TSt -> *(!a,!*TSt)) -> Task a | iData a

/**
* Wraps a function of proper type to create a task that will consist
* of a sequence of subtasks. The given task function will execute in a blank sequence
* and the resulting sequence will be combined in a single sequence node.
*/
mkSequenceTask		:: !String !(*TSt -> *(!a,!*TSt)) -> Task a | iData a

/**
* Wrap a function of proper type to create a function that also
* keeps track of the the internal numbering and administration for
* combining a set of parallel subtasks
*/
mkParallelTask 		:: !String !(*TSt -> *(!a,!*TSt)) -> Task a | iData a

/**
* Wraps a function of proper type to create a task that will become
* part of a parallel set of subtasks. This function should only be used
* by tasks that are wrapped with mkParallelTask
*/
//mkParallelSubTask 	:: !String !Int (Task a) 	-> Task a  		| iCreateAndPrint a

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
 

//// UTILITY

/**
* Deletes iData states for all subtasks of the given task number.
* This function can be used for ad-hoc garbage collection.
* 
* @param The task number of the task which subtasks must be deleted
* @param The task state
*
* @return The task state
*/
deleteAllSubTasks 	:: ![TaskNr] TSt 			-> TSt
/**
* Utility function to increment the last segment a task number
*
* @param The original task number
*
* @return The incremented task number
*/ 
incTaskNr 			:: !TaskNr 					-> TaskNr
/**
* Converts a task number to its dotted string representation
*
* @param The task number as integer list
*
* @return The formatted task number
*/
taskNrToString		:: !TaskNr 					-> String
/**
* Parses a formatted task number to its integer list representation
*
* @param The task nr as formatted string
*
* @return The task nr as integer list
*/
taskNrFromString 	:: !String 					-> TaskNr
/**
* Determines the process number part of a task number
*
* @param The task number as integer list
* 
* @return The process number or -1 when the task number is empty
*/
taskNrToProcessNr	:: !TaskNr					-> ProcessNr
