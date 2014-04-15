definition module iTasks.API.Core.TaskCombinators
/**
* This is the kernel module for the specification of workflows. It contains the core set of iTasks combinators
* with which additional combinators can be defined.
*/
from System.Time    import :: Timestamp
from iTasks.API.Core.LayoutCombinators import :: SetLayout, :: AfterLayout, :: ModifyLayout, :: SetValueAttribute, :: LayoutRules
import iTasks.Framework.Task, iTasks.Framework.SDS, iTasks.Framework.Generic

derive class iTask ParallelTaskType, WorkOnStatus

/**
* Adds a result transformation function to a task.
* The resulting task is still considered a single step in the workflow.
*
* @param Function: The transformation function. It works on maybe's to also map over instable tasks.
* @param Task: The task to which the transformation function is added
*
* @return The transformed task
*/
transform :: ((TaskValue a) -> TaskValue b) !(Task a) -> Task b | iTask a & iTask b 

/**
* Projects the result of a task in a share when its result changes.
* The resulting task is still considered a single step in the workflow.
*
* @param The projection function
* @param The share onto which the result should be projected
* @param The task that provides the result

* @return The modified task
*/
project	:: ((TaskValue a) r -> Maybe w) (ReadWriteShared r w) !(Task a) -> Task a | iTask a

/**
* The generic sequential combinator.
* It does a task followed by one out of a given list of continuations.
* Once the transition to the continuation has been made it cannot be reversed.
*
* @param Task: The first step in the sequence
* @param Value before step function: Computes the value of the composition before a step is taken
* @param Continuations: A set of continuation definitions from which one is selected to make the step
*   -OnValue: inspect the value, step if the predicate matches
*	-OnAction: enable an action if the predicate matches, step if the actions is chosen
*	-OnException: Provides an exception handler for exceptions of type e
*	-OnAllExceptions: Provides an exception handler that catches all exceptions
*
*	@return The combined task
*/
step :: !(Task a) ((Maybe a) -> (Maybe b)) [TaskCont a (Task b)] -> Task b | iTask a & iTask b

:: TaskCont a b
    =       OnValue             ((TaskValue a)  -> Maybe b)
    |       OnAction    Action  ((TaskValue a)  -> Maybe b)
    | E.e:  OnException         (e              -> b)           & iTask e
    |       OnAllExceptions     (String         -> b)

/**
* Parallel task creation
*
* @param Description: The (overloaded) task description
* @param Initial tasks: The initial list of tasks to run in parallel, each task is given
*        a view on the status of all tasks in the list
* @param Continuations: A set of continuation definitions with which the list of tasks
*        can be extended
*   -OnValue:         Inspect the value, add if the predicate matches
*	-OnAction:        Enable an action if the predicate matches, add if the actions is chosen
*	-OnException:     Provides an exception handler for exceptions of type e
*                     The task in the parallel set that raised the exception is replaced
*                     with the continuation
*	-OnAllExceptions: Provides an exception handler that catches all exceptions
*                     The task in the parallel set that raised the exception is replaced
*                     with the continuation
* @return The sum of all results
* @gin False
*/
parallel :: !d ![(!ParallelTaskType,!ParallelTask a)] [TaskCont [(!TaskTime,!TaskValue a)] (!ParallelTaskType,!ParallelTask a)] -> Task [(!TaskTime,!TaskValue a)] | descr d & iTask a
					
/**
* Get the shared state of a task list
*/
taskListState :: !(SharedTaskList a) -> ReadOnlyShared [TaskValue a]
/**
* Get the properties share of a task list
*/
taskListMeta	:: !(SharedTaskList a) -> ReadWriteShared [TaskListItem a] [(TaskId,TaskAttributes)]
/*
* Get the id of the entry in the list the current task is part of
*/
taskListSelfId :: !(SharedTaskList a) -> ReadOnlyShared TaskId
/**
* Get the current tasks management meta data share
*/
taskListSelfManagement :: !(SharedTaskList a) -> Shared TaskAttributes

//Task list manipulation
/**
* Appends a task to a task list
*/
appendTask :: !ParallelTaskType !(ParallelTask a)	!(SharedTaskList a) -> Task TaskId | iTask a
/**
* Removes (and stops) a task from a task list
*/
removeTask :: !TaskId								!(SharedTaskList a)	-> Task Void | iTask a
/**
* Focuses a task in a task list
*/
focusTask :: !TaskId                                !(SharedTaskList a) -> Task Void | iTask a

/**
* State of another process the user works on.
*/
:: WorkOnStatus
	= WOActive		//* the process is active, the current user works on it
	| WOInUse User	//* the process is active, another user is working on it
	| WOFinished	//* the process is finished
	| WOExcepted	//* an uncaught exception was thrown inside of the process
	| WODeleted		//* the process has been deleted

/**
* Work on a detached task.
*
* @param Task identification of the task to work on
* 
* @return The state of the task to work on
* @throws WorkOnException
*/
workOn :: !TaskId -> Task WorkOnStatus

/**
* Execute a task with the identity of the given user
*
* @param The user with which identity the task is to be executed
* @param The task to do
*
* @return The modified task
*/
workAs :: !User !(Task a)						-> Task a | iTask a

/**
* Provide a local read/write shared for a task to work on.
*
* @param The initial value of the shared variable
* @param The task which uses the shared variable
*/
withShared :: !b !((Shared b) -> Task a) -> Task a | iTask a & iTask b

/**
* Expose a share to be accessable via an URL.
*
* @param The share to be exposed
* @param The task which uses the shared variable
*/
exposeShared :: !(RWShared p r w) !(String (RWShared p r w) -> Task a) -> Task a | iTask a & iTask r & iTask w & JSONDecode{|*|} p & JSONEncode{|*|} p & TC p

/**
* Fine tune a task by specifying custom layouts, tweaking generic layouts,
* or add additional titles, hints and descriptions
*/
class tune b    :: !b !(Task a) -> Task a
class tunev b a | iTask a :: !(b a) !(Task a) -> Task a

instance tune	SetLayout				//Set layout algorithm
instance tune	AfterLayout				//Apply a modification after a layout has been run
instance tune	ModifyLayout			//Modify the existing layout
instance tunev  SetValueAttribute a     //Set a meta attribute based on the current task value

/**
*  Fine tune evaluation behaviour
*/
instance tune	LazyRefresh
