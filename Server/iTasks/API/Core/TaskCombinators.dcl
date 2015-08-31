definition module iTasks.API.Core.TaskCombinators
/**
* This is the kernel module for the specification of workflows. It contains the core set of iTasks combinators
* with which additional combinators can be defined.
*/
from System.Time    import :: Timestamp
from iTasks.UI.Layout import :: SetLayout, :: AfterLayout, :: ModifyLayout, :: SetValueAttribute, :: LayoutRules
import iTasks._Framework.Task, iTasks._Framework.SDS, iTasks._Framework.Generic

derive class iTask ParallelTaskType, AttachmentStatus

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
parallel :: ![(!ParallelTaskType,!ParallelTask a)] [TaskCont [(!TaskTime,!TaskValue a)] (!ParallelTaskType,!ParallelTask a)] -> Task [(!TaskTime,!TaskValue a)] | iTask a

//Task list manipulation
/**
* Appends a task to a task list
*/
appendTask  :: !ParallelTaskType !(ParallelTask a)	!(SharedTaskList a) -> Task TaskId | iTask a
/**
* Removes (and stops) a task from a task list
*/
removeTask  :: !TaskId								!(SharedTaskList a)	-> Task () | iTask a
/**
* Replaces a task in a list and resets its execution state.
* All meta-data is kept
*/
replaceTask :: !TaskId !(ParallelTask a)            !(SharedTaskList a) -> Task () | iTask a
/**
* Focuses a task in a task list
*/
focusTask   :: !TaskId                              !(SharedTaskList a) -> Task () | iTask a

/**
* State of another process the user works on.
*/
:: AttachmentStatus
    = ASAttached Stability  //* the task instance is currently attached to this task
    | ASInUse TaskId 		//* the task instance is already attached to another task 
    | ASExcepted            //* the task instance had an uncaught exception
    | ASDeleted             //* the task instance does not exist anymore
    | ASIncompatible        //* the task instance can not be executed in this is version of the program (it was created by an older version)

/**
* Attaches a a detached task.
*
* @param Task identification of the task to attach
* 
* @return The state of the task to work on
* @throws WorkOnException
*/
attach :: !TaskId -> Task AttachmentStatus

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
exposeShared :: !(RWShared p r w) !(String (RWShared p r w) -> Task a) -> Task a | iTask a & iTask r & iTask w & iTask p

/**
* Fine tune a task by specifying custom layouts, tweaking generic layouts,
* or add additional titles, hints and descriptions
*/
class tune b    :: !b !(Task a) -> Task a
class tunev b a | iTask a :: !(b a) !(Task a) -> Task a

instance tune	SetLayout				//Set layout algorithm
instance tune	AfterLayout				//Apply a modification after a layout has been run
instance tune	ModifyLayout			//Modify the existing layout

/**
*  Fine tune evaluation behaviour
*/
instance tune	LazyRefresh
