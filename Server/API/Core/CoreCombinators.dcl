definition module CoreCombinators
/**
* This is the kernel module for the specification of workflows. It contains the core set of iTasks combinators
* with which additional combinators can be defined.
*/
from Time				import :: Timestamp
from SharedCombinators	import :: Shared, :: ReadOnlyShared, :: ReadWriteShared
from LayoutCombinators	import :: SetLayout, :: ModifyLayout, :: Layout
import Task

import iTaskClass
derive class iTask ParallelResult, ParallelTaskType

/**
* Adds a result transformation function to a task.
* The resulting task is still considered a single step in the workflow.
*
* @param Function: The transformation function. It works on maybe's to also map over instable tasks.
* @param Task: The task to which the transformation function is added
*
* @return The transformed task
*/
transform :: ((Maybe a) -> Maybe b) (Task a) -> Task b | iTask a & iTask b 

/**
* Projects the result of a task in a share when its result changes.
* The resulting task is still considered a single step in the workflow.
*
* @param The projection function
* @param The share onto which the result should be projected
* @param The task that provides the result

* @return The modified task
*/
project	:: ((Maybe a) r -> Maybe w) (ReadWriteShared r w) (Task a) -> Task a | iTask a

/**
* The generic sequential combinator.
* It does a task followed by one out of a given list of continuations.
* Once the transition to the continuation has been made it cannot be reversed.
*
* @param Task: The first step in the sequence
* @param Continuations: A set of continuation definitions from which one is selected
*   -AnyTime: Provides an action which is always enabled. Both for stable and instable tasks
*	-WithResult: Provides an action which is enabled when the result is valid and the predicate holds
*	-WithoutResult: Provides an action which is enabled only when the result is invalid
*	-WhenValid: Provides a trigger that fires as soon as the result matches the predicate
*	-WhenStable: Provides a trigger that fires as soon as the result becomes stable
*	-Catch: Provides an exception handler for exceptions of type e
*	-CathcAll: Provides an exception handler that catches all exceptions
*
*	@return The combined task
*/
step :: (Task a) [TaskStep a b] -> Task b | iTask a & iTask b

:: TaskStep a b
	=		AnyTime				Action				((Maybe a) -> Task b)
	|		WithResult			Action	(a -> Bool)	(a -> Task b)		
	|		WithoutResult		Action				(Task b)				
	|		WhenValid					(a -> Bool)	(a -> Task b)
	|		WhenStable								(a -> Task b)
	| E.e:	Catch									(e -> Task b)		& iTask e
	|		CatchAll								(String -> Task b)


/**
* All-in-one swiss-army-knife parallel task creation
*
* @param Description: The (overloaded) task description
* @param Accumulator: The accumulator
* @param Merge: Function defining how to convert the accumulator to the final result when the parallel task finishes
* @param Tasks: The list of tasks to run in parallel, each task is given a view on the status of all tasks in the set
* @return The resulting value
* 
* @gin False
*/
parallel :: !d !a ![(!ParallelTaskType,!ParallelTask a)] -> Task a | descr d & iTask a
					
/**
* Get the shared state of a task list
*/
taskListState	:: (SharedTaskList s) -> Shared s
/**
* Get the properties share of a task list
*/
taskListMeta	:: (SharedTaskList s) -> ReadOnlyShared [TaskListItem]

//Manipulation 

/**
* Appends a task to a task list
*/
appendTask :: !ParallelTaskType !(ParallelTask s)	!(SharedTaskList s)	-> Task Int | TC s
/**
* Removes (and stops) a task from a task list
*/
removeTask :: !TaskId								!(SharedTaskList s)	-> Task Void | TC s
/**
* Execute a task with the identity of the given user
*/
workAs :: !User !(Task a)						-> Task a | iTask a

/**
* Fine tune a task by specifying custom layouts, tweaking generic layouts,
* or add additional titles, hints and descriptions
*/
class tune b :: !b !(Task a) -> Task a
instance tune	SetLayout				//Set layout algorithm
instance tune	ModifyLayout			//Modify the existing layout
