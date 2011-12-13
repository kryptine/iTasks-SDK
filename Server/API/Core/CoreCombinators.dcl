definition module CoreCombinators
/**
* This is the kernel module for the specification of workflows. It contains the core set of iTasks combinators
* with which additional combinators can be defined.
*/
from Time				import :: Timestamp
from SharedCombinators	import :: Shared, :: ReadOnlyShared, :: ReadWriteShared
import Task

import iTaskClass
derive class iTask ParallelTaskMeta, ParallelControl, ParallelTaskType

/**
* Adds result transformation function to a task.
* The resulting task is still considered a single step in the workflow.
*
* @param Function: The transformation function. It works on maybe's to also map over instable tasks.
* @param Task: The task to which the transformation function is added
*
*	@return The transformed task
*/
transform :: ((Maybe a) -> Maybe b) (Task a) -> Task b | iTask a & iTask b 

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
parallel :: !d !s (ResultFun s a) ![TaskContainer s] -> Task a | iTask s & iTask a & descr d

/** 
* ResultFun is called when the parallel task is stopped, either because all tasks completed, 
* or the set was stopped by a task
*/
:: ResultFun s a 		:== TerminationStatus s -> a	

:: TerminationStatus	=	AllRunToCompletion			//* all parallel processes have ended their execution
						|	Stopped						//* the control signal StopParallel has been commited
/**
* A container for a child task of a parallel.
*/				
:: TaskContainer s		:== (ParallelTaskType, (ParallelTask s))

/**
* Defines how a task is shown inside of a parallel.
*/
:: ParallelTaskType		= Embedded 
						| Detached !ManagementMeta	//* displays the task computed by the function as a distinct new task for the user identified in the worker field of ManagerProperties
						
/**
* A task inside of a parallel. The first parameter is a reference to the shared data state. The second one is a reference to the shared parallel info.
*/
:: ParallelTask s		:== (TaskList s) -> Task ParallelControl

/**
* Control flow type for parallel sets. Tasks running in parallel have to indicate whether
* to continue the parallel set or to stop the set when they complete.
*/
:: ParallelControl			= Stop | Continue

/**
* Information about a task in a parallel set.
*/
:: ParallelTaskMeta =	{ index				:: !Int									//* The task's index
						, taskId			:: !TaskId
						, taskMeta			:: !TaskMeta
						, progressMeta		:: !Maybe ProgressMeta
						, managementMeta	:: !Maybe ManagementMeta
						}
						
/**
* Get the shared state of a task list
*/
taskListState	:: (TaskList s) -> Shared s | TC s

/**
* Get the properties share of a task list
*/
taskListMeta	:: (TaskList s) -> Shared [ParallelTaskMeta]

//Manipulation 

/**
* Add a task to a task list
*/
appendTask :: !(TaskContainer s) !(TaskList s)	-> Task Int | TC s

/**
* Removes (and stops) a task from a task list
*/
removeTask :: !Int !(TaskList s)				-> Task Void | TC s

/**
* Execute a task with the identity of the given user
*/
workAs :: !User !(Task a)						-> Task a | iTask a
