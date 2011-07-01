definition module CoreCombinators
/**
* This is the kernel module for the specification of workflows. It contains the core set of iTasks combinators
* with which additional combinators can be defined.
*/
from Time				import :: Timestamp
from TuningCombinators	import :: Tag
from SharedCombinators	import :: Shared, :: ReadOnlyShared, :: ReadWriteShared
from ProcessDB			import :: Process
import Task

import iTaskClass
derive class iTask ParallelTaskInfo, ParallelControl


//Standard monadic operations:

/**
* Combines two tasks sequentially. The first task is executed first. When it is finished
* the second task is executed with the result of the first task as parameter.
*
* @param First: The first task to be executed
* @param Second: The second task, which receives the result of the first task
* @return The combined task
* 
* @gin False
*/
(>>=) infixl 1 	:: !(Task a) !(a -> Task b) 			-> Task b		| iTask a & iTask b
/**
* Combines two tasks sequentially just as >>=, but the result of the first task is disregarded.
*
* @param First: The first task to be executed
* @param Second: The second task to be executed
* @return The combined task
*
* @gin False
*/
(>>|) infixl 1 :: !(Task a) (Task b)					-> Task b		| iTask a & iTask b

/**
* Adds terminator generator (yielding user actions or trigger) to task.
* The input of the function is the current state of the task.
* The state of interaction tasks is changing during execution.
* Other tasks are first executed, their constant state is their result.
* The new return type of the task is determined by the type yieded by the generated terminators.
*
* @param Task: The task to which terminators are added
* @param Terminator generator function: The function generating the terminators
* @return The task which added terminators
*
* @gin False
*/
(>>+) infixl 1 :: !(Task a) !(TermFunc a b) -> Task b | iTask a & iTask b

/*
* Empty list of actions.
* 'task >>+ noActions' never terminates.
*/
noActions :: (TermFunc a b) | iTask a & iTask b

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
:: TaskContainer s		:== (TaskGUI, (ParallelTask s))

/**
* Defines how a task is shown inside of a parallel.
*/
:: TaskGUI				= DetachedTask !ManagerProperties						//* displays the task computed by the function as a distinct new task for the user identified in the worker field of ManagerProperties
						| WindowTask   !WindowTitle								//* displays the task computed by the function in a window
						| DialogTask   !WindowTitle								//* displays the task computed by the function in a dialog (a dialog can not have a menu)
						| BodyTask												//* inlines the task in the current task
						| HiddenTask											//* does not display the task at all

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
:: ParallelTaskInfo =	{ index			:: !Int										//* The task's index
						, properties	:: !Either TaskProperties ProcessProperties //* Task properties for inbody tasks and process properties for detached tasks
						}

/**
* Get the shared state of a task list
*/
taskListState		:: (TaskList s) -> Shared s | TC s

/**
* Get the properties share of a task list
*/
taskListProperties	:: (TaskList s) -> Shared [ParallelTaskInfo]

//Manipulation 

/**
* Add a task to a task list
*/
appendTask :: !(TaskContainer s) !(TaskList s)	-> Task Int | TC s

/**
* Removes (and stops) a task from a task list
*/
removeTask :: !Int !(TaskList s)				-> Task Void | TC s
