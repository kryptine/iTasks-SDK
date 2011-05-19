definition module CoreCombinators
/**
* This is the kernel module for the specification of workflows. It contains the core set of iTasks combinators
* with which additional combinators can be defined.
*/
from Time				import :: Timestamp
from TuningCombinators	import :: Tag
from Shared				import :: Shared, :: ReadOnlyShared, :: SymmetricShared
from ProcessDB			import :: Process
import Task, ProcessDBTasks

derive class iTask ParallelTaskInfo
derive JSONEncode	TaskContainer
derive JSONDecode	TaskContainer
derive gUpdate		TaskContainer
derive gDefaultMask	TaskContainer
derive gVerify		TaskContainer
derive gVisualize	TaskContainer
derive gEq			TaskContainer

//Standard monadic operations:

/**
* Combines two tasks sequentially. The first task is executed first. When it is finished
* the second task is executed with the result of the first task as parameter.
*
* @param The first task to be executed
* @param The second task, which receives the result of the first task
* @return The combined task
*/
(>>=) infixl 1 	:: !(Task a) !(a -> Task b) 			-> Task b		| iTask a & iTask b
/**
* Combines two tasks sequentially just as >>=, but the result of the first task is disregarded.
*
* @param The first task to be executed
* @param The second task to be executed
* @return The combined task
*/
(>>|) infixl 1 :: !(Task a) (Task b)					-> Task b		| iTask a & iTask b


/**
* All-in-one swiss-army-knife parallel task creation
*
* @param The (overloaded) task description
* @param The accumulator
* @param A function defining how to convert the accumulator to the final result when the parallel task finishes
* @param The list of tasks to run in parallel, each task is given a view on the status of all tasks in the set
* @return The resulting value
*/
parallel :: !d !s !(ResultFun s a) ![TaskContainer s] -> Task a | iTask s & iTask a & descr d

:: ResultFun s a 		:== TerminationStatus s -> a	//ResultFun is called when the parallel task is stopped
														//either because all tasks completed, or the set was stopped by a task

:: TerminationStatus	=	AllRunToCompletion			// all parallel processes have ended their execution
						|	Stopped						// the control signal StopParallel has been commited
				
:: TaskContainer s		= E.a: DetachedTask	!ManagerProperties !ActionMenu	!((SymmetricShared s) (ParallelInfo s) -> Task a) & iTask a
						| E.a: WindowTask	!WindowTitle !ActionMenu		!((SymmetricShared s) (ParallelInfo s) -> Task a) & iTask a
						| E.a: DialogTask	!WindowTitle					!((SymmetricShared s) (ParallelInfo s) -> Task a) & iTask a
						| E.a: InBodyTask									!((SymmetricShared s) (ParallelInfo s) -> Task a) & iTask a
						| E.a: HiddenTask									!((SymmetricShared s) (ParallelInfo s) -> Task a) & iTask a

:: ParallelInfo s		:== Shared [ParallelTaskInfo] [Control s]
:: ParallelTaskInfo =	{ index			:: !TaskIndex								// The task's index
						, properties	:: !Either TaskProperties ProcessProperties // Task properties for inbody tasks and process
						}															// properties for detached tasks

:: Control s			= StopParallel												// stop the entire parallel execution
						| AppendTask		!(TaskContainer s)						// append and additional task to be run in parallel as well
						| RemoveTask		!TaskIndex								// remove the task with indicated index from the set
						| UpdateProperties	!TaskIndex !ManagerProperties			// update the properties of a task
						| FocusTask			!TaskIndex								// set the window focus of indicated ordinary or control task
					
:: TaskIndex			:== Int

derive class iTask Control

// Multi-user workflows

/**
* Create a new process.
*
* @param Automatically garbage collect the process when it is finished (removing all references to the state of the process).
* @param The initial manager properties
* @param The task's menu
* @param The task that is to be started in the new process.
*
* @return A reference to the newly created process
*/
spawnProcess	:: !Bool !ManagerProperties !ActionMenu !(Task a) -> Task ProcessId | iTask a

/**
* Kills a process disregarding any other references to this process.
*
* @param The process reference
*
* @return Void
*/
killProcess 	:: !ProcessId -> Task Void

/**
* Spawn a process at regular times
*
* @param A function that computes the next time a new instance to be spawned
* @param The task to spawn as process
*
* @return A reference to a control memory this contains a schedulerstate to control the scheduler and a list of active processes.
*/
scheduledSpawn	:: !(DateTime -> DateTime) !(Task a) -> Task (ReadOnlyShared (!SchedulerState,![ProcessId])) | iTask a

:: SchedulerState = SSActive //Keep monitoring time and spawn new tasks
				  | SSFinish //Let the already running tasks finish, but don't start new ones anymore
				  | SSCancel //Stop immediately, cancel all active tasks.
