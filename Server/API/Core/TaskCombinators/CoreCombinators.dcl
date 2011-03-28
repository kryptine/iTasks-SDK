definition module CoreCombinators
/**
* This is the kernel module for the specification of workflows. It contains the core set of iTasks combinators
* with which additional combinators can be defined.
*/
from Time				import :: Timestamp
from TuningCombinators	import :: Tag
from Shared				import :: Shared, :: ReadOnlyShared
from ProcessDB			import :: Process
import Task, ProcessDBTasks

derive class iTask ParallelTaskInfo

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
* Lifts a value to the task domain. The return_V task finishes immediately and yields its parameter
* as result of the task.
*
* @param The value to be returned
* @return A task that will return the value defined by the parameter
*/
return 		:: !a 										-> Task a 		| iTask a

//Repetition and loops:

/**
* Repeats a task until a given predicate holds. The predicate is tested as soon as the
* given task is finished. When it does not hold, the task is restarted.
*
* @param The task to be looped
* @param The predicate over the result of the task to determine if the combination is finished
* @return The combined task
*/
(<!)  infixl 6 	:: !(Task a)  !(a -> .Bool) 			-> Task a 		| iTask a

// Sequential composition

/**
* Execute the list of tasks one after another.
*
* @param A label for tracing
* @param The list of tasks to be executed sequentially
* @return The combined task
*/
sequence	:: !String ![Task a] 						-> Task [a]		| iTask a

:: Control a acc	= StopParallel												// stop the entire parallel execution
					| AppendTasks	![(!TaskContainer a,!AccuFun a acc)]		// append additional ordinary tasks to be run in parallel as well
					| AppendCTasks	![ControlTaskContainer a acc]				// append additional contorl tasks to be run in parallel as well
					| StopTasks		![TaskIndex]								// kill ordinary & control tasks with indicated index
					| ResetTasks	![TaskIndex]								// start ordinary & control tasks with indicated index from scratch
					| ReplaceTasks	![(!TaskIndex,!TaskContainer a)]			// replace ordinary tasks with indicated index
					| ReplaceCTasks	![(!TaskIndex,!ControlTaskContainer a acc)]	// replace control tasks with indicated index
					| FocusTask		!TaskIndex									// set the window focus of indicated ordinary or control task
					
derive class iTask Control

/**
* AccuFun is called when an ordinary parallel iTask task (not a control task) terminates returning a value of type a 
* 
* @param The value returned by the terminated task
* @param The current value of the accumulator 
* @return Tuple with new value of the accumulator, and a list of control signals
*/
:: AccuFun taskResult pState :== taskResult pState -> (!pState, ![Control taskResult pState])

// Index of parallel executing processes, number of processes can dynamically increase
:: TaskIndex :== Int

/**
* ResultFun  is called when the parallel task is stopped
* 
* @param The termination status: why was the parallel task ended
* @param The current value of the accumulator 
* @return The resulting value of type b
*/
:: ResultFun pState pResult :== TerminationStatus pState -> pResult
:: TerminationStatus	=	AllRunToCompletion	// all parallel processes have ended their execution
						|	Stopped				// the control signal StopParallel has been commited

:: ParallelTaskInfo =	{ index				:: !TaskIndex				// the task's index
						, taskProperties	:: !TaskProperties			// task properties
						, processProperties	:: !Maybe ProcessProperties	// process properties for tasks which are detached processes
						}

/**
* A control task is a special task used to control and view the other tasks and their manager properties.
* It can generate control signals, after which it will re-incarnate itself.
*
* @param The control view enabling to view the current state of the accumulator and the properties of all subtasks and change manager properties of detached tasks
* @return A list of control signals
*/
:: ControlTaskContainer a acc :== ParamTaskContainer (Shared (!acc,![ParallelTaskInfo]) [(!TaskIndex,!ManagerProperties)]) [Control a acc]

/**
* All-in-one swiss-army-knife parallel task creation
*
* @param The (overloaded) task description
* @param The accumulator
* @param A function defining how to convert the accumulator tot the final result when the parallel task finishes
* @param Layout merge function that layouts the user interfaces of tasks that are placed in the body
* @param The list of Control tasks to run in parallel, each task is given a read-only view on the status of all tasks in the set
* @param The list of ordinary tasks to run in parallel
* @return The resulting value
*/
parallel :: !d !pState !(ResultFun pState pResult) ![ControlTaskContainer taskResult pState] ![(!TaskContainer taskResult,!AccuFun taskResult pState)] -> Task pResult | iTask taskResult & iTask pState & iTask pResult & descr d

// Multi-user workflows

/**
* Create a new process.
*
* @param Automatically garbage collect the process when it is finished (removing all references to the state of the process).
* @param The task that is to be started in the new process.
*
* @return A reference to the newly created process
*/
spawnProcess	:: !Bool !(TaskContainer a) -> Task (!ProcessId,!SharedProc,!SharedProcResult a) | iTask a

:: SharedProc			:== ReadOnlyShared (Maybe Process)
// the first maybe indicates if the process finished, the second if result is deleted
:: SharedProcResult a	:== ReadOnlyShared (Maybe (Maybe a))

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
