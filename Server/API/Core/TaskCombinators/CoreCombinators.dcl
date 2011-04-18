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

derive class iTask ParallelTaskInfo, ControlTaskContainer
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
* Lifts a value to the task domain. The return_V task finishes immediately and yields its parameter
* as result of the task.
*
* @param The value to be returned
* @return A task that will return the value defined by the parameter
*/
return 		:: !a 										-> Task a 		| iTask a

:: Control acc	= StopParallel												// stop the entire parallel execution
				| AppendTasks	![TaskContainer acc]						// append additional ordinary tasks to be run in parallel as well
				| AppendCTasks	![ControlTaskContainer acc]					// append additional contorl tasks to be run in parallel as well
				| StopTasks		![TaskIndex]								// kill ordinary & control tasks with indicated index
				| ResetTasks	![TaskIndex]								// start ordinary & control tasks with indicated index from scratch
				| ReplaceTasks	![(!TaskIndex,!TaskContainer acc)]			// replace ordinary tasks with indicated index
				| ReplaceCTasks	![(!TaskIndex,!ControlTaskContainer acc)]	// replace control tasks with indicated index
				| FocusTask		!TaskIndex									// set the window focus of indicated ordinary or control task
					
derive class iTask Control

/**
* AccuFun is called when an ordinary parallel iTask task (not a control task) terminates returning a value of type a 
* 
* @param The value returned by the terminated task (Nothing if detached task is cancelled)
* @param The current value of the accumulator 
* @return Tuple with new value of the accumulator, and a list of control signals
*/
:: AccuFun			taskResult pState :== taskResult			pState -> (!pState, ![Control pState])
:: AccuFunDetached	taskResult pState :== (Maybe taskResult)	pState -> (!pState, ![Control pState])

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

:: ParallelTaskInfo =	{ index				:: !TaskIndex								// the task's index
						, properties		:: !Either TaskProperties ProcessProperties // Task properties for inbody tasks and process
						}																// properties for detached tasks
					
:: TaskContainer acc	= E.a: DetachedTask	!ManagerProperties !ActionMenu	!(Task a) !(AccuFunDetached a acc)	& iTask a
						| E.a: WindowTask	!WindowTitle !ActionMenu		!(Task a) !(AccuFun a acc)			& iTask a
						| E.a: DialogTask	!WindowTitle					!(Task a) !(AccuFun a acc)			& iTask a
						| E.a: InBodyTask									!(Task a) !(AccuFun a acc)			& iTask a
						| E.a: HiddenTask									!(Task a) !(AccuFun a acc)			& iTask a
/**
* A control task is a special task used to control and view the other tasks and their manager properties.
* It can generate control signals, after which it will re-incarnate itself.
*
* @param The control view enabling to view the current state of the accumulator and the properties of all subtasks and change manager properties of detached tasks
* @return A list of control signals
*/
:: ControlTaskContainer acc	= DetachedCTask	!ManagerProperties !ActionMenu	!(CTask acc)
							| WindowCTask	!WindowTitle !ActionMenu		!(CTask acc)
							| DialogCTask	!WindowTitle					!(CTask acc)
							| InBodyCTask									!(CTask acc)
							| HiddenCTask									!(CTask acc)
:: CTask acc :== (Shared (!acc,![ParallelTaskInfo]) [(!TaskIndex,!ManagerProperties)]) -> Task [Control acc]

/**
* All-in-one swiss-army-knife parallel task creation
*
* @param The (overloaded) task description
* @param The accumulator
* @param A function defining how to convert the accumulator to the final result when the parallel task finishes
* @param Layout merge function that layouts the user interfaces of tasks that are placed in the body
* @param The list of Control tasks to run in parallel, each task is given a read-only view on the status of all tasks in the set
* @param The list of ordinary tasks to run in parallel
* @return The resulting value
*/
parallel :: !d !pState !(ResultFun pState pResult) ![ControlTaskContainer pState] ![TaskContainer pState] -> Task pResult | iTask pState & iTask pResult & descr d

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
spawnProcess	:: !Bool !ManagerProperties !ActionMenu !(Task a) -> Task (!ProcessId,!SharedProc,!SharedProcResult a) | iTask a

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
