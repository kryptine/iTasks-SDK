definition module CoreCombinators
/**
* This is the kernel module for the specification of workflows. It contains the core set of iTasks combinators
* with which additional combinators can be defined.
*/
from Time				import :: Timestamp
from TaskTree			import :: TaskParallelType, :: GroupedBehaviour
from TuningCombinators	import :: Tag
from Shared				import ::Shared
import Task

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

:: PAction x acc	= Stop				// stop the entire parallel/grouped execution
					| Extend ![Task x]	// dynamically extend list of tasks in parallel/group
					| Focus Tag			// focus child-tasks with given tag
					
derive class iTask PAction

// The ValueMerger consists of 
// - an accumulator
// - a function AccuFun which is called whn a ordinary task finishes defining how its result is added to the accumulator
// - a function defining how to convert the accumulator tot the final result when the parallel task finishes
:: ValueMerger taskResult pState pResult :== (!pState, !AccuFun taskResult pState, !ResultFun pState pResult)

/**
* AccuFun is called when an ordinary parallel iTask task (i.e. not a control task) terminates returning a value of type a 
* 
* @param The index of the terminated task 
* @param The value returned by the terminated task
* @param The current value of the accumulator 
* @return Tuple with new value of the accumulator, and possibly an action
*/
:: AccuFun taskResult pState :== TaskIndex taskResult pState -> (!pState, !Maybe (PAction taskResult pState))

// Index in list of parallel executing processes, [0..length list -1]; number of processes can dynamically increase
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


// This tuple is used to link actions to groups, similar to TaskAction.
// Its two parts represent the (what , when) aspects of actions.
// What: The conceptual action to be taken
// When: The condition that determine if the action can be taken
:: GroupAction gState :== (Action, GroupCondition gState)
:: GroupCondition gState
	=	Always														// group action is always enabled
	| 	StatePredicate !(gState -> Bool)							// use predicate on internal state to determine if action is enabled
	| E.s w:
		SharedPredicate !(Shared s w) !(s -> Bool) & iTask s		// use predicate on given shared variable to determine if action is enabled
:: GroupActionGenFunc result	:== (Action, ActionData) -> result	// function mapping task action events to result applied to the group

:: CTask a acc :== (Shared (acc,[TaskProperties]) [ManagerProperties]) -> Task (PAction a acc)

/**
* All-in-one swiss-army-knife parallel task creation
*
* @param The (overloaded) task description
* @param The Value merger: a set of functions defining how subtasks values are accumulated 
* @param Layout merge function that layouts the user interfaces of tasks that are placed in the body
* @param The list of Control tasks to run in parallel, each task is given a read-only view on the status of all tasks in the set
* @param The list of ordinary tasks to run in parallel
* @return The resulting value
*/
parallel :: !TaskParallelType !d !(ValueMerger taskResult pState pResult) ![CTask taskResult pState] ![Task taskResult] -> Task pResult | iTask taskResult & iTask pState & iTask pResult & descr d

/**
* Execute a list of grouped tasks, assigned to the same user. How tasks are combined in the user interface can
* be influenced by assigning a GroupedBehaviour to sub-tasks using the annotation combinator. The group-combinator
* keeps an internal state of type 'gState' and uses the accumulator function to alter this state and dynamically
* add new tasks or stop execution of the entire group using the result of a subtask as soon as it is finished.
*
* @param Label
* @param Description
* @param An accumulator function which alters the internal state
* @param A function which transforms the internal state to the desired output
* @param Initial value of the internal state
* @param List of initial tasks
* @param List of group-actions generating a 'taskResult', makes it possible to change internal state & add tasks without finishing tasks already running
*/
group 	 :: !d !((taskResult,Int) gState -> (gState,Maybe (PAction taskResult gState))) (gState -> gResult) !gState ![Task taskResult] ![GroupAction gState] (GroupActionGenFunc taskResult)	-> Task gResult | iTask taskResult & iTask gState & iTask gResult & descr d

// Multi-user workflows

/**
* Create a new process.
*
* @param Activate the process immediately (False creates the process in a suspended state)
* @param Automatically garbage collect the process when it is finished (removing all references to the state of the process).
* @param The task that is to be started in the new process.
*
* @return A reference to the newly created process
*/
spawnProcess	:: !Bool !Bool !(Task a)	-> Task (ProcessRef a) | iTask a

/**
* Kills a process disregarding any other references to this process.
*
* @param The process reference
*
* @return Void
*/
killProcess 	:: !(ProcessRef a) -> Task Void | iTask a

/**
* Wait (blocking) for a process to complete.
*
* @param A flag indicating if to continue automatically after the process finished
*        or to show the process result and let the user continue.
* @param The process reference
*
* @return A task that maybe gives the result of the process.
*         When a process is prematurely deleted, the task yields Nothing
*         Possibly the user can also cancel the task.
*/
waitForProcess			:: !Bool !(ProcessRef a) -> Task (Maybe a) | iTask a
waitForProcessCancel	:: !Bool !(ProcessRef a) -> Task (Maybe a) | iTask a

/**
* Spawn a process at regular times
*
* @param A function that computes the next time a new instance to be spawned
* @param The task to spawn as process
*
* @return A reference to a control memory this contains a schedulerstate to control the scheduler and a list of active processes.
*/
scheduledSpawn	:: (DateTime -> DateTime) (Task a) -> Task (Shared (SchedulerState,[ProcessRef a]) Void) | iTask a

:: SchedulerState = SSActive //Keep monitoring time and spawn new tasks
				  | SSFinish //Let the already running tasks finish, but don't start new ones anymore
				  | SSCancel //Stop immediately, cancel all active tasks.
