definition module CoreCombinators
/**
* This is the kernel module for the specification of workflows. It contains the core set of iTasks combinators
* with which additional combinators can be defined.
*/
from Types 				import :: Task, :: TaskPriority
from Time				import :: Timestamp
from TaskTree			import :: TaskParallelType, :: GroupedBehaviour
from InteractionTasks	import :: Action, :: ActionEvent, :: ActionData
from TuningCombinators	import :: Tag
from iTasks				import class iTask(..)
from Types				import :: DateTime

import GenVisualize, GenUpdate

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
* Repeats a task infinitely. As soon as the task is finished, it is restarted immediately.
* As a consequence, the combined task never finishes.
*
* @param The task that has to be repeated infinitely
* @return The combined task
*/
forever		:: !(Task a) 								-> Task a 		| iTask a
/**
* Repeats a task until a given predicate holds. The predicate is tested as soon as the
* given task is finished. When it does not hold, the task is restarted.
*
* @param The task to be looped
* @param The predicate over the result of the task to determine if the combination is finished
* @return The combined task
*/
(<!)  infixl 6 	:: !(Task a)  !(a -> .Bool) 			-> Task a 		| iTask a

/**
* Iterates a task until a given predicate holds. The repetition is initialized using the
* first parameter and continued using the second. The output of each cycle serves as input
* for the next. The predicate is tested as soon as the given task is finished. 
* When it does not hold, the task is restarted.
*
* @param The initial task
* @param The task to be looped
* @param The termination predicate
* @return The combined task
*/
iterateUntil :: !(Task a) !(a -> Task a) !(a -> .Bool) -> Task a | iTask a

// Sequential composition

/**
* Execute the list of tasks one after another.
*
* @param A label for tracing
* @param The list of tasks to be executed sequentially
* @return The combined task
*/
sequence	:: !String ![Task a] 						-> Task [a]		| iTask a


///////////////// BEGIN EXPERIMENTAL PARALLEL DEFINITION /////////////////////////

/**
* All-in-one swiss-army-knife parallel task creation
*
* @param The (overloaded) task description
* @param The Value merger: a set of functions defining how subtasks values are accumulated 
* @param Layout merge function that layouts the user interfaces of tasks that are placed in the body
* @param The list of tasks to run in parallel, each task is given a read-only view on the status of all tasks in the set
* @return The resulting value
*/
parallel ::	!d !(ValueMerger a acc b) !LayoutMerger ![PTask a acc] -> Task b | iTask a, acc, b & descr d 

// Task Description - to be defined elsewhere
class descr d
	where	toDescr :: d -> TaskDescription d 

:: TaskDescription d	=	{ longDescription	:: !String
							, subject			:: !d
							}
								
// The accumulator, a function defining how a new result is added to the accumulator, a function defining how to convert acc tot the final result
:: ValueMerger a acc b	:== (!acc, !AccuFun a acc, !ResultFun acc b)

/**
* AccuFun is called when an ordinary parallel iTask task (i.e. not a control task) terminates returning a value of type a 
* 
* @param The index of the terminated task 
* @param The value returned by the terminated task
* @param The current value of the accumulator 
* @return Tuple with new value of the accumulator, and possibly an action
*/
:: AccuFun a acc		:== !Index !a !acc -> (!acc, !Maybe (PAction a acc))

// Index in list of parallel executing processes, [0..length list -1]; number of processes can dynamically increase
:: Index				:== Int

// ControlTasks can perform the following actions:					
:: PAction a acc		= StopParallel											// stop the entire parallel execution
						| ExtendParallel [PTask a acc]							// add additional parallel tasks
						| StopTasks [Index]										// kill tasks with indicated id
						| SuspendTask [Index]									// suspend tasks with indicated id
						| ResumeTask [Index]									// resume tasks with indicated id
						| ResetTask [Index]										// start tasks with indicated id from scratch
						| UpdateProperties [(Index, Properties -> Properties)]	// change properties of indicated tasks
						| ReplaceTasks [(Index, Task a)]						// replace indicated tasks by new ones
						| Focus Index											// set the window focus of indicated task 

/**
* ResultFun  is called when the parallel task is stopped
* 
* @param The termination status: why was the parallel task ended
* @param The current value of the accumulator 
* @return The resulting value of type b
*/
:: ResultFun acc b		:== TerminationStatus acc -> b							
:: TerminationStatus	=	AllRunToCompletion	// all parallel processes have ended their execution
						|	Stopped				// the control signal StopParallel has been commited

// A PTask can be an ordinary iTask Task or it can be a ControlTask
:: PTask a acc			=	ResultTask  (Task a)
						|	ControlTask (CTask a acc)
						
/**
* CTask is a special task
* 
* @param The ControlView enabling to view the current state of the accumulator and the properties of all subtasks
* @param The current value of the accumulator 
* @return The action to do
*/
:: CTask a acc			:== ControlView acc -> Task (PAction a acc)

// A ControlView is a read-only DBId storage
:: ControlView acc		:== ListenerDBId (acc, [TaskProperties])
:: ListenerDBId	a		:==	DBId a

// Layout combination of tasks
// typically horizontal or vertical, no yet defined in TUIdefinition.dcl !
:: LayoutMerger	:== [TUIDefinition] -> TUIDefinition

///////////////// END EXPERIMENTAL PARALLEL DEFINITION /////////////////////////

///////////////// BEGIN EXPERIMENTAL ASSIGN DEFINITION /////////////////////////

/**
* Assign a new property to a task thus creating a new Main Task
*
* @param The property of the task
* @param The task 
*
* @return The combined task
*/ 
assign :: !Properties !(Task a) -> Task a	| iTask a

:: Properties 			= NewMainTask TaskProperties ActionMenuSink		// task in tab 
						| WindowTask Title ActionMenu 					// task in window 							
						| DialogTask Title								// task as dialogue

// Task properties to be assigned by the programmer

:: TaskProperties =
	{ worker			:: !User							// Who has to do the task? 
	, subject			:: !String 							// The subject of the task
	, description		:: !String							// Description of the task (html)
	, context			:: !Maybe String					// Optional context information for doing the task (html)
	, priority			:: !TaskPriority					// What is the current priority of this task?
	, deadline			:: !Deadline						// When is the task due?
	, tags				:: ![String]						// A list of tags ??????? STIL NEEDED ??
	}
:: TaskPriority			= HighPriority						// tasks can have three levels of priority
						| NormalPriority
						| LowPriority
:: Deadline =
	{ whenToStart		:: !DateTime						// When should the worker really start doing something
	, whenToFinish		:: !DateTime						// When should the task be done
	}

:: ActionMenuSink		:== MenuAction -> MenuDefinition	// Define which actions are shown in the menu, others become buttons...   
:: ActionMenu			:== MenuAction -> MenuDefinition	// Define which actions are shown in the menu, others passed upwards...   
:: MenuDefinition		:== [Menu]							// As usual
:: MenuAction 			:== (ActionName, ActionLabel)
:: ActionName			:== !String							// Name used as identifier
:: ActionLabel			:== !String							// Label shown in button or menu


///////////////// END EXPERIMENTAL ASSIGN DEFINITION /////////////////////////


// Multi-user workflows

/**
* Create a new process.
*
* @param The user that will perform processes main task.
* @param Activate the process immediately (False creates the process in a suspended state)
* @param Automatically garbage collect the process when it is finished (removing all references to the state of the process).
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
* @param The process reference
*
* @return A task that maybe gives the result of the process.
*         When a process is prematurely deleted, the task yields Nothing
*/
waitForProcess	:: (ProcessRef a)				-> Task (Maybe a)	| iTask a

/**
* Spawn a process at regular times
*
* @param A function that computes the next time a new instance to be spawned
* @param The task to spawn as process
*
* @return A reference to a control memory this contains a schedulerstate to control the scheduler and a list of active processes.
*/
scheduledSpawn	:: (DateTime -> DateTime) (Task a) -> Task (DBId (SchedulerState,[ProcessRef a])) | iTask a

:: SchedulerState = SSActive //Keep monitoring time and spawn new tasks
				  | SSFinish //Let the already running tasks finish, but don't start new ones anymore
				  | SSCancel //Stop immediately, cancel all active tasks.
