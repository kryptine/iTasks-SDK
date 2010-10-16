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

:: PAction x	= Stop			// stop the entire parallel/grouped execution
				| Continue		// continue execution without change
				| Extend .[x]	// dynamically extend list of tasks in parallel/group
				| Focus Tag		// focus child-tasks with given tag

// This tuple is used to link actions to groups, similar to TaskAction.
// Its two parts represent the (what , when) aspects of actions.
// What: The conceptual action to be taken
// When: The condition that determine if the action can be taken
:: GroupAction gState			:== (Action, GroupCondition gState)
:: GroupCondition gState		=			Always																	// group action is always enabled
								| 			StatePredicate !(gState -> Bool)										// use predicate on internal state to determine if action is enabled
								| E.shared:	SharedPredicate !(DBId shared) !((Maybe shared) -> Bool) & iTask shared	// use predicate on given shared variable to determine if action is enabled
:: GroupActionGenFunc result	:== (Action, ActionData) -> result															// function mapping task action events to result applied to the group
/**
* Execute a list of parallel tasks, assigned to different users. The combinator keeps an internal
* state of type 'pState' and uses the accumulator function to alter this state and dynamically add new tasks
* or stop execution of the entire parallel using the result of a subtask as soon as it is finished.
*
* @param Type of the parallel, defines who is allowed to see the status of the parallel
* @param Label
* @param Description
* @param An accumulator function which alters the internal state
* @param A function which transforms the internal state to the desired output
* @param Initial value of the internal state
* @param List of initial tasks
*/
parallel :: !TaskParallelType !String !String !((taskResult,Int) pState -> (pState,PAction (Task taskResult)))	(pState -> pResult) !pState ![Task taskResult]									-> Task pResult | iTask taskResult & iTask pState & iTask pResult

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
group 	 :: !String !String !((taskResult,Int) gState -> (gState,PAction (Task taskResult))) (gState -> gResult) !gState ![Task taskResult] ![GroupAction gState] !(GroupActionGenFunc taskResult)	-> Task gResult | iTask taskResult & iTask gState & iTask gResult

// Multi-user workflows

/**
* Assign a task to a(nother) user.
*
* @param The initial UserId of the user to which the task is delegated
* @param The task that is to be delegated.
*
* @return The combined task
*/ 
assign :: !User !(Task a) -> Task a	| iTask a

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
