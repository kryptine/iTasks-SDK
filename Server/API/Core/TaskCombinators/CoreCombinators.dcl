definition module CoreCombinators
/**
* This is the kernel module for the specification of workflows. It contains the core set of iTasks combinators
* with which additional combinators can be defined.
*/
from Types 				import :: Task, :: TaskPriority
from Time				import :: Timestamp
from TaskTree			import :: TaskParallelType, :: GroupedBehaviour
from ProcessDB			import :: Action
from TuningCombinators	import :: Tag
from iTasks				import class iTask(..)

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

:: PAction x t	= Stop			// stop the entire parallel/grouped execution
				| Continue		// continue execution without change
				| Extend .[x]	// dynamically extend list of tasks in parallel/group
				| Focus (Tag t) // focus child-tasks with given tag

:: GroupAction taskResult gState shared		= GroupAction Action taskResult (GroupCondition gState shared)					// accept given menu-action for entire group and generate 'taskResult' which is given to accumulator function
											| GroupActionParam String (String -> taskResult) (GroupCondition gState shared)	// accept given parameterized action and use parameter to compute 'taskResult' which is given to accumulator function
:: GroupCondition gState shared				= GroupAlways																	// group action is always enabled
											| StatePredicate (gState -> Bool)												// use predicate on internal state to determine if action is enabled
											| SharedPredicate (DBId shared) ((SharedValue shared) -> Bool)					// use predicate on given shared variable to determine if action is enabled
:: SharedValue shared						= SharedDeleted																	// shared variable was deleted
											| SharedValue shared															// shared variable has stored value

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
parallel :: !TaskParallelType !String !String !((taskResult,Int) pState -> (pState,PAction (Task taskResult) tag))	(pState -> pResult) !pState ![Task taskResult]									-> Task pResult | iTask taskResult & iTask pState & iTask pResult

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
group 	 :: !String !String !((taskResult,Int) gState -> (gState,PAction (Task taskResult) tag)) (gState -> gResult) !gState ![Task taskResult] ![GroupAction taskResult gState shared]	-> Task gResult | iTask taskResult & iTask gState & iTask gResult & iTask shared

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