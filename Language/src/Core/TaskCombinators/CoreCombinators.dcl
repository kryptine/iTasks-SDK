definition module CoreCombinators
/**
* This is the kernel module for the specification of workflows. It contains the core set of iTasks combinators
* with which additional combinators can be defined.
*/
from Types 			import :: Task, :: TaskPriority
from Time			import :: Timestamp
from TaskTree		import :: TaskParallelType, :: GroupedBehaviour

from	iTasks		import class iTask(..)

import	GenPrint, GenParse, GenVisualize, GenUpdate

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

// Parallel composition

/**
* Execute a list of tasks in parallel. The parameters define how the tasks are combined in the
* user interface and when the combined task is finished. The combinator keeps an internal state of type 'b'
* and uses the accumulator function to alter this state using the result of a subtask as soon as it is finished.
*
* @param Label
* @param An accumulator function which alters the internal state
* @param A function which transforms the internal state to the desired output
* @param Initial value of the internal state
* @param List of initial tasks
*/
:: AssignedTask a =
	{ user :: UserName
	, task :: Task a
	}
	
:: PAction x = Stop | Continue | Extend .[x]
//:: PAction t a = Stop | Continue | Extend .[t a]

class PActionClass t where
	getName :: (t a) -> Maybe UserName
	getTask :: (t a) -> Task a

instance PActionClass AssignedTask
instance PActionClass Task

//parallel :: !TaskParallelType !String !String !((a,Int) b -> (b,PAction AssignedTask a)) (b -> c) !b ![AssignedTask a] -> Task c | iTask a & iTask b & iTask c
parallel :: !TaskParallelType !String !String !((a,Int) b -> (b,PAction (AssignedTask a))) (b -> c) !b ![AssignedTask a] -> Task c | iTask a & iTask b & iTask c
//group 	 :: 				  !String !String !((a,Int) b -> (b,PAction Task a)) 		 (b -> c) !b ![Task a] 		   -> Task c | iTask a & iTask b & iTask c
group 	 :: 				  !String !String !((a,Int) b -> (b,PAction (Task a))) 		 (b -> c) !b ![Task a] 		   -> Task c | iTask a & iTask b & iTask c

// Multi-user workflows

/**
* Assign a task to a(nother) user.
*
* @param The initial UserId of the user to which the task is delegated
* @param The initial priority of the task.
* @param The optional initial deadline of the task.
* @param The task that is to be delegated.
* @return The combined task
*/ 
class assign u :: u !TaskPriority !(Maybe Timestamp) !(Task a) -> Task a	| iTask a
instance assign UserName
instance assign User

//assign 	:: !UserName !TaskPriority !(Maybe Timestamp) !(Task a) -> Task a	| iTask a
