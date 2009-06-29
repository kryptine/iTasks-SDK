definition module BasicCombinators
/**
* This is the kernel module for the specification of workflows. It contains the core set of iTasks combinators
* with which additional combinators can be defined.
*/
from TSt 			import :: Task, :: LabeledTask, :: TaskCombination
from Types 			import :: UserId, :: TaskPriority
from iDataSettings	import class iPrint, class iParse, class iCreate, class iCreateAndPrint, class iSpecialStore, class iData
from Time			import :: Time

import iDataForms, GenPrint, GenParse

//Standard monadic operations:

/**
* Combines two tasks sequentially. The first task is executed first. When it is finished
* the second task is executed with the result of the first task as parameter.
*
* @param The first task to be executed
* @param The second task, which receives the result of the first task
* @return The combined task
*/
(>>=) infixl 1 	:: !(Task a) !(a -> Task b) 			-> Task b		| iData a & iData b
/**
* Combines two tasks sequentially just as >>=, but the result of the first task is disregarded.
*
* @param The first task to be executed
* @param The second task to be executed
* @return The combined task
*/
(>>|) infixl 1 :: !(Task a) (Task b)					-> Task b		| iData a & iData b
/**
* Lifts a value to the task domain. The return_V task finishes immediately and yields its parameter
* as result of the task.
*
* @param The value to be returned
* @return A task that will return the value defined by the parameter
*/
return 		:: !a 										-> Task a 		| iData a

//Repetition and loops:

/**
* Repeats a task infinitely. As soon as the task is finished, it is restarted immediately.
* As a consequence, the combined task never finishes.
*
* @param The task that has to be repeated infinitely
* @return The combined task
*/
forever		:: !(Task a) 								-> Task a 		| iData a
/**
* Repeats a task until a given predicate holds. The predicate is tested as soon as the
* given task is finished. When it does not hold, the task is restarted.
*
* @param The task to be looped
* @param The predicate over the result of the task to determine if the combination is finished
* @return The combined task
*/
(<!)  infixl 6 	:: !(Task a)  !(a -> .Bool) 			-> Task a 		| iData a

// Sequential composition

/**
* Execute the list of tasks one after another.
*
* @param A label for tracing
* @param The list of tasks to be executed sequentially
* @return The combined task
*/
sequence	:: !String ![LabeledTask a] 				-> Task [a]		| iData a

/**
* Reduces a multi-step sequence to a single step task
*
* @param A label for tracing
* @param The task that has to be reduced to one step
* @return The combined task
*/
compound 	:: !String !(Task a) 						-> Task a		| iData a 

// Parallel composition

/**
* Execute a list of tasks in parallel. The parameters define how the tasks are combined in the
* user interface and when the combined task is finished.
*
* @param A label for tracing
* @param A predicate on the list of results of the currently finished tasks which determines if the
*        combination is finished
* @param A transformation function used to combine the list of results, boolean indicates why composition stopped (True :predicate True, all finished otherwise)
* @param The list of tasks to be executed in parallel
* @return The combined task
*/
parallel 	:: !String !([a] -> Bool) !(Bool [a] -> b) ![LabeledTask a] -> Task b | iData a & iData b 

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
assign 	:: !UserId !TaskPriority !(Maybe Time) !(LabeledTask a) 		-> Task a	| iData a

/* Experimental department:

-!>				:: a task, either finished or interrupted (by completion of the first task) is returned in the closure
				   if interrupted, the work done so far is returned (!) which can be continued somewhere else
channel			:: splits a task in respectively a sender task closure and receiver taskclosure; 
				   when the sender is evaluated, the original task is evaluated as usual;
				   when the receiver task is evaluated, it will wait upon completeion of the sender and then get's its result;
				   Important: Notice that a receiver will never finish if you don't activate the corresponding receiver somewhere.
*/
/*
(-!>) infix 4 	:: (Task stop) (Task a) 					-> Task (Maybe stop,Task a) 	| iCreateAndPrint stop & iCreateAndPrint a
channel  		:: String (Task a) 							-> Task (Task a,Task a) 		| iCreateAndPrint a

closureTask  	:: (LabeledTask a) -> (Task (Task a)) | iCreateAndPrint a
closureLzTask  	:: (LabeledTask a) -> (Task (Task a)) | iCreateAndPrint a
*/