definition module BasicCombinators
/**
* This is the kernel module for the specification of workflows. It contains the core set of iTasks combinators
* with which additional combinators can be defined.
*/
import TSt

//Standard monadic combinators:

/**
* Combines two tasks sequentially. The first task is executed first. When it is finished
* the second task is executed with the result of the first task as parameter.
*
* @param The first task to be executed
* @param The second task, which receives the result of the first task
* @return The combined task
*/
(=>>) infixl 1 	:: !(Task a) !(a -> Task b) 				-> Task b		| iCreateAndPrint b
/**
* Lifts a value to the task domain. The return_V task finishes immediately and yields its parameter
* as result of the task.
*
* @param The value to be returned
* @return A task that will return the value defined by the parameter
*/
return_V 		:: !a 										-> Task a 		| iData a

//Repetition and loops:

/**
* Repeats a task infinitely. As soon as the task is finished, it is restarted immediately.
* As a consequence, the combined task never finishes.
*
* @param The task that has to be repeated infinitely
* @return The combined task
*/
foreverTask		:: !(Task a) 								-> Task a 		| iData a
/**
* Repeats a task as long as a given predicate holds. The predicate is tested as soon as the
* given task is finished. When it does not hold, the task is restarted.
*
* @param The task to be looped
* @param The predicate over the result of the task to determine if the combination is finished
* @return The combined task
*/
(<!)  infixl 6 	:: !(Task a)  !(a -> .Bool) 				-> Task a 		| iCreateAndPrint a


// Selection:

/**
* Select n tasks to do out of m (n <= m) and execute them in indicated order. First a
* selecting task is executed which determines which tasks have to be done. Then these selected
* tasks are combined by
*
* @param A selecting task that given a list of labeled tasks yields a list of indexes in the list
*        of labeled tasks. The tasks with these indexes will be executed.
* @param A task that combines a list of labeled tasks into a single task
* @return The combined task
*/
selectTasks 	:: !([LabeledTask a] -> Task [Int]) !([LabeledTask a] -> Task [a]) ![LabeledTask a] -> Task [a] | iData a


// Sequential composition

/**
* Execute the list of tasks one after another.
*
* @param The list of tasks to be executed sequentially
* @return The combined task
*/
seqTasks		:: ![LabeledTask a] 						-> Task [a]		| iCreateAndPrint a

// Parallel composition

/**
* Execute a list of tasks in parallel. The parameters define how the tasks are combined in the
* user interface and when the combined task is finished.
*
* @param A name for tracing
* @param A TaskCombination directive which determines how the tasks will be combined in the user interface
* @param A predicate on the list of results of the currently finished tasks which determines if the
*        combination is finished
* @param The list of tasks to be executed in parallel
* @return The combined task
*/
allTasksCond 	:: !String !TaskCombination !([a] -> Bool) ![LabeledTask a] -> Task [a] | iData a 

/**
* Delegate a task to a(nother) user.
*
* @param The UserId of the user to which the task is delegated
* @param The task that is to be delegated.
* @return The combined task
*/ 
assignTaskTo 	:: !UserId !(LabeledTask a) 				-> Task a		| iData a

//SOON TO BE OBSOLETE:

/* Support for user defined combinators
newTask			:: lifts a (user defined) task to an abstract unit: after completion of a (complicated task) only i's final result will be remembered
*/
newTask 		:: !String !(Task a) 						-> Task a		| iData a 
newTaskTrace 	:: !String !(Task a) 						-> Task a 		| iData a







/* Experimental department:

   May not work when the tasks are garbage collected !!

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