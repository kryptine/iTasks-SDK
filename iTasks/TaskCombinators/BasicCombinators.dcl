definition module BasicCombinators

// *********************************************************************************************************************************
// The iTasks library enables the specification of interactive multi-user workflow tasks (iTask) for the web.
// This is the kernel module for end users which contains the basic iTasks combinators.
// *********************************************************************************************************************************
// iTask & iData Concept and Implementation: (c) 2006,2007,2008 - Rinus Plasmeijer
// *********************************************************************************************************************************
//

import iTasksTypes

/*
Standard monadic combinators on iTasks:
(=>>)			:: for sequencing: bind
return_V		:: lift a value to the iTask domain and return it
*/
(=>>) infixl 1 	:: !(Task a) !(a -> Task b) 				-> Task b		| iCreateAndPrint b
return_V 		:: !a 										-> Task a 		| iCreateAndPrint a

/*
Assign tasks to user with indicated id:
assignTaskTo 	:: assign task to indicated user
*/
assignTaskTo 	:: !UserId !(LabeledTask a) 				-> Task a		| iData a	

/*
Repetition and loops:
foreverTask		:: infinitely repeating Task
(<!)			:: repeat task (as a loop)   as long as predicate does not hold; also works for tasks that don't require any user interactions (e.g. database access)
*/
foreverTask		:: !(Task a) 								-> Task a 		| iData a
(<!)  infixl 6 	:: !(Task a)  !(a -> .Bool) 				-> Task a 		| iCreateAndPrint a

/*
Sequencing Tasks:
seqTasks		:: do all iTasks one after another, task completed when all done
*/
seqTasks		:: ![LabeledTask a] 						-> Task [a]		| iCreateAndPrint a

/*
selectTasks		:: Select n tasks to do out of m (n <= m) and do them in indicated order (either sequential or interleaved)
*/
:: SelectingTask a 	:== [LabeledTask a] -> Task [Int]	// task selecting which tasks to do
:: OrderingTask a	:== [LabeledTask a] -> Task [a]		// task determining in which order the selected tasks are done (can be interleaved or sequential)

selectTasks 	:: !(SelectingTask a) !(OrderingTask a) ![LabeledTask a] -> Task [a] | iData a

/*
Execute all Tasks in parallel / interleaved and FINISH as soon as the predicate holds for the tasks which are finished:
allTasksCond	:: 	- string is used to give a useful name to the trace;
					- the predicate is applied on the set of tasks which are finished
*/
:: DisplaySubTasks	:== !String !TaskNr ![(Bool,HtmlTree)] -> HtmlTree 				// function determining how to display the subtasks
:: FinishPred a		:== [a] -> Bool													// predicate determining when the task is finished, depending on the results returned by finished subtasks

allTasksCond 	:: !String !DisplaySubTasks !(FinishPred a) ![LabeledTask a] -> Task [a] | iData a 

displayAsTab 	:: DisplaySubTasks													// show each sub tasks in a tab
displayAll 		:: DisplaySubTasks

/* Support for user defined combinators
newTask			:: lifts a (user defined) task to an abstract unit: after completion of a (complicated task) only i's final result will be remembered
Once			:: task will be done only once, the value of the task will be remembered, important for side effecting functions lifted to iData domain
*/
newTask 		:: !String !(Task a) 						-> Task a		| iData a 
Once 			:: !String !(Task a) 						-> Task a 		| iData a

newTaskTrace 	:: !String !(Task a) 						-> (Task a) 	| iData a
/* Operations on Task state
taskId			:: give id of user assigned to task
userId			:: give id of application user
*/
//taskId			:: TSt 				-> (Int,TSt)
//userId 			:: TSt 				-> (Int,TSt)

/* Experimental department:

   May not work when the tasks are garbage collected !!

-!>				:: a task, either finished or interrupted (by completion of the first task) is returned in the closure
				   if interrupted, the work done so far is returned (!) which can be continued somewhere else
channel			:: splits a task in respectively a sender task closure and receiver taskclosure; 
				   when the sender is evaluated, the original task is evaluated as usual;
				   when the receiver task is evaluated, it will wait upon completeion of the sender and then get's its result;
				   Important: Notice that a receiver will never finish if you don't activate the corresponding receiver somewhere.
*/

(-!>) infix 4 	:: (Task stop) (Task a) 					-> Task (Maybe stop,TCl a) 	| iCreateAndPrint stop & iCreateAndPrint a
channel  		:: String (Task a) 							-> Task (TCl a,TCl a) 		| iCreateAndPrint a

closureTask  	:: (LabeledTask a) -> (Task (TCl a)) | iCreateAndPrint a
closureLzTask  	:: (LabeledTask a) -> (Task (TCl a)) | iCreateAndPrint a
