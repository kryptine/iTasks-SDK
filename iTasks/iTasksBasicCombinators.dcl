definition module iTasksBasicCombinators

// *********************************************************************************************************************************
// This module contains the basic iTasks combinators
// *********************************************************************************************************************************
// iTask & iData Concept and Implementation: (c) 2006,2007,2008 - Rinus Plasmeijer
// *********************************************************************************************************************************
//
import iTasksHandler

:: TCl a 			= 	TCl !.(Task a)									// task closure, container for a task used for higher order tasks (task which deliver a task)			
:: ChoiceUpdate		:== !Bool [Bool] -> [Bool]							// changed checkbox + current settings -> new settings

derive gForm 	TCl						
derive gUpd 	TCl
derive gPrint 	TCl
derive gParse 	TCl
derive read 	TCl
derive write 	TCl


/*
Standard monadic combinators on iTasks:
(=>>)			:: for sequencing: bind
return_V		:: lift a value to the iTask domain and return it
*/
(=>>) infixl 1 	:: !(Task a) !(a -> Task b) 				-> Task b		| iCreateAndPrint b
return_V 		:: !a 										-> Task a 		| iCreateAndPrint a

/*
Assign tasks to user with indicated id:
assignTaskTo 	:: assign task to indicated user, True for verbose reporting
*/
assignTaskTo 	:: !Bool !UserId !(LabeledTask a) 			-> Task a		| iData a	

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
Choose the tasks you want to do one forehand:
chooseTask_btn	:: choose ONE task by pressing a button, True for horizontal buttons, else vertical
chooseTask_pdm	:: as chooseTask_btn, depending on pulldownmenu item selected, Int for initial value
chooseTask_radio:: as chooseTask_btn, depending on radio item selected, Int for initial value, htmlcode for option explanation 

chooseTask_cb	:: choice N tasks out of N, order of chosen task depending on first arg
				   (initial setting, effect for all when set, explanation) for each option
*/

chooseTask_btn 	:: !HtmlCode !Bool![LabeledTask a] 			-> Task a	 	| iCreateAndPrint a
chooseTask_pdm 	:: !HtmlCode !Int ![LabeledTask a] 			-> Task a	 	| iCreateAndPrint a
chooseTask_radio:: !HtmlCode !Int ![(HtmlCode,LabeledTask a)]

															-> Task a		| iCreateAndPrint a
chooseTask_cbox	:: !([LabeledTask a] -> Task [a])
				   !HtmlCode ![((!Bool,!ChoiceUpdate,!HtmlCode),LabeledTask a)] 
															-> Task [a] 	| iData a

/*
Do m Tasks parallel / interleaved and FINISH as soon as SOME Task completes:
orTask2			:: do both iTasks in any order, combined task completed as any subtask is done
andTask2		:: do both iTasks in any order (interleaved), task completed when both done
andTasksCond	:: do tasks in any order until pred holds for finished tasks, string used for naming group of task navigation buttons
*/
orTask2			:: !(Task a,Task b) 						-> Task (EITHER a b) 	
																			| iCreateAndPrint a & iCreateAndPrint b
andTask2		:: !(Task a,Task b) 						-> Task (a,b) 	| iCreateAndPrint a & iCreateAndPrint b
andTasksCond	:: !String !([a] -> Bool) ![LabeledTask a] -> (Task [a]) 	| iData a 

/* Support for user defined combinators
newTask			:: same, but optimized: after completion only result will remembered
Once			:: task will be done only once, the value of the task will be remembered, important for side effecting functions lifted to iData domain
*/
newTask 		:: !String !(Task a) 						-> Task a		| iData a 
Once 			:: !String !(Task a) 						-> Task a 		| iData a

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

