definition module CommonCombinators
/**
* This module contains a collection of handy iTasks combinators defined in terms of the basic iTask combinators
* with Thanks to Erik Zuurbier for suggesting some of the advanced combinators
*/

import BasicCombinators, iDataWidgets		 

/* Assign tasks to user with indicated id:
(@:)			:: will prompt who is waiting for task with give name
(@::)			:: as @:, a default task name is chosen as label
(@:>)			:: as @:, no prompting
(@::>)			:: as @::, no prompting
*/
(@:)   infix 3 	:: !UserId !(LabeledTask a)					-> Task a		| iData a
(@::)  infix 3 	:: !UserId !(Task a)		    			-> Task a		| iData a
(@:>)  infix 3 	:: !UserId !(LabeledTask a)					-> Task a		| iData a
(@::>) infix 3 	:: !UserId !(Task a)		    			-> Task a		| iData a


/* Handling recursion and loops:
repeatTask		:: repeat Task until predicate is valid
(<|)			:: repeat task (recursively) as long as predicate does not hold, and give error message otherwise
*/
repeatTask		:: !(a -> Task a) !(a -> Bool) a 			-> Task a		| iData a
(<|)  infixl 6 	:: !(Task a)  !(a -> (Bool, [HtmlTag])) 	-> Task a 		| iData a

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
selection :: !([LabeledTask a] -> Task [Int]) !([LabeledTask a] -> Task [a]) ![LabeledTask a] -> Task [a] | iData a
selectTasks :== selection

/*
Choose the tasks you want to do one forehand:
chooseTask_btn	:: choose ONE task by pressing a button, True for horizontal buttons, else vertical
chooseTask_pdm	:: as chooseTask_btn, depending on pulldownmenu item selected, Int for initial value
chooseTask_radio:: as chooseTask_btn, depending on radio item selected, Int for initial value, htmlcode for option explanation 

chooseTask_cb	:: choice N tasks out of N, order of chosen task depending on first arg
				   (initial setting, effect for all when set, explanation) for each option
*/

chooseTask_btn 	:: ![HtmlTag] ![LabeledTask a] 					-> Task a	 	| iData a
chooseTask_pdm 	:: ![HtmlTag] !Int ![LabeledTask a] 			-> Task a	 	| iData a
chooseTask_cbox	:: !([LabeledTask a] -> Task [a]) 
				   ![HtmlTag] ![((!Bool,!(Bool [Bool] -> [Bool]),![HtmlTag]),LabeledTask a)]
																-> Task [a]		| iData a

/* Choose out the tasks you want to do one forehand, labels are used to make the choice:
button 			:: return value when button pressed
buttonTask		:: do the iTask when button pressed
chooseTask		:: Choose ONE iTask from list, depending on button pressed, button horizontal displayed
chooseTaskV		:: as chooseTask, buttons vertical displayed

mchoiceTask		:: Checked tasks will be done SEQUENTIALLY
mchoiceTask2	:: as mchoiceTask, boolean used for initial setting of the checks
mchoiceTask3	:: as mchoiceTask2, function can be used to (re)set the checkboxes

mchoiceTask		:: Checked tasks can be done in INTERLEAVED
mchoiceTask2	:: as mchoiceTask, boolean used for initial setting of the checks
mchoiceTask3	:: as mchoiceTask2, function can be used to (re)set the checkboxes

*/
button 			:: !String 	!a 								-> Task a 		| iData a
buttonTask		:: !String   !(Task a)						-> Task a 		| iData a
chooseTask		:: ![HtmlTag] ![LabeledTask a] 				-> Task a 		| iData a
chooseTaskV 	:: ![HtmlTag] ![LabeledTask a] 				-> Task a 		| iData a

mchoiceTasks 	:: ![HtmlTag] ![LabeledTask a] 				-> Task [a] 	| iData a
mchoiceTasks2 	:: ![HtmlTag] ![(!Bool,LabeledTask a)] 		-> Task [a] 	| iData a
mchoiceTasks3 	:: ![HtmlTag] ![((!Bool,!(Bool [Bool] -> [Bool]),![HtmlTag]),LabeledTask a)] 
															-> Task [a] 	| iData a

mchoiceAndTasks :: ![HtmlTag] ![LabeledTask a] 				-> Task [a]		| iData a
mchoiceAndTasks2:: ![HtmlTag] ![(!Bool,LabeledTask a)] 		-> Task [a] 	| iData a
mchoiceAndTasks3 :: ![HtmlTag] ![((!Bool,!(Bool [Bool] -> [Bool]),![HtmlTag]),LabeledTask a)] 
															-> Task [a] 	| iData a
/* Do m Tasks parallel / interleaved and FINISH as soon as SOME Task completes:
(-||-)			:: do both iTasks in any order, combined task completed as soon as any subtask is done
(-&&-)			:: do both iTasks in any order (interleaved), task completed when both done
orTasks			:: do all  iTasks in any order (interleaved), task completed as soon as any subtask is done
orTasksV		:: same as orTask, but display task vertical
orTask2			:: do both iTasks in any order, combined task completed as any subtask is done
andTasks		:: do all  iTasks in any order (interleaved), task completed when all  done
andTask2		:: do both iTasks in any order (interleaved), task completed when both done
andTasks_mu		:: assign task to indicated users, task completed when all done
andTasksCond 	:: do all  iTasks in any order (interleaved), task completed when predicate holds for finished tasks 
*/
(-||-) infixr 3 :: !(Task a) !(Task a) 						-> Task a 		| iData a
(-&&-) infixr 4 :: !(Task a) !(Task b) 						-> Task (a,b) 	| iData a & iData b
orTasks 		:: ![LabeledTask a] 						-> Task a 		| iData a
orTasksV		:: ![LabeledTask a] 						-> Task a 		| iData a
orTask2			:: !(Task a,Task b) 						-> Task (EITHER a b) 
																			| iData a & iData b	
andTasks		:: ![LabeledTask a]							-> Task [a]		| iData a
andTask2		:: !(Task a,Task b) 						-> Task (a,b) 	| iData a & iData b
andTasks_mu 	:: !String ![(Int,Task a)]					-> Task [a] 	| iData a
andTasksCond 	:: !String !([a] -> Bool) ![LabeledTask a]	-> Task [a] 	| iData a 

/* convenient combinators for tasks that maybe return a result:
(=>>?)			:: as bind, but do the second task only if the first one delivers a result 
(-&&-?)			:: do both tasks in any order, task completed when all done, or one of them delivers nothing
*/
(=>>?) infixl 1 :: !(Task (Maybe a)) !(a -> Task (Maybe b)) -> Task (Maybe b) 		| iData a & iData b
(-&&-?)infixr 4 :: !(Task (Maybe a)) !(Task (Maybe b)) 		-> Task (Maybe (a,b)) 	| iData a & iData b


/* Time and Date management:
waitForTimerTask:: Task is done when specified amount of time has passed 
*/
waitForTimerTask:: !HtmlTime								-> Task HtmlTime
