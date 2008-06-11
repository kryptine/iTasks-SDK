definition module iTasksCombinators

// *********************************************************************************************************************************
// This module contains a collection of handy iTasks combinators defined in terms of the basic iTask combinators
// with Thanks to Erik Zuurbier for suggesting some of the advanced combinators
// *********************************************************************************************************************************
// iTask & iData Concept and Implementation: (c) 2006,2007,2008 - Rinus Plasmeijer
// *********************************************************************************************************************************
//
import iTasksBasicCombinators				 

/* standard monadic combinators on iTasks:
(#>>)			:: for sequencing: bind, but no argument passed
return_D		:: return the value and show it in iData display format
return_VF		:: return the value and show the Html code specified
*/
(#>>) infixl 1 	:: !(Task a) !(Task b) 						-> Task b		| iCreateAndPrint b
return_D		:: !a 										-> Task a		| gForm {|*|}, iCreateAndPrint a
return_VF 		:: !HtmlCode !a 		  					-> Task a		| iCreateAndPrint a

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
(<|)  infixl 6 	:: !(Task a)  !(a -> (Bool, HtmlCode)) 		-> Task a 		| iData a

/*
Choose the tasks you want to do one forehand:
chooseTask_btn	:: choose ONE task by pressing a button, True for horizontal buttons, else vertical
chooseTask_pdm	:: as chooseTask_btn, depending on pulldownmenu item selected, Int for initial value
chooseTask_radio:: as chooseTask_btn, depending on radio item selected, Int for initial value, htmlcode for option explanation 

chooseTask_cb	:: choice N tasks out of N, order of chosen task depending on first arg
				   (initial setting, effect for all when set, explanation) for each option
*/

chooseTask_btn 	:: !HtmlCode !Bool![LabeledTask a] 			-> Task a	 	| iData a
chooseTask_pdm 	:: !HtmlCode !Int ![LabeledTask a] 			-> Task a	 	| iData a
chooseTask_radio:: !HtmlCode !Int ![(HtmlCode,LabeledTask a)]

															-> Task a		| iData a
chooseTask_cbox	:: 	!(![LabeledTask a] -> Task [a]) 
					!HtmlCode ![((!Bool,!ChoiceUpdate,!HtmlCode),LabeledTask a)]
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
chooseTask		:: !HtmlCode ![LabeledTask a] 				-> Task a 		| iData a
chooseTaskV 	:: !HtmlCode ![LabeledTask a] 				-> Task a 		| iData a

mchoiceTasks 	:: !HtmlCode ![LabeledTask a] 				-> Task [a] 	| iData a
mchoiceTasks2 	:: !HtmlCode ![(!Bool,LabeledTask a)] 		-> Task [a] 	| iData a
mchoiceTasks3 	:: !HtmlCode ![((!Bool,!ChoiceUpdate,!HtmlCode),LabeledTask a)] 
															-> Task [a] 	| iData a

mchoiceAndTasks :: !HtmlCode ![LabeledTask a] 				-> Task [a]		| iData a
mchoiceAndTasks2:: !HtmlCode ![(!Bool,LabeledTask a)] 		-> Task [a] 	| iData a
mchoiceAndTasks3 :: !HtmlCode ![((!Bool,!ChoiceUpdate,!HtmlCode),LabeledTask a)] 
															-> Task [a] 	| iData a
/* Do m Tasks parallel / interleaved and FINISH as soon as SOME Task completes:
(-||-)			:: do both iTasks in any order, combined task completed as soon as any subtask is done
(-&&-)			:: do both iTasks in any order (interleaved), task completed when both done
orTasks			:: do all  iTasks in any order (interleaved), task completed as soon as any subtask is done
orTask2			:: do both iTasks in any order, combined task completed as any subtask is done
andTasks		:: do all  iTasks in any order (interleaved), task completed when all  done
andTask2		:: do both iTasks in any order (interleaved), task completed when both done
andTasks_mu		:: assign task to indicated users, task completed when all done
andTasksCond 	:: do all  iTasks in any order (interleaved), task completed when predicate holds for finished tasks 
andTasksCond_pdm:: same as andTasksCond, pull down menu used to select the tasks
*/
(-||-) infixr 3 :: !(Task a) !(Task a) 						-> Task a 		| iData a
(-&&-) infixr 4 :: !(Task a) !(Task b) 						-> Task (a,b) 	| iData a & iData b
orTasks 		:: ![LabeledTask a] 						-> Task a 		| iData a
orTask2			:: !(Task a,Task b) 						-> Task (EITHER a b) 
																			| iData a & iData b	
andTasks		:: ![LabeledTask a]							-> Task [a]		| iData a
andTask2		:: !(Task a,Task b) 						-> Task (a,b) 	| iData a & iData b
andTasks_mu 	:: !String ![(Int,Task a)]					-> Task [a] 	| iData a
andTasksCond 	:: !String !([a] -> Bool) ![LabeledTask a]	-> Task [a] 	| iData a 
andTasksCond_pdm:: !String !([a] -> Bool) ![LabeledTask a]	-> Task [a]		| iData a 

/* convenient combinators for tasks that maybe return a result:
(=>>?)			:: as bind, but do the second task only if the first one delivers a result 
(-&&-?)			:: do both tasks in any order, task completed when all done, or one of them delivers nothing
*/
(=>>?) infixl 1 :: !(Task (Maybe a)) !(a -> Task (Maybe b)) -> Task (Maybe b) 		| iCreateAndPrint a & iCreateAndPrint b
(-&&-?)infixr 4 :: !(Task (Maybe a)) !(Task (Maybe b)) 		-> Task (Maybe (a,b)) 	| iData a & iData b


/* Time and Date management:
waitForTimerTask:: Task is done when specified amount of time has passed 
*/
waitForTimerTask:: !HtmlTime								-> Task HtmlTime
