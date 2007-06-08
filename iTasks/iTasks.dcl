definition module iTasks

// iTasks library for defining interactive multi-user workflow tasks (iTask) for the web
// defined on top of the iData library

// (c) iTask & iData Concept and Implementation by Rinus Plasmeijer, 2006,2007 - MJP
// Version 1.0 - april 2007 - MJP
// This library is still under construction - MJP

import iDataSettings, iDataButtons

derive gForm 	Void						
derive gUpd 	Void, TCl
derive gPrint 	Void, TCl
derive gParse 	Void
derive gerda 	Void

:: *TSt										// task state
:: Task a		:== St *TSt a				// an interactive task
:: Void 		= Void						// for tasks returning non interesting results, won't show up in editors either

/* Initiating the iTask library: to be used with an iData server wrapper!
startTask		:: start iTasks beginning with user with given id, True if trace allowed
				 	id < 0	: for login purposes.						
startNewTask	:: same, lifted to iTask domain, use it after a login ritual						
singleUserTask 	:: start wrapper function for single user 
multiUserTask 	:: start wrapper function for user with indicated id with option to switch between [0..users - 1]  
multiUserTask2 	:: same, but forces an automatic update request every (n minutes, m seconds)  
*/

startTask 		:: !Int !Bool !(Task a) 	!*HSt -> (a,[BodyTag],!*HSt) 	| iCreate a
startNewTask 	:: !Int !Bool !(Task a) 		  -> Task a 				| iCreateAndPrint a 

singleUserTask 	:: !Int	!Bool !(Task a) 	!*HSt -> (Html,*HSt) 			| iCreate a
multiUserTask 	:: !Int !Bool !(Task a)  	!*HSt -> (Html,*HSt) 			| iCreate a
multiUserTask2  :: !(!Int,!Int) !Int !Bool !(Task a) !*HSt -> (Html,*HSt) 	| iCreate a 

/* Setting options for any collection of iTask workflows
(<<@)			:: set iData attribute globally for indicated (composition of) iTask(s) 
*/
class (<<@) infixl 3 b :: (Task a) b -> Task a 

:: GarbageCollect =	Collect | NoCollect

instance <<@		Lifespan				// default: Session
				, 	StorageFormat			// default: PlainString
				, 	Mode					// default: Edit
				, 	GarbageCollect			// deafult: Collect

defaultUser		:== 0						// default id of user

// Here follow the iTasks combinators:

/* promote any iData editor to the iTask domain
editTask		:: create a task editor to edit a value of given type, and add a button with given name to finish the task
*/
editTask 		:: String a 				-> Task a				| iData a 

/* standard monadic combinators on iTasks
(=>>)			:: for sequencing: bind
(#>>)			:: for sequencing: bind, but no argument passed
return_V		:: lift a value to the iTask domain and return it
*/

(=>>) infix  1 	:: (Task a) (a -> Task b)			-> Task b		| iCreateAndPrint b
(#>>) infixl 1 	:: (Task a)      (Task b)			-> Task b
return_V 		:: b 								-> Task b		| iCreateAndPrint b

/* prompting variants
(?>>)			:: prompt as long as task is active but not finished
(!>>)			:: prompt when task is activated
(<|)			:: repeat task (from scratch) as long as predicate does not hold, and give error message otherwise
return_VF		:: return the value and show the Html code specified
return_D		:: return the value and show it in iData display format
*/

(?>>) infix  5 	:: [BodyTag] (Task a) 				-> Task a		| iCreate a
(!>>) infix  5 	:: [BodyTag] (Task a) 				-> Task a		| iCreate a
(<|)  infix  6 	:: (Task a) (a -> .Bool, a -> [BodyTag]) 
													-> Task a 		| iCreate a
return_VF 		:: a [BodyTag] 		  				-> Task a		| iCreateAndPrint a
return_D		:: a 								-> Task a		| gForm {|*|}, iCreateAndPrint a

/* Assign tasks to user with indicated id
(@:)			:: will prompt who is waiting for task with give name
(@::)			:: same, default task name given
*/
(@:)  infix 3 	:: !(!String,!Int) (Task a)			-> Task a		| iCreateAndPrint a
(@::) infix 3 	::           !Int  (Task a)			-> Task a		| iCreate a

/* Handling recursion and loops
newTask			:: use the to promote a (recursively) defined user function to as task
foreverTask		:: infinitely repeating Task
repeatTask		:: repeat Task until predict is valid
*/
newTask 		:: !String (Task a) 				-> Task a		| iData a 
foreverTask		:: (Task a) 						-> Task a		| iData a
repeatTask_Std	:: (a -> Task a) (a -> Bool) -> a	-> Task a		| iCreateAndPrint a

/*	Sequencing Tasks:
seqTasks		:: do all iTasks one after another, task completed when all done
*/
seqTasks		:: [(String,Task a)] 	-> Task [a]					| iCreateAndPrint a

/* Choose Tasks
buttonTask		:: Choose the iTask when button pressed
chooseTask		:: Choose one iTask from list, depending on button pressed, button horizontal displayed
chooseTaskV		:: Choose one iTask from list, depending on button pressed, buttons vertical displayed
chooseTask_pdm	:: Choose one iTask from list, depending on pulldownmenu item selected
mchoiceTask		:: Multiple Choice of iTasks, depending on marked checkboxes
*/
buttonTask		::   String (Task a)	-> Task  a 					| iCreateAndPrint a
chooseTask		:: [(String, Task a)] 	-> Task  a 					| iCreateAndPrint a
chooseTaskV 	:: [(String, Task a)] 	-> Task  a 					| iCreateAndPrint a
chooseTask_pdm 	:: [(String, Task a)] 	-> Task  a		 			| iCreateAndPrint a
mchoiceTasks 	:: [(String, Task a)] 	-> Task [a]					| iCreateAndPrint a

/* Do m Tasks parallel / interleaved and FINISH as soon as SOME Task completes:
orTask			:: do both iTasks in any order, task completed and ends as soon as first one done
(-||-)			:: same, now as infix combinator
orTask2			:: do both iTasks in any order, task completed and ends as soon as first one done
orTasks			:: do all  iTasks in any order, task completed and ends as soon as first one done
*/
orTask 			:: (Task a,Task a) 		-> Task a 					| iCreateAndPrint a
(-||-) infixr 3 :: (Task a) (Task a) 	-> Task a 					| iCreateAndPrint a
orTask2			:: (Task a,Task b) 		-> Task (EITHER a b)	 	| iCreateAndPrint a & iCreateAndPrint b
orTasks			:: [(String,Task a)] 	-> Task a					| iData a 

/* Do Tasks parallel / interleaved and FINISH when ALL Tasks done:
andTask			:: do both iTasks in any order (interleaved), task completed when both done
(-&&-)			:: same, now as infix combinator
andTasks		:: do all  iTasks in any order (interleaved), task completed when all  done
andTasks_mu		:: assign task to indicated users, task completed when all done
*/
andTask			:: (Task a,Task b) 		-> Task (a,b) 				| iCreateAndPrint a & iCreateAndPrint b
(-&&-) infixr 4 :: (Task a) (Task b) 	-> Task (a,b) 				| iCreateAndPrint a & iCreateAndPrint b
andTasks		:: [(String,Task a)]	-> Task [a]					| iCreateAndPrint a
andTasks_mu 	:: String [(Int,Task a)]-> Task [a] 				| iData a


/* Time and Date management:
waitForTimeTask	:: Task is done when time has come
waitForTimerTask:: Task is done when specified amount of time has passed 
waitForDateTask	:: Task is done when date has come
*/
waitForTimeTask	:: HtmlTime				-> Task HtmlTime
waitForTimerTask:: HtmlTime				-> Task HtmlTime
waitForDateTask	:: HtmlDate				-> Task HtmlDate

/* Experimental department
   Will not work when the tasks are garbage collected to soon !!
-!>				:: a task, either finished or interrupted (by completion of the first task) is returned in the closure
				   if interrupted, the work done so far is returned (!) which can be continued somewhere else
channel			:: splits a task in respectively a sender task closure and receiver taskclosure; 
				   when the sender is evaluated, the original task is evaluated as usual;
				   when the receiver task is evaluated, it will wait upon completeion of the sender and then get's its result;
				   Important: Notice that a receiver will never finish if you don't activate the corresponding receiver somewhere.
closureTask		:: The task is executed as usual, but a receiver closure is returned immediately.
				   When the closure is evaluated somewhere, one has to wait until the task is finished.
				   Handy for passing a result to several interested parties.
closureLZTask	:: Same, but now the original task will not be done unless someone is asking for the result somewhere.
*/
:: TCl a 		= TCl (Task a)			

(-!>) infix 4 	:: (Task stop) (Task a) -> Task (Maybe stop,TCl a) 	| iCreateAndPrint stop & iCreateAndPrint a
channel  		:: String (Task a) 		-> Task (TCl a,TCl a) 		| iCreateAndPrint a
closureTask  	:: String (Task a) 		-> Task (TCl a) 			| iCreateAndPrint a
closureLzTask  	:: String (Task a) 		-> Task (TCl a) 			| iCreateAndPrint a

/* Operations on Task state
taskId			:: id assigned to task
userId			:: id of application user
addHtml			:: add html code
*/

taskId			:: TSt -> (Int,TSt)
userId 			:: TSt -> (Int,TSt)
addHtml 		:: [BodyTag] TSt -> TSt

/* Lifting to iTask domain
(*>>)			:: lift functions of type (TSt -> (a,TSt)) to iTask domain 
(@>>)			:: lift functions of (TSt -> TSt) to iTask domain 
appIData		:: lift iData editors to iTask domain
appHSt			:: lift HSt domain to TSt domain, will be executed only once
appHSt2			:: lift HSt domain to TSt domain, will be executed on each invocation
*/
(*>>) infix 4 	:: (TSt -> (a,TSt)) (a -> Task b) 	-> Task b
(@>>) infix 4 	:: (TSt -> TSt) (Task a) 			-> Task a
appIData 		:: (IDataFun a) 					-> Task a 		| iData a
appHSt 			:: (HSt -> (a,HSt)) 				-> Task a		| iData a
appHSt2			:: (HSt -> (a,HSt)) 				-> Task a		| iData a

/* Controlling side effects
Once			:; 	task will be done only once, the value of the task will be remembered
*/

Once 			:: (Task a) 						-> Task a 		| iData a
