definition module iTasks

// iTasks library for defining interactive multi-user workflow tasks (iTask) for the web.
// Defined on top of the iData library.

// (c) iTask & iData Concept and Implementation by Rinus Plasmeijer, 2006,2007 - MJP
// This library is still under construction - MJP

iTaskVersion :== "0.96 - December 2007"

import iDataSettings, iDataButtons, StdBimap
derive gForm 	Void, TCl						
derive gUpd 	Void, TCl
derive gPrint 	Void, TCl
derive gParse 	Void, TCl
derive gerda 	Void
derive read 	Void, TCl
derive write 	Void, TCl

:: *TSt												// abstract task state
:: Task a			:== St *TSt a					// an interactive task

:: Void 			= Void							// for tasks returning non interesting results, won't show up in editors either

defaultUser			:== 0							// default id of user

// *********************************************************************************************************************************
// Setting options for any collection of iTask workflows:

:: GarbageCollect 	= Collect 						// garbage collect iTask administration
					| NoCollect						// no garbage collection

class (<<@) infixl 3 b :: !(Task a) !b -> Task a 		// to set iData attribute globally for indicated (composition of) iTask(s) 

instance <<@		Lifespan						// default: Session
				, 	StorageFormat					// default: PlainString
				, 	Mode							// default: Edit
				, 	GarbageCollect					// default: Collect

:: SubPage			= UseAjax  						// use Ajax technology to update part of a page, only works if Ajax enabled 
					| OnClient 						// use SAPL to update part of a page on the client, only works if Client enabled and Sapl is running...

class 	(@>>) infixl 7 b ::  !b !(Task a)   -> (Task a) | iData a	

instance @>>		SubPage							// default: the *whole* page will be updated when a form has been modified

// *********************************************************************************************************************************
/* Initiate the iTask library with an iData server wrapper such as doHtmlServer in combination with one of the following functions:
					
singleUserTask 	:: iTask start function for defining tasks for one, single user
multiUserTask 	:: iTask start function for multi-users, with option in window to switch between [0..users - 1]  
workFlowTask	:: iTask start function for a real workflow, expects a login task and the actual task
				   a predefined login task is defined in iTaskLogin.dcl				
*/
singleUserTask 	:: ![StartUpOptions] !(Task a) 				!*HSt -> (!Bool,Html,*HSt) 	| iCreate a
multiUserTask 	:: ![StartUpOptions] !(Task a)  			!*HSt -> (!Bool,Html,*HSt) 	| iCreate a
workFlowTask	:: ![StartUpOptions] !(Task (Int,a)) 
									 !((Int,a) -> Task b)	!*HSt -> (!Bool,Html,*HSt) 	| iCreate a 

:: StartUpOptions	= TraceOn | TraceOff				// for single & multiUser: default = TraceOn
					| ThreadStorage Lifespan			// for Ajax: where to store threadinformation: default = TxtFile
					| ShowUsers Int						// for multiUserTask, toggle between given maximum number of users, default: ShowUser 5 
					| VersionCheck | VersionNoCheck		// for single & multiUser: default = VersionNoCheck 

// *********************************************************************************************************************************
// Here follow the iTasks combinators:

/* promote any iData editor to the iTask domain
editTask		:: create a task editor to edit a value of given type, and add a button with given name to finish the task
editTask		:: create a task editor (with submit button) to edit a value of given type, finish only if predicate holds 
*/
editTask 		:: !String 	!a 								-> Task a		| iData a 
editTaskPred 	:: 			!a !(a -> (Bool, [BodyTag]))	-> Task a		| iData a 

/* standard monadic combinators on iTasks:
(=>>)			:: for sequencing: bind
(#>>)			:: for sequencing: bind, but no argument passed
return_V		:: lift a value to the iTask domain and return it
*/

(=>>) infixl 1 	:: !(Task a) !(a -> Task b) 				-> Task b		| iCreateAndPrint b
(#>>) infixl 1 	:: !(Task a) !(Task b) 						-> Task b
return_V 		:: !a 										-> Task a 		| iCreateAndPrint a

/* prompting variants:
(?>>)			:: prompt as long as task is active but not finished
(!>>)			:: prompt when task is activated
(<<?)			:: same as ?>>, except that prompt is displayed *after* task
(<<!)			:: same as !>>, except that prompt is displayed *after* task
(<|)			:: repeat task (recursively) as long as predicate does not hold, and give error message otherwise
(<!)			:: repeat task (as a loop)   as long as predicate does not hold; also works for tasks that don't require any user interactions (e.g. database access)
return_VF		:: return the value and show the Html code specified
return_D		:: return the value and show it in iData display format
*/

(?>>) infixr 5 	:: ![BodyTag] !(Task a) 					-> Task a		| iCreate a
(!>>) infixr 5 	:: ![BodyTag] !(Task a) 					-> Task a		| iCreate a
(<<?) infixl 5 	:: !(Task a) ![BodyTag] 					-> Task a		| iCreate a
(<<!) infixl 5 	:: !(Task a) ![BodyTag] 					-> Task a		| iCreate a
(<|)  infixl 6 	:: !(Task a)  !(a -> (Bool, [BodyTag])) 	-> Task a 		| iCreate a
(<!)  infixl 6 	:: !(Task a)  !(a -> .Bool) 				-> Task a 		| iCreateAndPrint a
return_VF 		:: ![BodyTag] !a 		  					-> Task a		| iCreateAndPrint a
return_D		:: !a 										-> Task a		| gForm {|*|}, iCreateAndPrint a

/* Assign tasks to user with indicated id:
(@:)			:: will prompt who is waiting for task with give name
(@::)			:: same, default task name given
*/
//(@:)  infix 3 	:: !(!String,!Int) (Task a)	-> (Task a)			| iCreateAndPrint a
//(@::) infix 3 	:: !Int (Task a)		    -> (Task a)			| iCreate a

(@:)  infix 3 	:: !(!String,!Int) !(Task a)				-> Task a		| iData a
(@::) infix 3 	:: !Int !(Task a)		    				-> Task a		| iData a
/* Handling recursion and loops:
newTask			:: use the to promote a (recursively) defined user function to as task
foreverTask		:: infinitely repeating Task
repeatTask		:: repeat Task until predict is valid
*/
newTask 		:: !String !(Task a) 						-> Task a		| iData a 
foreverTask		:: !(Task a) 								-> Task a 		| iData a
repeatTask		:: !(a -> Task a) !(a -> Bool) -> a 		-> Task a		| iData a

/*	Sequencing Tasks:
seqTasks		:: do all iTasks one after another, task completed when all done
*/
seqTasks		:: ![(String,Task a)] 						-> Task [a]		| iCreateAndPrint a

/* Choose Tasks:
buttonTask		:: Choose the iTask when button pressed
chooseTask		:: Choose one iTask from list, depending on button pressed, button horizontal displayed
chooseTaskV		:: Choose one iTask from list, depending on button pressed, buttons vertical displayed
chooseTask_pdm	:: Choose one iTask from list, depending on pulldownmenu item selected
mchoiceTask		:: Multiple Choice of iTasks, depending on marked checkboxes
*/
buttonTask		:: !String !(Task a)						-> Task a 		| iCreateAndPrint a
chooseTask		:: ![(String,Task a)] 						-> Task a 		| iCreateAndPrint a
chooseTaskV 	:: ![(String,Task a)] 						-> Task a 		| iCreateAndPrint a
chooseTask_pdm 	:: ![(String,Task a)] 						-> Task a	 	| iCreateAndPrint a
mchoiceTasks 	:: ![(String,Task a)] 						-> Task [a] 	| iCreateAndPrint a

/* Do m Tasks parallel / interleaved and FINISH as soon as SOME Task completes:
orTask			:: do both iTasks in any order, combined task completed as any subtask is done
(-||-)			:: same, now as infix combinator
orTask2			:: do both iTasks in any order, combined task completed as any subtask is done
orTasks			:: do all  iTasks in any order, combined task completed as any subtask is done
*/
orTask 			:: !(Task a,Task a) 						-> Task a 		| iCreateAndPrint a
(-||-) infixr 3 :: !(Task a) !(Task a) 						-> Task a 		| iCreateAndPrint a
orTask2			:: !(Task a,Task b) 						-> Task (EITHER a b) 	
																			| iCreateAndPrint a & iCreateAndPrint b
orTasks			:: ![(String,Task a)] 						-> Task a		| iData a 

/* Do Tasks parallel / interleaved and FINISH when ALL Tasks done:
andTask			:: do both iTasks in any order (interleaved), task completed when both done
(-&&-)			:: same, now as infix combinator
andTasks		:: do all  iTasks in any order (interleaved), task completed when all  done
andTasks_mu		:: assign task to indicated users, task completed when all done
*/
andTask			:: !(Task a,Task b) 						-> Task (a,b) 	| iCreateAndPrint a & iCreateAndPrint b
(-&&-) infixr 4 :: !(Task a) !(Task b) 						-> Task (a,b) 	| iCreateAndPrint a & iCreateAndPrint b
andTasks		:: ![(String,Task a)]						-> Task [a]		| iCreateAndPrint a
andTasks_mu 	:: !String ![(Int,Task a)]					-> Task [a] 	| iData a


/* Time and Date management:
waitForTimeTask	:: Task is done when time has come
waitForTimerTask:: Task is done when specified amount of time has passed 
waitForDateTask	:: Task is done when date has come
*/
waitForTimeTask	:: !HtmlTime								-> Task HtmlTime
waitForTimerTask:: !HtmlTime								-> Task HtmlTime
waitForDateTask	:: !HtmlDate								-> Task HtmlDate

/* Experimental department:
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
:: TCl a 		= TCl .(Task a)			

(-!>) infix 4 	:: (Task stop) (Task a) 					-> Task (Maybe stop,TCl a) 	| iCreateAndPrint stop & iCreateAndPrint a
channel  		:: String (Task a) 							-> Task (TCl a,TCl a) 		| iCreateAndPrint a
closureTask  	:: String (Task a) 							-> Task (TCl a) 			| iCreateAndPrint a
closureLzTask  	:: String (Task a) 							-> Task (TCl a) 			| iCreateAndPrint a

/* Exception Handling:
<^>				:: When exception of type e is Raised in Task a, catch it here and transfer it to type a.
				   Otherwise try a parent handler.
Raise 			:: Raises an exception; the type stored in the dynamic determines the handlers who can catch it
*/
(<^>) infix  1  :: !(e -> a) !(Task a) -> Task a 		| iData   a & TC e			// assigns an exception Handler
Raise 			:: e -> Task a 							| iCreate a & TC e			// rases an exception


/* Lifting to iTask domain
(*>>)			:: lift functions of type (TSt -> (a,TSt)) to iTask domain 
(@>>)			:: lift functions of (TSt -> TSt) to iTask domain 
appIData		:: lift iData editors to iTask domain
appIData2		:: lift iData editors to iTask domain, and pass iDataTasknumber for naming convenience
appHSt			:: lift HSt domain to TSt domain, will be executed only once; string used for tracing
appHSt2			:: lift HSt domain to TSt domain, will be executed on each invocation; string used for tracing
*/
(*>>) infix 4 	:: (TSt -> (a,TSt)) (a -> Task b) 			-> Task b
(*@>) infix 4 	:: (TSt -> TSt) (Task a) 					-> Task a
appIData 		:: (IDataFun a) 							-> Task a 			| iData a
appIData2 		:: (String *HSt -> *(Form a,*HSt)) 			-> Task a			| iData a 
appHSt 			:: !String (HSt -> (a,HSt)) 				-> Task a			| iData a
appHSt2			:: !String (HSt -> (a,HSt)) 				-> Task a			| iData a

/* Controlling side effects
Once			:: 	task will be done only once, the value of the task will be remembered, maybe useful for some lifted iData
*/

Once 			:: (Task a) 								-> Task a 			| iData a

/* Operations on Task state
taskId			:: id assigned to task
userId			:: id of application user
addHtml			:: add html code
*/

taskId			:: TSt 				-> (Int,TSt)
userId 			:: TSt 				-> (Int,TSt)
addHtml 		:: [BodyTag] TSt 	-> TSt

