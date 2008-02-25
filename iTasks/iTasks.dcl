definition module iTasks

// iTasks library for defining interactive multi-user workflow tasks (iTask) for the web.
// Defined on top of the iData library.

// (c) iTask & iData Concept and Implementation by Rinus Plasmeijer, 2006,2007 - MJP
// This library is still under construction - MJP

iTaskVersion :== "0.97 - Februari 2008 - "

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
:: LabeledTask a	:== !(!String,!Task a)			// a Task with a label used for labeling buttons, pull down menu, and the like
:: UserId			:== !Int						// unique id of iTask user

:: HtmlCode			:== [BodyTag]					// most programmers will only write bodytags

:: Void 			= Void							// for tasks returning non interesting results, won't show up in editors either


// *********************************************************************************************************************************
// Setting options for any collection of iTask workflows:

:: GarbageCollect 	= Collect 						// garbage collect iTask administration
					| NoCollect						// no garbage collection

class (<<@) infixl 3 b :: !(Task a) !b -> Task a 	// to set iData attribute globally for indicated (composition of) iTask(s) 

instance <<@		  Lifespan						// default: Session
					, StorageFormat					// default: PlainString
					, Mode							// default: Edit
					, GarbageCollect				// default: Collect

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
workFlowTask	:: ![StartUpOptions] !(Task (UserId,a)) 
									 !((UserId,a) -> Task b)!*HSt -> (!Bool,Html,*HSt) 	| iCreate a 

:: StartUpOptions	= TraceOn | TraceOff				// for single & multiUser: default = TraceOn
					| ThreadStorage Lifespan			// for Ajax: where to store threadinformation: default = TxtFile
					| ShowUsers Int						// for multiUserTask, toggle between given maximum number of users, default: ShowUser 5 
					| VersionCheck | NoVersionCheck		// for single & multiUser: default = VersionNoCheck 
					| MyHeader HtmlCode					// wil replace standard iTask information line

// *********************************************************************************************************************************
/* Here follow the iTasks combinators:

Basic editors:
editTask		:: create a task editor to edit a value of given type, and add a button with given name to finish the task
editTask		:: create a task editor (with submit button) to edit a value of given type, finish only if predicate holds 

Standard monadic combinators on iTasks:
(=>>)			:: for sequencing: bind
return_V		:: lift a value to the iTask domain and return it

Prompting variants:
(?>>)			:: prompt as long as task is active but not finished
(!>>)			:: prompt when task is activated
(<<?)			:: as ?>>, except that prompt is displayed *after* task
(<<!)			:: as !>>, except that prompt is displayed *after* task

Assign tasks to user with indicated id:
assignTaskTo 	:: assign task to indicated user, True for verbose reporting

Repetition and loops:
foreverTask		:: infinitely repeating Task
(<!)			:: repeat task (as a loop)   as long as predicate does not hold; also works for tasks that don't require any user interactions (e.g. database access)

Sequencing Tasks:
seqTasks		:: do all iTasks one after another, task completed when all done

Choose the tasks you want to do one forehand:
chooseTask_btn	:: choose ONE task by pressing a button, True for horizontal buttons, else vertical
chooseTask_pdm	:: as chooseTask_btn, depending on pulldownmenu item selected, Int for initial value
chooseTask_radio:: as chooseTask_btn, depending on radio item selected, Int for initial value, htmlcode for option explanation 

chooseTask_cb	:: choice N tasks out of N, order of chosen task depending on first arg
				   (initial setting, effect for all when set, explanation) for each option

Do m Tasks parallel / interleaved and FINISH as soon as SOME Task completes:
orTask2			:: do both iTasks in any order, combined task completed as any subtask is done
andTask2		:: do both iTasks in any order (interleaved), task completed when both done
andTasksCond	:: do tasks in any order until pred holds for finished tasks, string used for naming group of task navigation buttons
*/

editTask 		:: !String 	!a 								-> Task a		| iData a 
editTaskPred 	:: 			!a !(a -> (Bool, HtmlCode))		-> Task a		| iData a 

(=>>) infixl 1 	:: !(Task a) !(a -> Task b) 				-> Task b		| iCreateAndPrint b
return_V 		:: !a 										-> Task a 		| iCreateAndPrint a

(?>>) infixr 5 	:: !HtmlCode !(Task a) 						-> Task a		| iCreate a
(!>>) infixr 5 	:: !HtmlCode !(Task a) 						-> Task a		| iCreate a
(<<?) infixl 5 	:: !(Task a) !HtmlCode 						-> Task a		| iCreate a
(<<!) infixl 5 	:: !(Task a) !HtmlCode 						-> Task a		| iCreate a

assignTaskTo 	:: !Bool !UserId !(LabeledTask a) 				-> Task a		| iData a	

foreverTask		:: !(Task a) 								-> Task a 		| iData a
(<!)  infixl 6 	:: !(Task a)  !(a -> .Bool) 				-> Task a 		| iCreateAndPrint a

seqTasks		:: ![LabeledTask a] 						-> Task [a]		| iCreateAndPrint a

chooseTask_btn 	:: !HtmlCode !Bool![LabeledTask a] 			-> Task a	 	| iCreateAndPrint a
chooseTask_pdm 	:: !HtmlCode !Int ![LabeledTask a] 			-> Task a	 	| iCreateAndPrint a
chooseTask_radio:: !HtmlCode !Int ![(HtmlCode,LabeledTask a)]
															-> Task a		| iCreateAndPrint a

:: ChoiceUpdate	:== !Bool [Bool] -> [Bool]									// changed checkbox + current settings -> new settings

chooseTask_cbox	:: !([LabeledTask a] -> Task [a])
				   !HtmlCode ![((!Bool,!ChoiceUpdate,!HtmlCode),LabeledTask a)] 
															-> Task [a] 	| iData a

orTask2			:: !(Task a,Task b) 						-> Task (EITHER a b) 	
																			| iCreateAndPrint a & iCreateAndPrint b
andTask2		:: !(Task a,Task b) 						-> Task (a,b) 	| iCreateAndPrint a & iCreateAndPrint b
andTasksCond	:: !String !([a] -> Bool) ![LabeledTask a] -> (Task [a]) 	| iData a 

/* Time and Date management:
waitForTimeTask	:: Task is done when time has come
waitForDateTask	:: Task is done when date has come
*/
waitForTimeTask	:: !HtmlTime								-> Task HtmlTime
waitForDateTask	:: !HtmlDate								-> Task HtmlDate

/* Experimental department:

   May not work when the tasks are garbage collected !!

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

<^>				:: Evaluate the task; An exception of type e raised by this task, will be catched by the closest handler.
				   One can use the function create a proper task value or signal the fact that an exception has occured.  
Raise 			:: Raises an exception of type e which will be catched by the closest parent handler for this type
*/
(<^>) infix  1  :: !(e -> a) !(Task a) 						-> Task a 					| iData a   & TC e			// assigns an exception Handler
Raise 			:: e 										-> Task a 					| iCreate a & TC e			// rases an exception


// *********************************************************************************************************************************
/* Support for user defined combinators
mkTask			:: for making a user defined combinator, name will appear intrace 
newTask			:: same, but optimized: after completion only result will remembered
Once			:: task will be done only once, the value of the task will be remembered, important for side effecting functions lifted to iData domain
*/
mkTask 			:: !String !(Task a) 						-> Task a 		| iCreateAndPrint a
newTask 		:: !String !(Task a) 						-> Task a		| iData a 
Once 			:: !String !(Task a) 						-> Task a 		| iData a

// *********************************************************************************************************************************
/* Lifting of other domains to the iTask domain
(*>>)			:: lift functions of type (TSt -> (a,TSt)) to iTask domain 
(@>>)			:: lift functions of (TSt -> TSt) to iTask domain 
appIData		:: lift iData editors to iTask domain
appIData2		:: lift iData editors to iTask domain, and pass iDataTasknumber for naming convenience
appHStOnce		:: lift HSt domain to TSt domain, will be executed only once; string used for tracing
appHSt			:: lift HSt domain to TSt domain, will be executed on each invocation; string used for tracing
*/
(*=>) infix 4 	:: (TSt -> (a,TSt)) (a -> Task b) 			-> Task b
(*#>) infix 4 	:: (TSt -> TSt)     (Task a) 				-> Task a
appIData 		:: (IDataFun a) 							-> Task a 		| iData a
appIData2 		:: (String *HSt -> *(Form a,*HSt)) 			-> Task a		| iData a 
appHStOnce 		:: !String (HSt -> (a,HSt)) 				-> Task a		| iData a
appHSt			:: !String (HSt -> (a,HSt)) 				-> Task a		| iData a
appWorldOnce 	:: !String (*World -> *(a,*World)) 			-> Task a		| iData a
appWorld 		:: !String (*World -> *(a,*World)) 			-> Task a		| iData a

// *********************************************************************************************************************************
/* Operations on Task state
taskId			:: give id of user assigned to task
userId			:: give id of application user
addHtml			:: add html code
*/
taskId			:: TSt 				-> (Int,TSt)
userId 			:: TSt 				-> (Int,TSt)
addHtml 		:: HtmlCode TSt 	-> TSt

