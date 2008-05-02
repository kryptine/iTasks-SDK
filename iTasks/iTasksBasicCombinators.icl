implementation module iTasksBasicCombinators

// *********************************************************************************************************************************
// This module contains the basic iTasks combinators
// *********************************************************************************************************************************
// iTask & iData Concept and Implementation: (c) 2006,2007,2008 - Rinus Plasmeijer
// *********************************************************************************************************************************
//
import StdList, StdArray, StdTuple, StdFunc
import dynamic_string, graph_to_string_with_descriptors, graph_to_sapl_string
import DrupBasic
import iDataTrivial, iDataFormlib
import iTasksHandler, iTasksLiftingCombinators, iTasksSettings, iTasksEditors
import InternaliTasksThreadHandling, iTasksHtmlSupport

derive gForm 	Maybe, []
derive gUpd 	Maybe, []
derive gPrint	Maybe

:: TCl a 			= 	TCl !.(Task a)									// task closure, container for a task used for higher order tasks (task which deliver a task)			
:: ChoiceUpdate		:== !Bool [Bool] -> [Bool]							// changed checkbox + current settings -> new settings


// ******************************************************************************************************
// monads for combining iTasks

(=>>) infixl 1 :: !(Task a) !(a -> Task b) -> Task b | iCreateAndPrint b
(=>>) taska taskb = mybind
where
	mybind tst=:{options}
	# (a,tst=:{activated}) = taska tst
	| activated	= taskb a {tst & options = options}
	= (createDefault,tst)

return_V :: !a -> (Task a) | iCreateAndPrint a
return_V a  = mkTask "return_V" dotask
where
	dotask tst = (a,tst) 

	
// ******************************************************************************************************
// newTask needed for recursive task creation

newTask :: !String !(Task a) -> (Task a) 	| iData a 
newTask taskname mytask = mkTask taskname newTask`
where
	newTask` tst=:{tasknr,userId,options}		
	# taskId					= iTaskId userId tasknr taskname
	# (taskval,tst) 			= LiftHst (mkStoreForm (Init,storageFormId options taskId (False,createDefault)) id) tst  // remember if the task has been done
	# (taskdone,taskvalue)		= taskval.value										// select values
	| taskdone					= (taskvalue,tst)									// if rewritten return stored value
	# (val,tst=:{activated})	= mytask {tst & tasknr = [-1:tasknr]} 				// do task, first shift tasknr
	| not activated				= (createDefault,{tst & tasknr = tasknr, options = options})	// subtask not ready, return value of subtasks
	# tst						= deleteSubTasksAndThreads tasknr tst				// task ready, garbage collect it
	# (_,tst) 					= LiftHst (mkStoreForm (Init,storageFormId options taskId (False,createDefault)) (\_ -> (True,val))) tst  // remember if the task has been done
	= (val,{tst & tasknr = tasknr, options = options})


Once :: !String !(Task a) -> (Task a) | iData a
Once label task = mkTask label doit
where
	doit tst=:{activated,html,tasknr,hst,userId,options}
	# taskId			= iTaskId userId tasknr (label +++ "_")
	# (store,hst) 		= mkStoreForm (Init,storageFormId options taskId (False,createDefault)) id hst  			
	# (done,value)		= store.value
	| done 				= (value,{tst & hst = hst})													// if task has completed, don't do it again
	# (value,tst=:{hst})= task {tst & hst = hst}
	# (store,hst) 		= mkStoreForm (Init,storageFormId options taskId (False,createDefault)) (\_ -> (True,value)) hst 	// remember task status for next time
	# (done,value)		= store.value
	= (value,{tst & activated = done, hst = hst})													// task is now completed, handle as previously

// ******************************************************************************************************
// looping tasks

// when gc option set and task finished, it will throw away all subtasks and start all over
// otherwise, when task finshed it will remember the new tasknr to prevent checking of previously finished tasks

foreverTask :: !(Task a) -> Task a | iData a
foreverTask task = mkTask "foreverTask" foreverTask`
where
	foreverTask` tst=:{tasknr,activated,userId,options,html} 
	| options.gc == Collect																				// garbace collect everything when task finsihed
		# (val,tst=:{activated})= task {tst & tasknr = [-1:tasknr]}										// shift tasknr
		| activated 			= foreverTask` (deleteSubTasksAndThreads tasknr {tst & tasknr = tasknr, options = options, html = html}) 			// loop
		= (val,tst)					
	# taskId					= iTaskId userId tasknr "ForSt"											// create store id
	# (currtasknr,tst)			= LiftHst (mkStoreForm (Init,storageFormId options taskId tasknr) id) tst		// fetch actual tasknr
	# (val,tst=:{activated})	= task {tst & tasknr = [-1:currtasknr.value]}
	| activated 																						// task is completed	
		# ntasknr				= incNr currtasknr.value												// incr tasknr
		# (currtasknr,tst)		= LiftHst (mkStoreForm (Init,storageFormId options taskId tasknr) (\_ -> ntasknr)) tst // store next task nr
		= foreverTask` {tst & tasknr = tasknr, options = options, html = html}										// initialize new task
	= (val,tst)					

(<!) infixl 6 :: !(Task a) !(a -> .Bool) -> Task a | iCreateAndPrint a
(<!) taska pred = mkTask "less!" doTask
where
	doTask tst=:{activated, tasknr}
	# (a,tst=:{activated}) 	= taska {tst & tasknr = [-1:tasknr]}
	| not activated 		= (a,tst)
	| not (pred a)			
		# tst = deleteSubTasksAndThreads [0:tasknr] tst
		= doTask {tst & tasknr = tasknr}
//		= (a,{tst & activated = False})
	= (a,tst)

// ******************************************************************************************************
// Assigning tasks to users, each user has to be identified by an unique number >= 0

assignTaskTo :: !Bool !UserId !(LabeledTask a) -> Task a | iData a	
assignTaskTo verbose nuserId (taskname,taska) = assignTaskTo`
where
	assignTaskTo` tst=:{html=ohtml,activated,userId,workflowLink=(_,(_,processNr,workflowLabel))}
	| not activated						= (createDefault,tst)
	# tst								= IF_Ajax (administrateNewThread userId tst) tst 
	# (a,tst=:{html=nhtml,activated})	= IF_Ajax (UseAjax @>> taska) taska {tst & html = BT [],userId = nuserId}		// activate task of indicated user
	| activated 						= (a,{tst & activated = True						// work is done	
												  ,	userId = userId							// restore previous user id						
												  ,	html = ohtml })							// plus new one tagged
	= (a,{tst & userId = userId																// restore user Id
			  , html = 	ohtml +|+ 															// show old code
						if verbose 
							( BT [showText ("Waiting for Task "), showLabel taskname, showText " from ", showUser nuserId,Br] +|+  // show waiting for
							  ((nuserId,processNr,workflowLabel,taskname) @@: BT [showText "Requested by ", showUser userId,Br,Br] +|+ nhtml)) 
							((nuserId,processNr,workflowLabel,taskname) @@: nhtml)
		 })												

	showUser nr = showLabel ("User " <+++ nr)


// ******************************************************************************************************
// sequencingtasks

seqTasks :: ![LabeledTask a] -> (Task [a])| iCreateAndPrint a
seqTasks options = mkTask "seqTasks" seqTasks`
where
	seqTasks` tst=:{tasknr}
	# (val,tst)	 = doseqTasks options [] {tst & tasknr = [-1:tasknr]}
	= (val,{tst & tasknr = tasknr})

	doseqTasks [] accu tst 		= (reverse accu,{tst & activated = True})
	doseqTasks [(taskname,task):ts] accu tst=:{html,options} 
	# (a,tst=:{activated=adone,html=ahtml}) 
									= task {tst & activated = True, html = BT []}
	| not adone						= (reverse accu,{tst & html = html +|+ BT [showLabel taskname,Br,Br] +|+ ahtml})
	= doseqTasks ts [a:accu] {tst & html = html +|+ ahtml, options = options}

// ******************************************************************************************************
// choose one or more tasks on forehand out of a set

chooseTask_btn :: !HtmlCode !Bool![LabeledTask a] -> Task a | iCreateAndPrint a
chooseTask_btn htmlcode bool ltasks = chooseTask_btn` htmlcode bool ltasks
where
	chooseTask_btn` _ _ [] tst		= return createDefault tst				
	chooseTask_btn`  prompt horizontal taskOptions tst=:{tasknr,html,options,userId}									// choose one subtask out of the list
	# taskId						= iTaskId userId tasknr ("ChoSt" <+++ length taskOptions)
	# (chosen,tst)					= LiftHst (mkStoreForm  (Init,storageFormId options taskId -1) id) tst
	| chosen.value == -1			// no choice made yet
		# buttonId					= iTaskId userId tasknr "ChoBut"
		# allButtons				= if horizontal 
											[[(but txt,\_ -> n)  \\ txt <- map fst taskOptions & n <- [0..]]]
											[[(but txt,\_ -> n)] \\ txt <- map fst taskOptions & n <- [0..]]
		# (choice,tst)				= LiftHst (TableFuncBut (Init,pageFormId options buttonId allButtons)) tst
		# (chosen,tst)				= LiftHst (mkStoreForm  (Init,storageFormId options taskId -1) choice.value) tst
		| chosen.value == -1		= (createDefault,{tst & activated =False,html = html +|+ BT prompt +|+ BT choice.form})
		# chosenTask				= snd (taskOptions!!chosen.value)
		# (a,tst=:{activated=adone,html=ahtml}) = chosenTask {tst & tasknr = [-1:tasknr], activated = True, html = BT []}
		= (a,{tst & tasknr = tasknr, activated = adone, html = html +|+ ahtml})
	# chosenTask					= snd (taskOptions!!chosen.value)
	# (a,tst=:{activated=adone,html=ahtml}) = chosenTask {tst & tasknr = [-1:tasknr], activated = True, html = BT []}
	= (a,{tst & tasknr = tasknr, activated = adone, html = html +|+ ahtml})

but i = iTaskButton i

chooseTask_pdm :: !HtmlCode !Int ![LabeledTask a] -> (Task a) |iCreateAndPrint a
chooseTask_pdm prompt defaultOn taskOptions = mkTask "chooseTask_pdm" (dochooseTask_pdm taskOptions)
where
	dochooseTask_pdm [] tst			= return createDefault tst	
	dochooseTask_pdm taskOptions tst=:{tasknr,html,userId,options}													// choose one subtask out of  a pulldown menu
	# taskId						= iTaskId userId tasknr ("ChoStPdm" <+++ length taskOptions)
	# (chosen,tst)					= LiftHst (mkStoreForm  (Init,storageFormId options taskId -1) id) tst
	| chosen.value == -1			// no choice made yet
		# numberOfItems					= length taskOptions
		# defaultOn						= if (defaultOn >= 0 && defaultOn <= numberOfItems  - 1) defaultOn 0 		
		# taskPdMenuId					= iTaskId userId tasknr ("ChoPdm" <+++ numberOfItems)
		# (choice,tst)					= LiftHst (FuncMenu  (Init,sessionFormId options taskPdMenuId (defaultOn,[(txt,id) \\ txt <- map fst taskOptions]))) tst
		# (_,tst=:{activated=adone,html=ahtml})	
										= editTaskLabel "" "Done" Void {tst & activated = True, html = BT [], tasknr = [-1:tasknr]} 	
		| not adone						= (createDefault,{tst & activated = False, html = html +|+ BT prompt +|+ BT choice.form +|+ ahtml, tasknr = tasknr})
		# chosenIdx						= snd choice.value
		# chosenTask					= snd (taskOptions!!chosenIdx)
		# (chosen,tst)					= LiftHst (mkStoreForm  (Init,storageFormId options taskId -1) (\_ -> chosenIdx)) tst
		# (a,tst=:{activated=bdone,html=bhtml}) 
										= chosenTask {tst & activated = True, html = BT [], tasknr = [0:tasknr]}
		= (a,{tst & tasknr = tasknr, activated = bdone, html = html +|+ bhtml})
	# chosenTask					= snd (taskOptions!!chosen.value)
	# (a,tst=:{activated=adone,html=ahtml}) 
									= chosenTask {tst & activated = True, html = BT [], tasknr = [0:tasknr]}
	= (a,{tst & activated = adone, html = html +|+ ahtml, tasknr = tasknr})

chooseTask_radio :: !HtmlCode !Int ![(HtmlCode,LabeledTask a)] -> (Task a) |iCreateAndPrint a
chooseTask_radio prompt defaultOn taskOptions = mkTask "chooseTask_radio" (dochooseTask_pdm taskOptions)
where
	dochooseTask_pdm [] tst			= return createDefault tst	
	dochooseTask_pdm taskOptions tst=:{tasknr,html,userId,options}													// choose one subtask out of  a pulldown menu
	# numberOfButtons				= length taskOptions
	# defaultOn						= if (defaultOn >= 0 && defaultOn <= numberOfButtons - 1) defaultOn 0 		
	# taskId						= iTaskId userId tasknr ("ChoStRadio" <+++ numberOfButtons)
	# (chosen,tst)					= LiftHst (mkStoreForm  (Init,storageFormId options taskId (False,defaultOn)) id) tst
	# (done,choice)					= chosen.value
	| not done			// no choice made yet
		# taskRadioMenuId				= iTaskId userId tasknr ("ChoRadio" <+++ numberOfButtons)
		# (nradio,tst)					= LiftHst (ListFuncRadio (Init,sessionFormId options taskRadioMenuId (defaultOn,[\i a -> i \\ j <- [0 .. numberOfButtons - 1]]))) tst
		# choice						= if nradio.changed (snd nradio.value) choice
		# (nradio,tst)					= LiftHst (ListFuncRadio (Set, sessionFormId options taskRadioMenuId (choice,[\i a -> i \\ j <- [0 .. numberOfButtons - 1]]))) tst
		# (_,tst=:{activated=adone,html=ahtml})	
										= editTaskLabel "" "Done" Void {tst & activated = True, html = BT [], tasknr = [-1:tasknr]} 	
		# (_,tst)						= LiftHst (mkStoreForm  (Init,storageFormId options taskId (False,defaultOn)) (\_ -> (adone,choice))) {tst & activated = adone, html = BT []}
		# radioform						= nradio.form <=|>  [[showLabel label] <||> htmlcode \\ (htmlcode,(label,_)) <- taskOptions]
		| not adone						= (createDefault,{tst & activated = False, html = html +|+ BT prompt +|+ BT [radioform] +|+ ahtml, tasknr = tasknr})
		# (_,chosenTask)				= snd (taskOptions!!choice)
		# (a,tst=:{activated=bdone,html=bhtml}) 
										= chosenTask {tst & activated = True, html = BT [], tasknr = [0:tasknr]}
		= (a,{tst & tasknr = tasknr, activated = bdone, html = html +|+ bhtml})
	# (_,chosenTask)					= snd (taskOptions!!choice)
	# (a,tst=:{activated=adone,html=ahtml}) 
									= chosenTask {tst & activated = True, html = BT [], tasknr = [0:tasknr]}
	= (a,{tst & activated = adone, html = html +|+ ahtml, tasknr = tasknr})

chooseTask_cbox :: !([LabeledTask a] -> Task [a]) !HtmlCode ![((!Bool,!ChoiceUpdate,!HtmlCode),LabeledTask a)] -> Task [a] 	| iData a
chooseTask_cbox taskorderfun prompt taskOptions = mkTask "mchoiceTask" (domchoiceTasks taskOptions)
where
	domchoiceTasks [] tst	= ([],{tst& activated = True})
	domchoiceTasks taskOptions tst=:{tasknr,html,options,userId}									// choose one subtask out of the list
	# seltaskId				= iTaskId userId tasknr ("MtpChSel" <+++ length taskOptions)
	# donetaskId			= iTaskId userId tasknr "MtpChSt"
	# (cboxes,tst)			= LiftHst (ListFuncCheckBox (Init,cFormId options seltaskId initCheckboxes)) tst
	# (fun,nblist)			= cboxes.value
	# nsettings				= fun nblist
	# (cboxes,tst)			= LiftHst (ListFuncCheckBox (Set ,cFormId options seltaskId (setCheckboxes nsettings))) tst
	# (done,tst)			= LiftHst (mkStoreForm      (Init,storageFormId options donetaskId False) id) tst
	# optionsform			= cboxes.form <=|> [[showLabel label] <||> htmlcode \\ ((_,_,htmlcode),(label,_)) <- taskOptions]
	| done.value			= taskorderfun [labeledTask \\ (_,labeledTask) <- taskOptions & True <- snd cboxes.value] {tst & tasknr = [0:tasknr]}
	# (_,tst=:{html=ahtml,activated = adone})
							= (editTaskLabel "" "OK" Void) {tst & activated = True, html = BT [], tasknr = [-1:tasknr]} 
	| not adone				= ([],{tst & html = html +|+ BT prompt +|+ BT [optionsform] +|+ ahtml})
	# (_,tst)				= LiftHst (mkStoreForm      (Init,storageFormId options donetaskId False) (\_ -> True)) tst
	= domchoiceTasks taskOptions {tst & tasknr = tasknr, html = html, options = options, userId =userId, activated = True}									// choose one subtask out of the list

	initCheckboxes  = 
		[(if set CBChecked CBNotChecked  label,  \b bs _ -> setfun b bs) \\ ((set,setfun,_),(label,_)) <- taskOptions & i <- [0..]]

	setCheckboxes  boollist = 
		[(if set CBChecked CBNotChecked  label,  \b bs _ -> setfun b bs) \\ ((_,setfun,_),(label,_)) <- taskOptions 
																	& i <- [0..] & set <- boollist]

// ******************************************************************************************************
// Speculative OR-tasks: task ends as soon as one of its subtasks completes

orTask2 :: !(Task a,Task b) -> (Task (EITHER a b)) | iCreateAndPrint a & iCreateAndPrint b
orTask2 (taska,taskb) = mkTask "orTask2" (doorTask2 (taska,taskb))
where
	doorTask2 (taska,taskb) tst=:{tasknr,html,options,userId}
	# taskId								= iTaskId userId tasknr "orTask2St"
	# (chosen,tst)							= LiftHst (mkStoreForm  (Init,storageFormId options taskId -1) id) tst
	| chosen.value == 0						// task a was finished first in the past
		# (a,tst=:{html=ahtml})				= mkParSubTask "orTask" 0 taska {tst & tasknr = tasknr, html = BT []}
		= (LEFT a,{tst & html = html})
	| chosen.value == 1						// task b was finished first in the past
		# (b,tst=:{html=bhtml})				= mkParSubTask "orTask" 1 taskb {tst & tasknr = tasknr, html = BT []}
		= (RIGHT b,{tst & html = html})
	# (a,tst=:{activated=adone,html=ahtml})	= mkParSubTask "orTask" 0 taska {tst & tasknr = tasknr, html = BT []}
	# (b,tst=:{activated=bdone,html=bhtml})	= mkParSubTask "orTask" 1 taskb {tst & tasknr = tasknr, html = BT []}
	| adone
		# tst 								= deleteSubTasksAndThreads [1:tasknr] {tst & tasknr = tasknr}
		# (chosen,tst)						= LiftHst (mkStoreForm  (Init,storageFormId options taskId -1) (\_ -> 0)) {tst & html = BT []}
		= (LEFT a,{tst & html = html, activated = True})
	| bdone
		# tst 								= deleteSubTasksAndThreads [0:tasknr] {tst & tasknr = tasknr}
		# (chosen,tst)						= LiftHst (mkStoreForm  (Init,storageFormId tst.options taskId -1) (\_ -> 1)) {tst & html = BT []}
		= (RIGHT b,{tst & html = html, activated = True})
	= (LEFT a,{tst & activated = False, html = html +|+ ahtml +|+ bhtml})

// ******************************************************************************************************
// Parallel task ends when all it subtask are ended as well

andTask2 :: !(Task a,Task b) -> (Task (a,b)) | iCreateAndPrint a & iCreateAndPrint b
andTask2 (taska,taskb) = mkTask "andTask2" (doAndTask (taska,taskb))
where
	doAndTask (taska,taskb) tst=:{tasknr,html}
	# (a,tst=:{activated=adone,html=ahtml})	= mkParSubTask "andTask" 0 taska {tst & html = BT []}
	# (b,tst=:{activated=bdone,html=bhtml})	= mkParSubTask "andTask" 1 taskb {tst & tasknr = tasknr, html = BT []}
	= ((a,b),{tst & activated = adone&&bdone, html = html +|+ ahtml +|+ bhtml})

andTasksCond :: !String !([a] -> Bool) ![LabeledTask a] -> (Task [a]) | iData a // predicate used to test whether tasks are finished
andTasksCond label pred taskCollection = mkTask "andTasksPred" (doandTasks taskCollection)
where
	doandTasks [] tst	= return [] tst
	doandTasks taskCollection tst=:{tasknr,html,options,userId}
	# (alist,tst=:{activated=finished})		
						= checkAllTasks label taskCollection (0,-1) True [] {tst & html = BT [], activated = True}
	# myalist			= map snd alist
	| finished			= (myalist,{tst & html = html}) 					// stop, all andTasks are finished
	| pred myalist		= (myalist,{tst & html = html, activated = True})  	// stop, all work done so far satisfies predicate
	# buttonnames		= map fst taskCollection
	# ((chosen,buttons,chosenname),tst) 									// user can select one of the tasks to work on
						= LiftHst (mkTaskButtons True "" userId tasknr options buttonnames) tst
	# chosenTask		= snd (taskCollection!!chosen)
	# (a,tst=:{activated=adone,html=ahtml}) 								// enable the selected task (finished or not)
						= mkParSubTask label chosen chosenTask {tst & tasknr = tasknr, activated = True, html = BT []}
	# (alist,tst=:{activated=finished,html=allhtml})						// check again whether all other tasks are now finished, collect their code		
						= checkAllTasks label taskCollection (0,chosen) True [] {tst & tasknr = tasknr, html = BT [], activated = True}
	| not adone			= ([a],{tst &	activated = False 					// not done, since chosen task not finished
									, 	html = 	html +|+ 
												(BT (if (length buttonnames > 1) [showMainLabel label: buttons] [])) +-+ 	
												(BT [showLabel chosenname] +|+ ahtml) +|+ 
												(userId -@: allhtml) 		// code for non selected alternatives are not shown for the owner of this task
							})
	# (alist,tst=:{activated=finished,html=allhtml})		
						= checkAllTasks label taskCollection (0,-1) True [] {tst & html = BT [],activated = True} 
	# myalist			= map snd alist
	| finished			= (myalist,{tst & html = html}) 					// stop, all andTasks are finished
	| pred myalist		= (myalist,{tst & html = html, activated = True}) 	// stop, all work done so far satisfies predicate
	= (map snd alist,{tst 	& activated = finished
							, html = 	html +|+ 
										(BT (if (length buttonnames > 1) [showMainLabel label: buttons] [])) +-+ 	
										(BT [showLabel chosenname] +|+ ahtml) +|+ 
										(userId -@: allhtml)
						})
	
	checkAllTasks :: !String [(String,(*TSt -> *(a,*TSt)))] (Int,Int) Bool [(String,a)] *TSt -> *([(String,a)],*TSt) | iCreateAndPrint a
	checkAllTasks traceid options (ctasknr,skipnr) bool alist tst=:{tasknr}
	| ctasknr == length options 	= (reverse alist,{tst & activated = bool})			// all tasks tested
	| ctasknr == skipnr				= checkAllTasks traceid options (inc ctasknr,skipnr) bool alist tst // skip this task such that it is not included
	# (taskname,task)				= options!!ctasknr
	# (a,tst=:{activated = adone})	= mkParSubTask traceid ctasknr task {tst & tasknr = tasknr, activated = True} // check tasks
	| adone							= checkAllTasks traceid options (inc ctasknr,skipnr) bool [(taskname,a):alist] {tst & tasknr = tasknr, activated = True}
	= checkAllTasks traceid options (inc ctasknr,skipnr) False alist {tst & tasknr = tasknr, activated = True}

// ******************************************************************************************************
// Higher order tasks ! Experimental
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

(-!>) infix 4  :: (Task s) (Task a) -> (Task (Maybe s,TCl a)) | iCreateAndPrint s & iCreateAndPrint a
(-!>)  stoptask task =  mkTask "-!>" stop`
where
	stop` tst=:{tasknr,html,options,userId}
	# (val,tst=:{activated = taskdone,html = taskhtml}) = task     {tst & activated = True, html = BT [], tasknr = normalTaskId,options = options}
	# (s,  tst=:{activated = stopped, html = stophtml})	= stoptask {tst & activated = True, html = BT [], tasknr = stopTaskId,  options = options}
	| stopped	= return_V (Just s, TCl (close task))   {tst & html = html, activated = True}
	| taskdone	= return_V (Nothing,TCl (return_V val)) {tst & html = html +|+ taskhtml, activated = True}
	= return_V (Nothing,TCl (return_V val)) {tst & html = html +|+ taskhtml +|+ stophtml, activated = False}
	where
		close t = \tst -> t {tst & tasknr = normalTaskId, options = options, userId = userId} // reset userId because it influences the task id

		stopTaskId 		= [-1,0:tasknr]
		normalTaskId  	= [-1,1:tasknr]

channel  :: String (Task a) -> (Task (TCl a,TCl a)) | iCreateAndPrint a
channel name task =  mkTask "channel" (doSplit name task)

doSplit name task tst=:{tasknr,options,userId}
= return_V (TCl (sender myTask),TCl (receiver myTask)) tst
where
	myTask tst = task {tst & tasknr = [-1:tasknr], options = options, userId = userId}

	sender task tst=:{activated,tasknr}
	| not activated				= (createDefault,tst)
	# (val,tst) 				= task tst
	= (val,{tst & tasknr = tasknr})

	receiver task  tst=:{activated,tasknr,html}
	| not activated			 	= (createDefault,tst)
	# (val,tst=:{activated}) 	= task tst
	| activated	= (val,{tst & html = html, activated = True , tasknr = tasknr})
	= (val,{tst & html = html /*+|+ BT [showText ("Waiting for completion of "<+++ name)]*/, tasknr = tasknr})

closureTask  :: String (Task a) -> (Task (TCl a)) | iCreateAndPrint a
closureTask name task = mkTask ("closure " +++ name) mkClosure
where
	mkClosure tst=:{tasknr,options,userId}
	# ((TCl sa,ra),tst) 	= doSplit name task tst
	# (_,tst)     			= sa {tst & activated = True}
	= (ra, {tst & activated = True})

closureLzTask  :: String (Task a) -> (Task (TCl a)) | iCreateAndPrint a
closureLzTask name task = mkTask ("closure " +++ name) mkClosure
where
	mkClosure tst=:{tasknr,options,userId}
	# ((TCl sa,ra),tst) 	= doSplit name task tst
	# (_,tst)     			= sa tst
	= (ra, {tst & activated = True})

	doSplit name task tst=:{tasknr,options,userId}
	= return_V (TCl (sender myTask),TCl (receiver myTask)) tst
	where
		myTask tst = task {tst & tasknr = [-1:tasknr], options = options, userId = userId}
	
		sender task tst=:{activated,tasknr}
		| not activated				= (createDefault,tst)
		# (requested,tst)			= (sharedMem id) tst  // is this task demanded ?
		| not requested.value		= (createDefault,tst)
		# (val,tst) 				= task tst
		= (val,{tst & tasknr = tasknr})
	
		receiver task  tst=:{activated,tasknr,html}
		| not activated			 	= (createDefault,tst)
		# (requested,tst)			= (sharedMem (\_ -> True)) tst  // this task is now demanded !
		# (val,tst=:{activated}) 	= task tst
		| activated	= (val,{tst & html = html, activated = True , tasknr = tasknr})
		= (val,{tst & html = html /*+|+ BT [showText ("Waiting for completion of "<+++ name)]*/, tasknr = tasknr})

		sharedStoreId	= iTaskId userId tasknr "Shared_Store"
		sharedMem fun	= LiftHst (mkStoreForm (Init,storageFormId options sharedStoreId False) fun)

LiftHst fun tst=:{hst}
# (form,hst) = fun hst
= (form,{tst & hst = hst})


write{|TCl|} write_a (TCl task) wst
	= write{|*|} (copy_to_string task) wst

read {|TCl|} read_a  wst 
	# (Read str i file) = read{|*|} wst
	= Read (TCl  (deserialize str)) i file
where
	deserialize :: .String -> .(Task .a)
	deserialize str = fst (copy_from_string {c \\ c <-: str })

gPrint{|TCl|} ga (TCl task) ps = ps <<- copy_to_string task

gParse{|TCl|} ga expr
# mbstring = parseString expr
| isNothing mbstring = Nothing
= Just (TCl (fst(copy_from_string {s` \\ s` <-: fromJust mbstring})))
where
	parseString :: Expr -> Maybe String
	parseString expr = gParse{|*|} expr

gUpd{|TCl|} gc (UpdSearch _ 0)	  	 c		= (UpdDone, c)								
gUpd{|TCl|} gc (UpdSearch val cnt)  c		= (UpdSearch val (cnt - 2),c)						
gUpd{|TCl|} gc (UpdCreate l)        _		
# (mode,default)	= gc (UpdCreate l) undef
= (UpdCreate l, TCl (\tst -> (default,tst)))			
gUpd{|TCl|} gc mode                 b		= (mode, b)										

gForm{|TCl|} gfa (init,formid) hst
= ({value=formid.ival,changed=False,form=[]},hst)

