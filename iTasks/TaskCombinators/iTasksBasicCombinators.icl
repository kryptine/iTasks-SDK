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
import iTasksTypes, iTasksLiftingCombinators
import InternaliTasksThreadHandling
import GenBimap

derive gForm 	[], Time
derive gUpd 	[], Time
derive gPrint	Time
derive gParse	Time


// ******************************************************************************************************
// monads for combining iTasks

(=>>) infixl 1 :: !(Task a) !(a -> Task b) -> Task b | iCreateAndPrint b
(=>>) taska taskb = Task mybind
where
	mybind tst=:{options}
	# (a,tst=:{activated})	= appTaskTSt taska tst
	| activated				= appTaskTSt (taskb a) {tst & options = options}
							= (createDefault,tst)

return_V :: !a -> (Task a) | iCreateAndPrint a
return_V a  = mkTask "return_V" (Task dotask)
where
	dotask tst = (a,tst) 

	
// ******************************************************************************************************
// newTask needed for recursive task creation

newTask :: !String !(Task a) -> (Task a) 	| iData a 
newTask taskname mytask = mkTask taskname (Task newTask`)
where
	newTask` tst=:{tasknr,userId,options}		
	# taskId					= iTaskId userId tasknr taskname
	# (taskval,tst) 			= liftHst (mkStoreForm (Init,storageFormId options taskId (False,createDefault)) id) tst  // remember if the task has been done
	# (taskdone,taskvalue)		= taskval.Form.value											// select values
	| taskdone					= (taskvalue,tst)												// if rewritten return stored value
	# (val,tst=:{activated})	= appTaskTSt mytask {tst & tasknr = [-1:tasknr]} 				// do task, first shift tasknr
	| not activated				= (createDefault,{tst & tasknr = tasknr, options = options})	// subtask not ready, return value of subtasks
	# tst						= deleteSubTasksAndThreads tasknr tst							// task ready, garbage collect it
	# (_,tst) 					= liftHst (mkStoreForm (Init,storageFormId options taskId (False,createDefault)) (\_ -> (True,val))) tst  // remember if the task has been done
	= (val,{tst & tasknr = tasknr, options = options})


Once :: !String !(Task a) -> (Task a) | iData a
Once label task = mkTask label (Task doit)
where
	doit tst=:{activated,tasknr,hst,userId,options}
	# taskId			= iTaskId userId tasknr (label +++ "_")
	# (store,hst) 		= mkStoreForm (Init,storageFormId options taskId (False,createDefault)) id hst  			
	# (done,value)		= store.Form.value
	| done 				= (value,{tst & hst = hst})																		// if task has completed, don't do it again
	# (value,tst=:{hst})= appTaskTSt task {tst & hst = hst}
	# (store,hst) 		= mkStoreForm (Init,storageFormId options taskId (False,createDefault)) (\_ -> (True,value)) hst // remember task status for next time
	# (done,value)		= store.Form.value
	= (value,{tst & activated = done, hst = hst})																		// task is now completed, handle as previously

newTaskTrace :: !String !(Task a) -> (Task a) | iData a 			// used to insert a task trace later MJP BUG	 
newTaskTrace taskname mytask = /* newTask taskname */ mytask

// ******************************************************************************************************
// looping tasks

// when gc option set and task finished, it will throw away all subtasks and start all over
// otherwise, when task finshed it will remember the new tasknr to prevent checking of previously finished tasks

foreverTask :: !(Task a) -> Task a | iData a
foreverTask task = mkTask "foreverTask" (Task foreverTask`)
where
	foreverTask` tst=:{tasknr,activated,userId,options,html} 
	| options.gc == Collect																								// garbace collect everything when task finsihed
		# (val,tst=:{activated})= appTaskTSt task {tst & tasknr = [-1:tasknr]}											// shift tasknr
		| activated 			= foreverTask` (deleteSubTasksAndThreads tasknr {tst & tasknr = tasknr, options = options/*, html = html*/}) 			// loop
		= (val,tst)					
	# taskId					= iTaskId userId tasknr "ForSt"															// create store id
	# (currtasknr,tst)			= liftHst (mkStoreForm (Init,storageFormId options taskId tasknr) id) tst				// fetch actual tasknr
	# (val,tst=:{activated})	= appTaskTSt task {tst & tasknr = [-1:currtasknr.Form.value]}
	| activated 																										// task is completed	
		# ntasknr				= incNr currtasknr.Form.value															// incr tasknr
		# (currtasknr,tst)		= liftHst (mkStoreForm (Init,storageFormId options taskId tasknr) (\_ -> ntasknr)) tst 	// store next task nr
		= foreverTask` {tst & tasknr = tasknr, options = options/*, html = html*/}											// initialize new task
	= (val,tst)					

(<!) infixl 6 :: !(Task a) !(a -> .Bool) -> Task a | iCreateAndPrint a
(<!) taska pred = mkTask "less!" (Task doTask)
where
	doTask tst=:{activated, tasknr}
	# (a,tst=:{activated}) 	= appTaskTSt taska {tst & tasknr = [-1:tasknr]}
	| not activated 		= (a,tst)
	| not (pred a)			
		# tst = deleteSubTasksAndThreads [0:tasknr] tst
		= doTask {tst & tasknr = tasknr}
//		= (a,{tst & activated = False})
	= (a,tst)

// ******************************************************************************************************
// Assigning tasks to users, each user has to be identified by an unique number >= 0

assignTaskTo :: !UserId !(LabeledTask a) -> Task a | iData a	
assignTaskTo nuserId (taskname,taska) = newTask "assignTaskTo" (mkTask taskname (Task assignTaskTo`))
where
	assignTaskTo` tst=:{tasknr,activated,userId,workflowLink=(_,(_,processNr,workflowLabel))}
	| not activated						= (createDefault,tst)
	# (currtime,tst=:{html=ohtml})		= appTaskTSt (appWorldOnce ("Task " +++ taskname +++ " for " +++ toString nuserId) time) tst
	# tst								= IF_Ajax (administrateNewThread userId tst) tst 
	# (a,tst=:{html=nhtml,activated})	= appTaskTSt (IF_Ajax (UseAjax @>> taska) taska) {tst & html = BT [] [],userId = nuserId}	// activate task of indicated user NEWTRACE
	| activated 						= (a,{tst & activated = True													// work is done	
												  ,	userId = userId														// restore previous user id						
												  ,	html = ohtml +|+ (taskDescriptor currtime activated @@: nhtml)})									// plus new one tagged
	= (a,{tst & userId = userId																							// restore user Id
			  , html = 	ohtml +|+ (taskDescriptor currtime activated @@: nhtml)
		 })												
	where
		taskDescriptor currtime activated
		= 	{ delegatorId 	= userId
			, taskWorkerId	= nuserId
			, taskNrId		= toStringTaskNr tasknr
			, processNr		= processNr
			, workflowLabel	= workflowLabel
			, taskPriority	= NormalPriority
			, taskLabel		= taskname
			, timeCreated	= currtime
	 		, curStatus		= activated
	 		} 


// ******************************************************************************************************
// sequencingtasks

seqTasks :: ![LabeledTask a] -> (Task [a])| iCreateAndPrint a
seqTasks [(label,task)] = task =>> \na -> return_V [na]
seqTasks options = mkTask "seqTasks" (Task seqTasks`)
where
	seqTasks` tst=:{tasknr}
	# (val,tst)	 = doseqTasks options [] {tst & tasknr = [-1:tasknr]}
	= (val,{tst & tasknr = tasknr})

	doseqTasks [] accu tst 		= (reverse accu,{tst & activated = True})
	doseqTasks [(taskname,task):ts] accu tst=:{html,options} 
	# (a,tst=:{activated=adone,html=ahtml}) 
									= appTaskTSt task {tst & activated = True, html = BT [] []}
	| not adone						= (reverse accu,{tst & html = html +|+ BT [showLabel taskname,BrTag [] ,BrTag []] [] +|+ ahtml})
	= doseqTasks ts [a:accu] {tst & html = html +|+ ahtml, options = options}

// ******************************************************************************************************
// Select the tasks to do from a list with help of another task for selecting them:

selectTasks 	:: !(SelectingTask a) !(OrderingTask a) ![LabeledTask a] -> Task [a] | iData a
selectTasks chooser executer ltasks = newTask "selectTask" selectTasks`
where
	selectTasks`
	=						chooser ltasks
			=>> \chosen -> 	executer [ltasks!!i \\ i <- chosen | i >=0 && i < lengthltask]
			
	lengthltask = length ltasks

allTasksCond 	:: !String !DisplaySubTasks !(FinishPred a) ![LabeledTask a] -> Task [a] | iData a 
allTasksCond label displayOption pred taskCollection 
= 					mkTask label (Task (doandTasks taskCollection))
where
	doandTasks [] tst	= return [] tst
	doandTasks taskCollection tst=:{tasknr,html}
	# ((alist,acode),tst)		= checkAllTasks label taskCollection 0 ([],[]) {tst & html = BT [] [],activated = True} 
	| and (map fst acode) || pred alist	= (alist,{tst & html = html, activated = True}) 	// stop, all work done so far satisfies predicate
	= (alist,{tst 	& activated = False
					, html 		= html +|+ displayOption label tasknr acode				// show all subtasks using the displayOption function
								
			})
	where
		checkAllTasks :: !String ![LabeledTask a] !Int !(![a],![(Bool,HtmlTree)]) !*TSt -> *(!(![a],![(Bool,HtmlTree)]),!*TSt) | iCreateAndPrint a
		checkAllTasks traceid taskCollection ctasknr (alist,acode) tst=:{tasknr}
		| ctasknr == length taskCollection 	= ((reverse alist,reverse acode),tst)			// all tasks tested
		# (taskname,task)		= taskCollection!!ctasknr
		# (a,tst=:{activated = adone,html=html})	
								= appTaskTSt (mkParSubTask traceid ctasknr task) {tst & tasknr = tasknr, activated = True, html = BT [] []} // check tasks
		= checkAllTasks traceid taskCollection (inc ctasknr) (if adone [a:alist] alist,[(adone,html):acode]) {tst & tasknr = tasknr}

displayAsTab :: DisplaySubTasks
displayAsTab = displayAsTab`
where
	displayAsTab` label tasknr htmls 
		= CondAnd label nrSubTasks [({ caTaskNrId	= toStringTaskNr [0,i:tasknr]
									  , caIndex		= nrSubTasks
									  , caStatus	= finished	
									  },html) \\ (finished,html) <- htmls & i <- [0..]
									]
	where
		nrSubTasks = length htmls
	

displayAll :: DisplaySubTasks
displayAll = displayAll`
where
	displayAll` label tasknr htmls 
		= foldl (+|+) (BT [] []) (map snd htmls) 


	
	
/*
allTasksCond 	:: !String !(TasksToShow a) !(FinishPred a) ![LabeledTask a] -> Task [a] | iData a 
allTasksCond label chooser pred taskCollection 
= 					mkTask "andTasksCond" (Task (doandTasks chooser taskCollection))
where
	lengthltask = length taskCollection 

	doandTasks chooser [] tst	= return [] tst
	doandTasks chooser taskCollection tst=:{tasknr,html,options,userId}
	# ((alist,acode),tst=:{activated=finished,html=allhtml})		
						= checkAllTasks label taskCollection 0 True ([],[]) {tst & html = BT [] [],activated = True} 
	| finished			= (alist,{tst & html = html}) 						// stop, all andTasks are finished
	| pred alist		= (alist,{tst & html = html, activated = True}) 	// stop, all work done so far satisfies predicate
	# selectId			= iTaskId userId tasknr "anTaskSelect"
	# ((selected,shtml),tst)	= chooser selectId taskCollection {tst & html = BT [] []}
	# (_,tst=:{html=ashtml})	= showtasks label [(i,taskCollection!!i) \\ i <- selected | i >= 0 && i < lengthltask] {tst & html = BT [] [], activated = True}		
	= (alist,{tst 	& activated = finished
					, html = 	html +|+ 									// show previous code
								((BT shtml []) +-+ ashtml) +|+ 				// show selection button + selected itasks
								(userId -@: foldl (+|+) (BT [] []) [htmlcode \\ htmlcode <- acode & i <- [0..] | not (isMember i selected)]) // dont show non selected itasks, but scan them for task tree info								
			})
	where
		showtasks :: !String ![(!Int,!LabeledTask a)] !*TSt -> *(![a],!*TSt) | iCreateAndPrint a
		showtasks _ [] tst			= ([],tst)
		showtasks label [(chosen,(name,chosenTask)):tasks] tst=:{html=html}
		# (a,tst=:{html=ahtml}) 	= appTaskTSt (mkParSubTask label chosen chosenTask) {tst & tasknr = tasknr, activated = True, html = BT [] []}
		# (as,tst=:{html=ashtml})	= showtasks label tasks {tst & html = BT [] []}
		= ([a:as],{tst & html = html +|+ ahtml +|+ ashtml})			


		checkAllTasks :: !String ![LabeledTask a] !Int !Bool !(![a],![HtmlTree]) !*TSt -> *(!(![a],![HtmlTree]),!*TSt) | iCreateAndPrint a
		checkAllTasks traceid taskCollection ctasknr bool (alist,acode) tst=:{tasknr}
		| ctasknr == length taskCollection 	= ((reverse alist,reverse acode),{tst & activated = bool})			// all tasks tested
		# (taskname,task)		= taskCollection!!ctasknr
		# (a,tst=:{activated = adone,html=html})	
								= appTaskTSt (mkParSubTask traceid ctasknr task) {tst & tasknr = tasknr, activated = True, html = BT [] []} // check tasks
		| adone					= checkAllTasks traceid taskCollection (inc ctasknr) bool ([a:alist],[html:acode]) {tst & tasknr = tasknr, activated = True}
		= checkAllTasks traceid taskCollection (inc ctasknr) False (alist,[html:acode]) {tst & tasknr = tasknr, activated = True}


*/



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
(-!>)  stoptask task =  mkTask "-!>" (Task stop`)
where
	stop` tst=:{tasknr,html,options,userId}
	# (val,tst=:{activated = taskdone,html = taskhtml}) = appTaskTSt task     {tst & activated = True, html = BT [] [], tasknr = normalTaskId,options = options}
	# (s,  tst=:{activated = stopped, html = stophtml})	= appTaskTSt stoptask {tst & activated = True, html = BT [] [], tasknr = stopTaskId,  options = options}
	| stopped	= appTaskTSt (return_V (Just s, TCl (Task (close task))))   {tst & html = html, activated = True}
	| taskdone	= appTaskTSt (return_V (Nothing,TCl (return_V val))) {tst & html = html +|+ taskhtml, activated = True}
	= appTaskTSt (return_V (Nothing,TCl (return_V val))) {tst & html = html +|+ taskhtml +|+ stophtml, activated = False}
	where
		close t = \tst -> appTaskTSt t {tst & tasknr = normalTaskId, options = options, userId = userId} // reset userId because it influences the task id

		stopTaskId 		= [-1,0:tasknr]
		normalTaskId  	= [-1,1:tasknr]

channel  :: String (Task a) -> (Task (TCl a,TCl a)) | iCreateAndPrint a
channel name task =  mkTask "channel" (Task (doSplit name task))

doSplit name task tst=:{tasknr,options,userId}
= appTaskTSt (return_V (TCl (Task (sender (Task myTask))),TCl (Task (receiver (Task myTask))))) tst
where
	myTask tst = appTaskTSt task {tst & tasknr = [-1:tasknr], options = options, userId = userId}

	sender task tst=:{activated,tasknr}
	| not activated				= (createDefault,tst)
	# (val,tst) 				= appTaskTSt task tst
	= (val,{tst & tasknr = tasknr})

	receiver task  tst=:{activated,tasknr,html}
	| not activated			 	= (createDefault,tst)
	# (val,tst=:{activated}) 	= appTaskTSt task tst
	| activated	= (val,{tst & html = html, activated = True , tasknr = tasknr})
	= (val,{tst & html = html /*+|+ BT [showText ("Waiting for completion of "<+++ name)]*/, tasknr = tasknr})

closureTask  ::  (LabeledTask a) -> (Task (TCl a)) | iCreateAndPrint a
closureTask (name, task) = mkTask ("closure " +++ name) (Task mkClosure)
where
	mkClosure tst=:{tasknr,options,userId}
	# ((TCl sa,ra),tst) 	= doSplit name task tst
	# (_,tst)     			= appTaskTSt sa {tst & activated = True}
	= (ra, {tst & activated = True})

closureLzTask  :: (LabeledTask a) -> (Task (TCl a)) | iCreateAndPrint a
closureLzTask (name, task) = mkTask ("lazy closure " +++ name) (Task mkClosure)
where
	mkClosure tst=:{tasknr,options,userId}
	# ((TCl sa,ra),tst) 	= doSplit name task tst
	# (_,tst)     			= appTaskTSt sa tst
	= (ra, {tst & activated = True})

	doSplit name task tst=:{tasknr,options,userId}
		= appTaskTSt (return_V (TCl (Task (sender (Task myTask))),TCl (Task (receiver (Task myTask))))) tst
	where
		myTask tst = appTaskTSt task {tst & tasknr = [-1:tasknr], options = options, userId = userId}
	
		sender task tst=:{activated,tasknr}
		| not activated				= (createDefault,tst)
		# (requested,tst)			= (sharedMem id) tst  // is this task demanded ?
		| not requested.Form.value	= (createDefault,tst)
		# (val,tst) 				= appTaskTSt task tst
		= (val,{tst & tasknr = tasknr})
	
		receiver task tst=:{activated,tasknr,html}
		| not activated			 	= (createDefault,tst)
		# (requested,tst)			= (sharedMem (\_ -> True)) tst  // this task is now demanded !
		# (val,tst=:{activated}) 	= appTaskTSt task tst
		| activated	= (val,{tst & html = html, activated = True , tasknr = tasknr})
		= (val,{tst & html = html /*+|+ BT [showText ("Waiting for completion of "<+++ name)]*/, tasknr = tasknr})

		sharedStoreId	= iTaskId userId tasknr "Shared_Store"
		sharedMem fun	= liftHst (mkStoreForm (Init,storageFormId options sharedStoreId False) fun)



