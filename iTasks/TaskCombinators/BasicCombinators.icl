implementation module BasicCombinators

import StdList, StdArray, StdTuple, StdFunc
import dynamic_string, graph_to_string_with_descriptors, graph_to_sapl_string
import DrupBasic
import iDataTrivial, iDataFormlib
import LiftingCombinators, ClientCombinators
import InternaliTasksThreadHandling, InternaliTasksCommon
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
	# (a,tst=:{activated})	= accTaskTSt taska tst
	| activated				= accTaskTSt (taskb a) {tst & options = options}
							= (createDefault,tst)

return_V :: !a -> (Task a) | iCreateAndPrint a
return_V a  = mkBasicTask "return_V" (Task return_V`)
where
	return_V` tst = (a,tst) 

// ******************************************************************************************************
// Assigning tasks to users, each user has to be identified by an unique number >= 0

assignTaskTo :: !UserId !(LabeledTask a) -> Task a | iData a	
assignTaskTo newUserId (label,task) = Task assignTaskTo` 
where
	assignTaskTo` tst =:{TSt | userId = currentUserId}
		# tst		= addUser newUserId tst 
		# (a, tst)	= accTaskTSt (newTask label task) {TSt | tst & userId = newUserId}
		= (a, {TSt | tst & userId = currentUserId})

// ******************************************************************************************************
// newTask needed for recursive task creation

newTask :: !String !(Task a) -> (Task a) 	| iData a 
newTask taskname mytask = Task newTask`
where
	newTask` tst=:{taskNr,userId,options}		
		# storeName					= iTaskId userId (incTaskNr taskNr) taskname					
		# (taskval,tst) 			= accHStTSt (mkStoreForm (Init,storageFormId options storeName (False,createDefault)) id) tst	// remember if the task has been done
		# (taskdone,taskvalue)		= taskval.Form.value																			// select values
		| taskdone					= accTaskTSt (mkBasicTask taskname (Task (\tst -> (taskvalue,tst)))) tst						// if rewritten, we are a basic task returning a value
		# (a, tst=:{activated})		= accTaskTSt (mkSequenceTask taskname mytask) tst 												// execute task in an isolated sequence
		| activated
			# tst					= deleteSubTasksAndThreads (incTaskNr taskNr) tst																//garbage collect it
			# (_,tst) 				= accHStTSt (mkStoreForm (Init, storageFormId options storeName (False,createDefault)) (\_ -> (True,a))) tst	//remember that the task was done
			= (a, tst)
		| otherwise
			= (a, tst)

once :: !String !(Task a) -> (Task a) | iData a
once label task = mkBasicTask label (Task once`)
where
	once` tst=:{activated,taskNr,hst,userId,options}
		# taskId			= iTaskId userId taskNr (label +++ "_")
		# (store,hst) 		= mkStoreForm (Init,storageFormId options taskId (False,createDefault)) id hst  			
		# (done,value)		= store.Form.value
		| done 				= (value,{tst & hst = hst})																		// if task has completed, don't do it again
		# (value,tst=:{hst})= accTaskTSt task {tst & hst = hst}
		# (store,hst) 		= mkStoreForm (Init,storageFormId options taskId (False,createDefault)) (\_ -> (True,value)) hst // remember task status for next time
		# (done,value)		= store.Form.value
		= (value,{tst & activated = done, hst = hst})																		// task is now completed, handle as previously

newTaskTrace :: !String !(Task a) -> Task a | iData a 			// used to insert a task trace later MJP BUG	 
newTaskTrace taskname mytask = /* newTask taskname */ mytask

// ******************************************************************************************************
// looping tasks

// when gc option set and task finished, it will throw away all subtasks and start all over
// otherwise, when task finshed it will remember the new tasknr to prevent checking of previously finished tasks

foreverTask :: !(Task a) -> Task a | iData a
foreverTask task = mkSequenceTask "foreverTask" (Task foreverTask`)
where
	foreverTask` tst=:{taskNr} 
		# (val,tst=:{activated})= accTaskTSt task tst																	// execute task
		| activated		
			# tst = deleteSubTasksAndThreads (tl taskNr) tst
			# tst = resetSequence tst
			= foreverTask` tst				
		= (val,tst)					
/*
	# taskId					= iTaskId userId taskNr "ForSt"															// create store id
	# (currtasknr,tst)			= liftHst (mkStoreForm (Init,storageFormId options taskId taskNr) id) tst				// fetch actual tasknr
	# (val,tst=:{activated})	= accTaskTSt task {tst & taskNr = [-1:currtasknr.Form.value]}
	| activated 																										// task is completed	
		# ntasknr				= incTaskNr currtasknr.Form.value														// incr tasknr
		# (currtasknr,tst)		= liftHst (mkStoreForm (Init,storageFormId options taskId taskNr) (\_ -> ntasknr)) tst 	// store next task nr
		= foreverTask` {tst & taskNr = taskNr, options = options/*, html = html*/}										// initialize new task
	= (val,tst)					
*/

(<!) infixl 6 :: !(Task a) !(a -> .Bool) -> Task a | iCreateAndPrint a
(<!) taska pred = mkBasicTask "untilTask" (Task doTask)
where
	doTask tst=:{activated, taskNr}
	# (a,tst=:{activated}) 	= accTaskTSt taska {tst & taskNr = [-1:taskNr]}
	| not activated 		= (a,tst)
	| not (pred a)			
		# tst = deleteSubTasksAndThreads [0:taskNr] tst
		= doTask {tst & taskNr = taskNr}
//		= (a,{tst & activated = False})
	= (a,tst)


// ******************************************************************************************************
// sequencingtasks
seqTasks :: ![LabeledTask a] -> (Task [a])	| iCreateAndPrint a
seqTasks options = mkSequenceTask "seqTasks" (Task seqTasks`)
where
	seqTasks` tst
		= doseqTasks options [] tst

	doseqTasks [] accu tst				= (reverse accu,{tst & activated = True})
	doseqTasks [(taskname,task):ts] accu tst=:{options} 
		# (a,tst=:{activated=adone}) 
										= accTaskTSt task {tst & activated = True}
		| not adone						= (reverse accu, tst)
		| otherwise						= doseqTasks ts [a:accu] tst

// ******************************************************************************************************
// Select the tasks to do from a list with help of another task for selecting them:

selectTasks 	:: !(SelectingTask a) !(OrderingTask a) ![LabeledTask a] -> Task [a] | iData a
selectTasks chooser executer ltasks = newTask "selectTasks" selectTasks`
where
	selectTasks`
		=					chooser ltasks
			=>> \chosen -> 	executer [ltasks!!i \\ i <- chosen | i >=0 && i < lengthltask]
			
	lengthltask = length ltasks


allTasksCond 	:: !String !DisplaySubTasks !(FinishPred a) ![LabeledTask a] -> Task [a] | iData a 
allTasksCond label displayOption pred taskCollection 
	= mkParallelTask label (Task (doandTasks taskCollection))
where
	doandTasks [] tst	= return [] tst
	doandTasks taskCollection tst=:{taskNr,html}
		# ((alist,acode),tst)		= checkAllTasks label taskCollection 0 ([],[]) {tst & html = BT [] [],activated = True} 
		| and (map (\(x,_,_) -> x) acode) || pred alist
			= (alist,{tst & html = html, activated = True}) 	// stop, all work done so far satisfies predicate
		| otherwise
			# (output,combination)	= displayOption label taskNr acode
			# tst					= setCombination combination tst
			= (alist, {tst & activated	= False
						   , html 		= html +|+ output })	// show all subtasks using the displayOption function
	where
		checkAllTasks :: !String ![LabeledTask a] !Int !(![a],![(Bool,String,HtmlTree)]) !*TSt -> *(!(![a],![(Bool,String,HtmlTree)]),!*TSt) | iCreateAndPrint a
		checkAllTasks traceid taskCollection ctasknr (alist,acode) tst=:{taskNr}
			| ctasknr == length taskCollection
				= ((reverse alist,reverse acode),tst)			// all tasks tested
			# (taskname,task)		= taskCollection!!ctasknr
			# (a,tst=:{activated = adone, html})	
									= accTaskTSt (mkParallelSubTask traceid ctasknr task) {tst & taskNr = taskNr, activated = True, html = BT [] []} // check tasks
			= checkAllTasks traceid taskCollection (inc ctasknr) (if adone [a:alist] alist,[(adone,taskname,html):acode]) {tst & taskNr = taskNr}

displayAsTab :: DisplaySubTasks
displayAsTab = displayAsTab`
where
	displayAsTab` label tasknr htmls 
		= (CondAnd label nrSubTasks [( { caTaskNrId		= taskNrToString [0,i:tasknr]
									  , caTaskLabel		= tlabel
									  , caIndex			= i
									  , caNumSiblings	= nrSubTasks
									  , caStatus		= finished	
									  },html) \\ (finished,tlabel,html) <- htmls & i <- [0..]
									],TTSplit)
	where
		nrSubTasks = length htmls
	

displayAll :: DisplaySubTasks
displayAll = displayAll`
where
	displayAll` label tasknr htmls 
		= (foldl (+|+) (BT [] []) (map (\(_,_,x) -> x) htmls), TTVertical) 

// ******************************************************************************************************
// Higher order tasks ! Experimental
/* Experimental department:

   May not work when the tasks are garbage collected !!

-!>				:: a task, either finished or interrupted (by completion of the first task) is returned in the closure
				   if interrupted, the work done so far is returned (!) which can be continued somewhere else
channel			:: splits a task in respectively a sender task closure and receiver taskclosure; 
				   when the sender is evaluated, the original task is evaluated as usual;
				   when the receiver task is evaluated, it will wait upon completeion of the sender and then get's its result;
				   Important: Notice that a receiver will never finish if you don't activate the corresponding sender somewhere.
closureTask		:: The task is executed as usual, but a receiver closure is returned immediately.
				   When the closure is evaluated somewhere, one has to wait until the task is finished.
				   Handy for passing a result to several interested parties.
closureLZTask	:: Same, but now the original task will not be done unless someone is asking for the result somewhere.
*/

(-!>) infix 4  :: (Task s) (Task a) -> (Task (Maybe s,Task a)) | iCreateAndPrint s & iCreateAndPrint a
(-!>)  stoptask task =  mkBasicTask "-!>" (Task stop`)
where
	stop` tst=:{taskNr,html=html,options,userId}
		# (val,tst=:{activated = taskdone,html = taskhtml}) = accTaskTSt task     {tst & activated = True, html = BT [] [], taskNr = normalTaskId,options = options}
		# (s,  tst=:{activated = stopped, html = stophtml})	= accTaskTSt stoptask {tst & activated = True, html = BT [] [], taskNr = stopTaskId,  options = options}
		| stopped	= accTaskTSt (return_V (Just s, Task (close task)))   {tst & html = html, activated = True}
		| taskdone	= accTaskTSt (return_V (Nothing,return_V val)) {tst & html = html +|+ taskhtml , activated = True}
		= accTaskTSt (return_V (Nothing,return_V val)) {tst & html = html +|+ taskhtml +|+ stophtml , activated = False}
	where
		close t = \tst -> accTaskTSt t {tst & taskNr = normalTaskId, options = options, userId = userId} // reset userId because it influences the task id

		stopTaskId 		= [-1,0:taskNr]
		normalTaskId  	= [-1,1:taskNr]

channel  :: String (Task a) -> (Task (Task a,Task a)) | iCreateAndPrint a
channel name task =  mkBasicTask "channel" (Task (doSplit name task))

doSplit name task tst=:{taskNr,options,userId}
= accTaskTSt (return_V (Task (sender (Task myTask)),Task (receiver (Task myTask)))) tst
where
	myTask tst = accTaskTSt task {tst & taskNr = [-1:taskNr], options = options, userId = userId}

	sender task tst=:{activated,taskNr}
	| not activated				= (createDefault,tst)
	# (val,tst) 				= accTaskTSt task tst
	= (val,{tst & taskNr = taskNr})

	receiver task  tst=:{activated,taskNr,html}
	| not activated			 	= (createDefault,tst)
	# (val,tst=:{activated}) 	= accTaskTSt task tst
	| activated	= (val,{tst & html = html, activated = True , taskNr = taskNr})
	= (val,{tst & html = html /*+|+ BT [showText ("Waiting for completion of "<+++ name)]*/, taskNr = taskNr})

closureTask  ::  (LabeledTask a) -> (Task (Task a)) | iCreateAndPrint a
closureTask (name, task) = mkBasicTask ("closure " +++ name) (Task mkClosure)
where
	mkClosure tst=:{taskNr,options,userId}
	# ((sa,ra),tst) 		= doSplit name task tst
	# (_,tst)     			= accTaskTSt sa {tst & activated = True}
	= (ra, {tst & activated = True})

closureLzTask  :: (LabeledTask a) -> (Task (Task a)) | iCreateAndPrint a
closureLzTask (name, task) = mkBasicTask ("lazy closure " +++ name) (Task mkClosure)
where
	mkClosure tst=:{taskNr,options,userId}
	# ((sa,ra),tst) 		= doSplit name task tst
	# (_,tst)     			= accTaskTSt sa tst
	= (ra, {tst & activated = True})

	doSplit name task tst=:{taskNr,options,userId}
		= accTaskTSt (return_V (Task (sender (Task myTask)), Task (receiver (Task myTask)))) tst
	where
		myTask tst = accTaskTSt task {tst & taskNr = [-1:taskNr], options = options, userId = userId}
	
		sender task tst=:{activated,taskNr}
		| not activated				= (createDefault,tst)
		# (requested,tst)			= (sharedMem id) tst  // is this task demanded ?
		| not requested.Form.value	= (createDefault,tst)
		# (val,tst) 				= accTaskTSt task tst
		= (val,{tst & taskNr = taskNr})
	
		receiver task tst=:{activated,taskNr,html}
		| not activated			 	= (createDefault,tst)
		# (requested,tst)			= (sharedMem (\_ -> True)) tst  // this task is now demanded !
		# (val,tst=:{activated}) 	= accTaskTSt task tst
		| activated	= (val,{tst & html = html, activated = True , taskNr = taskNr})
		= (val,{tst & html = html /*+|+ BT [showText ("Waiting for completion of "<+++ name)]*/, taskNr = taskNr})

		sharedStoreId	= iTaskId userId taskNr "Shared_Store"
		sharedMem fun	= liftHst (mkStoreForm (Init,storageFormId options sharedStoreId False) fun)



