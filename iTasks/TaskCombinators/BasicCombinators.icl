implementation module BasicCombinators

import StdList, StdArray, StdTuple, StdMisc
from StdFunc import id, const
import dynamic_string, graph_to_string_with_descriptors, graph_to_sapl_string
import DrupBasic
import iDataTrivial, iDataFormlib
import LiftingCombinators, ClientCombinators
import Util, InternaliTasksThreadHandling
import GenBimap

derive gForm 	Time
derive gUpd 	Time
derive gPrint	Time
derive gParse	Time


//Standard monadic operations:

(>>=) infixl 1 :: !(Task a) !(a -> Task b) -> Task b | iCreateAndPrint b
(>>=) taska taskb = Task tbind
where
	tbind tst=:{options}
		# (a,tst=:{activated})	= accTaskTSt taska tst
		| activated				= accTaskTSt (taskb a) {tst & options = options}
								= (createDefault,tst)

(>>|) infixl 1 :: !(Task a) (Task b) -> Task b | iCreateAndPrint b
(>>|) taska taskb = taska >>= const taskb

return :: !a -> (Task a) | iData a
return a  = mkBasicTask "return" (\tst -> (a,tst))


//Repetition and loops:

forever :: !(Task a) -> Task a | iData a
forever task = mkSequenceTask "forever" (Task forever`)
where
	forever` tst=:{taskNr} 
		# (val,tst=:{activated})= accTaskTSt task tst					
		| activated		
			# tst = deleteSubTasksAndThreads (tl taskNr) tst
			# tst = resetSequence tst
			= forever` tst				
		= (val,tst)

(<!) infixl 6 :: !(Task a) !(a -> .Bool) -> Task a | iCreateAndPrint a
(<!) task pred = mkSequenceTask "<!" (Task doTask)
where
	doTask tst=:{activated, taskNr}
		# (a,tst=:{activated}) 	= accTaskTSt task tst
		| not activated
			= (a,tst)
		| not (pred a)			
			# tst = deleteSubTasksAndThreads (tl taskNr) tst
			# tst = resetSequence tst
			= doTask tst
		= (a,tst)


// Selection:

selection :: !([LabeledTask a] -> Task [Int]) !([LabeledTask a] -> Task [a]) ![LabeledTask a] -> Task [a] | iData a
selection chooser executer tasks = mkSequenceTask "selection" selection`
where
	selection`	= chooser tasks =>> \chosen -> 	executer [tasks!!i \\ i <- chosen | i >=0 && i < numTasks]
	numTasks 	= length tasks


// Sequential composition

sequence :: !String ![LabeledTask a] -> (Task [a])	| iCreateAndPrint a
sequence label options = mkSequenceTask label (Task sequence`)
where
	sequence` tst
		= doseqTasks options [] tst

	doseqTasks [] accu tst				= (reverse accu,{tst & activated = True})
	doseqTasks [(taskname,task):ts] accu tst=:{options} 
		# (a,tst=:{activated=adone}) 
										= accTaskTSt task {tst & activated = True}
		| not adone						= (reverse accu, tst)
		| otherwise						= doseqTasks ts [a:accu] tst


// Parallel composition

parallel :: !String !TaskCombination !([a] -> Bool) ![LabeledTask a] -> Task [a] | iData a 
parallel label combination pred taskCollection 
	= mkParallelTask label (Task (doandTasks taskCollection))
where
	doandTasks [] tst	=  ([],tst)
	doandTasks taskCollection tst=:{taskNr}
		# (alist,tst)	= checkAllTasks taskCollection 0 [] tst 
		| length alist == length taskCollection					// all tasks are done
			= (alist,{tst & activated = True})
		| pred alist
			= (alist,{tst & activated = True}) 					// stop, all work done so far satisfies predicate
		| otherwise	
			# tst		= setCombination combination tst
			= (alist, {tst & activated	= False})				// show all subtasks using the displayOption function
	where
		checkAllTasks :: ![LabeledTask a] !Int ![a] !*TSt -> (![a],!*TSt) | iCreateAndPrint a
		checkAllTasks taskCollection index accu tst
			| index == length taskCollection
				= (reverse accu,tst)															// all tasks tested
			# (taskname,task)			= taskCollection!!index
			# (a,tst=:{activated})	= accTaskTSt (mkParallelSubTask taskname index task) tst	// check tasks
			= checkAllTasks taskCollection (inc index) (if activated [a:accu] accu) {tst & activated = True}

// Multi-user workflows

delegate :: !UserId !(LabeledTask a) -> Task a | iData a	
delegate newUserId (label,task) = Task delegate` 
where
	delegate` tst =:{TSt | userId = currentUserId}
		# tst		= addUser newUserId tst 
		# (a, tst)	= accTaskTSt (newTask label task) {TSt | tst & userId = newUserId, delegatorId = tst.TSt.userId}
		= (a, {TSt | tst & userId = currentUserId})

// ******************************************************************************************************
// newTask needed for recursive task creation

newTask :: !String !(Task a) -> (Task a) 	| iData a 
newTask taskname mytask = Task newTask`
where
	newTask` tst=:{taskNr,userId,options,activated}		
		| not activated
			= (createDefault, tst)
		# storeName					= iTaskId (incTaskNr taskNr) taskname					
		# (taskval,tst) 			= accHStTSt (mkStoreForm (Init,storageFormId options storeName (False,createDefault)) id) tst	// remember if the task has been done
		# (taskdone,taskvalue)		= taskval.Form.value																			// select values
		| taskdone					= accTaskTSt (mkBasicTask taskname (\tst -> (taskvalue,tst))) tst							// if rewritten, we are a basic task returning a value
		# (a, tst=:{activated})		= accTaskTSt (mkSequenceTask taskname mytask) tst 												// execute task in an isolated sequence
		| activated
			# tst					= deleteSubTasksAndThreads (incTaskNr taskNr) tst																//garbage collect it
			# (_,tst) 				= accHStTSt (mkStoreForm (Init, storageFormId options storeName (False,createDefault)) (\_ -> (True,a))) tst	//remember that the task was done
			= (a, tst)
		| otherwise
			= (a, tst)

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
/*
(-!>) infix 4  :: (Task s) (Task a) -> (Task (Maybe s,Task a)) | iCreateAndPrint s & iCreateAndPrint a
(-!>)  stoptask task =  mkBasicTask "-!>" (Task stop`)
where
	stop` tst=:{taskNr,userId,options,html}
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
*/