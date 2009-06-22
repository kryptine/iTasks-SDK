implementation module BasicCombinators

import StdList, StdArray, StdTuple, StdMisc
from StdFunc import id, const
import dynamic_string, graph_to_string_with_descriptors, graph_to_sapl_string
import TSt
import DrupBasic
import iDataTrivial, iDataFormlib
import LiftingCombinators, ClientCombinators
import Util, Time
import GenBimap
import UserDB

derive gForm 	Time
derive gUpd 	Time
derive gPrint	Time
derive gParse	Time


//Standard monadic operations:

(>>=) infixl 1 :: !(Task a) !(a -> Task b) -> Task b | iCreateAndPrint b
(>>=) taska taskb = Task tbind
where
	tbind tst=:{TSt|options}
		# (a,tst=:{activated})	= accTaskTSt taska tst
		| activated				= accTaskTSt (taskb a) {TSt|tst & options = options}
								= (createDefault,tst)

(>>|) infixl 1 :: !(Task a) (Task b) -> Task b | iCreateAndPrint b
(>>|) taska taskb = taska >>= \_ -> taskb

return :: !a -> (Task a) | iData a
return a  = mkBasicTask "return" (\tst -> (a,tst))

//Repetition and loops:

forever :: !(Task a) -> Task a | iData a
forever task = mkSequenceTask "forever" forever`
where
	forever` tst=:{taskNr} 
		# (val,tst=:{activated})= accTaskTSt task tst					
		| activated		
			# tst = appHStTSt (deleteIData (iTaskId (tl taskNr) "")) tst
			# tst = resetSequence tst
			= forever` tst				
		= (val,tst)

(<!) infixl 6 :: !(Task a) !(a -> .Bool) -> Task a | iData a
(<!) task pred = mkSequenceTask "<!" doTask
where
	doTask tst=:{activated, taskNr}
		# (a,tst=:{activated}) 	= accTaskTSt task tst
		| not activated
			= (a,tst)
		| not (pred a)			
			# tst = appHStTSt (deleteIData (iTaskId (tl taskNr) "")) tst
			# tst = resetSequence tst
			= doTask tst
		= (a,tst)

// Sequential composition

sequence :: !String ![LabeledTask a] -> (Task [a])	| iData a
sequence label options = mkSequenceTask label sequence`
where
	sequence` tst
		= doseqTasks options [] tst

	doseqTasks [] accu tst				= (reverse accu,{tst & activated = True})
	doseqTasks [(taskname,task):ts] accu tst=:{TSt|options} 
		# (a,tst=:{activated=adone}) 
										= accTaskTSt task {tst & activated = True}
		| not adone						= (reverse accu, tst)
		| otherwise						= doseqTasks ts [a:accu] tst


compound :: !String !(Task a) -> (Task a) 	| iData a 
compound label task = mkSequenceTask label compound`
where
	compound` tst = accTaskTSt task tst

// Parallel composition

parallel :: !String !([a] -> Bool) !(Bool [a] -> b) ![LabeledTask a] -> Task b | iData a & iData b
parallel label pred combine tasks 
	= mkParallelTask label (doandTasks tasks)
where
	doandTasks [] tst	=  (combine (if (pred []) True False) [],tst)
	doandTasks taskCollection tst=:{taskNr}
		# (alist,tst=:{exception})	= checkAllTasks taskCollection 0 [] tst
		| isJust exception
			= (createDefault, {tst & activated = False})			// stop, an exception occurred in one of the branches
		| pred alist
			= (combine True alist,{tst & activated = True}) 		// stop, all work done so far satisfies predicate
		| length alist == length taskCollection						// all tasks are done
			= (combine False alist,{tst & activated = True})
		| otherwise	
			= (createDefault, {tst & activated = False})			// show all subtasks using the displayOption function
	where
		checkAllTasks :: ![LabeledTask a] !Int ![a] !*TSt -> (![a],!*TSt) | iData a
		checkAllTasks taskCollection index accu tst
			| index == length taskCollection
				= (reverse accu,tst)												// all tasks tested
			# (taskname,task)					= taskCollection!!index
			# (a,tst=:{activated,exception})	= accTaskTSt (mkSequenceTask taskname (accTaskTSt task)) tst	// check tasks
			| isJust exception
				= ([],tst)						//Stop immediately if a branch has an exception
			| otherwise
				= checkAllTasks taskCollection (inc index) (if activated [a:accu] accu) {tst & activated = True}

// Multi-user workflows

assign :: !UserId !TaskPriority !(Maybe Time) !(LabeledTask a) -> Task a | iData a	
assign toUserId initPriority initDeadline (label,task) = Task assign` 
where
	assign` tst =: {TSt | userId = currentUserId, delegatorId = currentDelegatorId}
		# (toUser,tst)		= accHStTSt (getUser toUserId) tst
		# (currentUser,tst)	= accHStTSt (getUser currentUserId) tst 
		# (now,tst)			= (accHStTSt (accWorldHSt time)) tst						//Retrieve current time
		# mti				= {TaskProperties|processId = 0, subject = label, user = toUser, delegator = currentUser, deadline = initDeadline, priority = initPriority, progress = TPActive, issuedAt = now, firstEvent = Nothing, latestEvent = Nothing}
		# (a, tst)			= accTaskTSt (mkMainTask label mti (accTaskTSt task)) {TSt | tst & userId = toUserId, delegatorId = currentUserId}
		= (a, {TSt | tst & userId = currentUserId, delegatorId = currentDelegatorId})

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