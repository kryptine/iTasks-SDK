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
import UserDB, ProcessDB, DynamicDB

derive gForm 	Time
derive gUpd 	Time
derive gPrint	Time
derive gParse	Time


//Standard monadic operations:

(>>=) infixl 1 :: !(Task a) !(a -> Task b) -> Task b | iData a & iData b
(>>=) taska taskb = mkSequenceTask ">>=" tbind
where
	tbind tst
		# (a,tst=:{activated})	= applyTask taska tst
		| activated				= applyTask (taskb a) tst
								= (createDefault,tst)

(>>|) infixl 1 :: !(Task a) (Task b) -> Task b | iData a & iData b
(>>|) taska taskb = taska >>= \_ -> taskb

return :: !a -> (Task a) | iData a
return a  = mkBasicTask "return" (\tst -> (a,tst))

//Repetition and loops:

forever :: !(Task a) -> Task a | iData a
forever task = mkSequenceTask "forever" forever`
where
	forever` tst=:{taskNr} 
		# (val,tst=:{activated})= applyTask task tst					
		| activated		
			# tst = deleteTaskStates (tl taskNr) tst
			# tst = resetSequence tst
			= forever` tst				
		= (val,tst)

(<!) infixl 6 :: !(Task a) !(a -> .Bool) -> Task a | iData a
(<!) task pred = mkSequenceTask "<!" doTask
where
	doTask tst=:{activated, taskNr}
		# (a,tst=:{activated}) 	= applyTask task tst
		| not activated
			= (a,tst)
		| not (pred a)			
			# tst = deleteTaskStates (tl taskNr) tst
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
										= applyTask task {tst & activated = True}
		| not adone						= (reverse accu, tst)
		| otherwise						= doseqTasks ts [a:accu] tst


compound :: !String !(Task a) -> (Task a) 	| iData a 
compound label task = mkSequenceTask label compound`
where
	compound` tst = applyTask task tst

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
			# (a,tst=:{activated,exception})	= applyTask (mkSequenceTask taskname (applyTask task)) tst	// check tasks
			| isJust exception
				= ([],tst)						//Stop immediately if a branch has an exception
			| otherwise
				= checkAllTasks taskCollection (inc index) (if activated [a:accu] accu) {tst & activated = True}

// Multi-user workflows

assign :: !UserId !TaskPriority !(Maybe Time) !(LabeledTask a) -> Task a | iData a	
assign toUserId initPriority initDeadline (label,task) = mkMainTask "assign" assign` 
where
	assign` tst =: {TSt| taskNr, taskInfo, mainTask = currentMainTask, staticInfo = {currentProcessId}, userId = currentUserId, delegatorId = currentDelegatorId}
		# taskId			= taskNrToString taskNr
		# (mbProc,tst)		= getSubProcess currentProcessId taskId tst
		# (taskProperties, processId, curTask, curTaskId, changeNr, tst=:{doChange,changes})
			= case mbProc of
				(Just {Process | properties, processId, taskfun, changeNr})
					| isNothing taskfun
						= (properties, processId, task, 0, changeNr, tst)
					| otherwise
						# tid			= fromJust taskfun
						# (mbTask,tst)	= getDynamic tid tst
						= case mbTask of
							(Just (t :: Task a^))	= (properties, processId, t, tid, changeNr, tst)
							_						= (properties, processId, task, 0, changeNr, tst)
				Nothing
					# (toUser,tst)		= getUser toUserId tst
					# (currentUser,tst)	= getUser currentUserId tst 
					# (now,tst)			= (accHStTSt (accWorldHSt time)) tst
					# initProperties	= {TaskProperties|processId = 0, subject = label, user = toUser, delegator = currentUser
							  , deadline = initDeadline, priority = initPriority, progress = TPActive
							  , issuedAt = now, firstEvent = Nothing, latestEvent = Nothing}
					# (processId, tst)	= createProcess (mkEmbeddedProcessEntry currentProcessId taskId initProperties Active currentMainTask) tst		  
					= (initProperties, processId, task, 0, 0, tst)
		//Apply a change
		| doChange
			= case changes of
				[(clabel,cid,(Change cfun :: Change a^)):rest]	= do_change processId taskInfo taskProperties changeNr curTask curTaskId (clabel,cid,cfun) rest tst
				other											= do_task processId taskInfo taskProperties taskNr changeNr curTask tst
		| otherwise
			= do_task processId taskInfo taskProperties taskNr changeNr curTask tst
		where
			do_change processId taskInfo taskProperties changeNr curTask curTaskId (changeLabel, changeId, changeFun) rest tst
			 	# (mbProperties, mbTask, mbChange) = changeFun taskProperties (setTaskContext [0,changeNr: drop 2 taskNr] curTask) task
				//Determine new change list
				# changes = case mbChange of
						(Just change)	= [(changeLabel,changeId,dynamic change):rest]
						_				= rest
				//Update task (and properties when changed) 	
				| isJust mbTask
					# changeNr			= inc changeNr 
					# taskProperties	= if (isJust mbProperties) (fromJust mbProperties) taskProperties
					# curTask			= fromJust mbTask					
					# (curTaskId,tst)	= updateTaskDynamic curTaskId (dynamic curTask) tst 
					# (_,tst) 			= updateProcess processId (\p -> {p & taskfun = Just curTaskId, properties = taskProperties, changeNr = changeNr}) tst
					= do_task processId taskInfo taskProperties taskNr changeNr curTask {TSt|tst & changes = changes} 
				//Only add properties
				| isJust mbProperties
					# taskProperties	= fromJust mbProperties
					# (_,tst) 			= updateProcess processId (\p -> {p & properties = taskProperties}) tst
					= do_task processId taskInfo taskProperties taskNr changeNr curTask {TSt|tst & changes = changes}
				// Task and properties unchanged
				| otherwise
					= do_task processId taskInfo taskProperties taskNr changeNr curTask {TSt|tst & changes = changes}
			where
				setTaskContext cxt (Task name _ tf) = Task name (Just cxt) tf
				
				updateTaskDynamic 0 d tst
					= createDynamic d tst
				updateTaskDynamic i d tst
					# (_,tst) = updateDynamic d i tst
					= (i,tst)
			
 			do_task processId taskInfo taskProperties taskNr changeNr curTask tst
 				# tst		= {tst & tree = TTMainTask taskInfo taskProperties []
 								, taskNr		= [0,changeNr: drop 2 taskNr]
 								, mainTask		= processId
 								, userId		= fst taskProperties.TaskProperties.user
 								, delegatorId	= fst taskProperties.TaskProperties.delegator
 								}
 				# (a, tst)	= applyTask curTask tst
 				= (a, {TSt | tst & userId = currentUserId, delegatorId = currentDelegatorId, mainTask = currentMainTask})

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