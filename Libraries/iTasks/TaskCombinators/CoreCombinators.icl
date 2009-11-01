implementation module CoreCombinators

import	StdList, StdArray, StdTuple, StdMisc, StdBool
from	StdFunc import id, const

import	TSt
import	Util
import	GenUpdate, GenBimap
import	UserDB, ProcessDB, DynamicDB
import	StdDynamic

//Standard monadic operations:

(>>=) infixl 1 :: !(Task a) !(a -> Task b) -> Task b | iTask a & iTask b
(>>=) taska taskb = mkSequenceTask ">>=" tbind
where
	tbind tst
		# (a,tst=:{activated})	= applyTask taska tst
		| activated				= applyTask (taskb a) tst
								= accWorldTSt defaultValue tst

(>>|) infixl 1 :: !(Task a) (Task b) -> Task b | iTask a & iTask b
(>>|) taska taskb = taska >>= \_ -> taskb

return :: !a -> (Task a) | iTask a
return a  = mkInstantTask "return" (\tst -> (a,tst))

//Repetition and loops:

forever :: !(Task a) -> Task a | iTask a
forever task = mkSequenceTask "forever" forever`
where
	forever` tst=:{taskNr} 
		# (val,tst=:{activated})= applyTask task tst					
		| activated		
			# tst = deleteTaskStates (tl taskNr) tst
			# tst = resetSequence tst
			= forever` tst				
		= (val,tst)

(<!) infixl 6 :: !(Task a) !(a -> .Bool) -> Task a | iTask a
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
sequence :: !String ![Task a] -> (Task [a])	| iTask a
sequence label tasks = mkSequenceTask label sequence`
where
	sequence` tst
		= doseqTasks tasks [] tst

	doseqTasks [] accu tst				= (reverse accu,{tst & activated = True})
	doseqTasks [task:ts] accu tst=:{TSt|options} 	
		# (a,tst=:{activated=adone}) 	= applyTask task {tst & activated = True}
		| not adone						= (reverse accu, tst)
		| otherwise						= doseqTasks ts [a:accu] tst

// Parallel composition
parallel :: !String !([a] -> Bool) ([a] -> b) ([a] -> b) ![Task a] -> Task b | iTask a & iTask b
parallel label pred combinePred combineAll tasks 
	= mkParallelTask label (parallel` tasks)
where
	parallel` [] tst	=  (combineAll [], tst)
	parallel` tasks tst
		# (alist,tst=:{exception})	= checkAllTasks tasks 0 [] tst
		| isJust exception
			= accWorldTSt defaultValue {tst & activated = False}// stop, an exception occurred in one of the branches
		| pred alist
			= (combinePred alist,{tst & activated = True}) 	// stop, all work done so far satisfies predicate
		| length alist == length tasks						// all tasks are done
			= (combineAll alist,{tst & activated = True})
		| otherwise
			= accWorldTSt defaultValue {tst & activated = False}// show all subtasks using the displayOption function
	where
		checkAllTasks tasks index accu tst
			| index == length tasks
				= (reverse accu,tst)												// all tasks tested
			# task								= tasks !! index
			# (a,tst=:{activated,exception})	= applyTask (mkSequenceTask (taskLabel task) (applyTask task)) tst	// check tasks
			| isJust exception
				= ([],tst)						//Stop immediately if a branch has an exception
			| otherwise
				= checkAllTasks tasks (inc index) (if activated [a:accu] accu) {tst & activated = True}

assign :: !UserId !TaskPriority !(Maybe Timestamp) !(Task a) -> Task a | iTask a	
assign toUserId initPriority initDeadline task = mkMainTask "assign" (assign` toUserId initPriority initDeadline task) 

import StdDebug

assign` :: !UserId !TaskPriority !(Maybe Timestamp) !(Task a) *TSt -> (a, *TSt) | iTask a
assign` toUserId initPriority initDeadline task tst =: { TSt| taskNr, taskInfo, firstRun, mainTask = currentMainTask, staticInfo = {currentProcessId}
													   , userId = currentUserId, delegatorId = currentDelegatorId, doChange, changes, dataStore, world, activated}
	# dTaskNr 			   = drop 2 taskNr
	# taskId  			   = taskNrToString dTaskNr
	# (mbProc,tst) 		   = trace_n("(Assign) TaskNr "+++taskId+++", Finished: "+++toString taskInfo.finished+++", Activated: "+++toString activated) (getProcess taskId tst)
	# (taskProperties, curTask, dynTask, changeNr, tst)
		= case mbProc of
			(Just {Process | properties, processId, changeNr})
				# (dynTask,tst) = loadTaskFunctionDynamic dTaskNr tst //loadValue (storekey taskNr) dataStore world
				# (curTask,tst) = loadTaskFunctionStatic  dTaskNr tst
				| isNothing curTask && isNothing dynTask
					= abort ("(Assign) No task functions stored for process "+++taskNrToString dTaskNr)
				| isNothing curTask
					= abort ("(Assign) No task static function stored for process "+++taskNrToString dTaskNr)
				| isNothing dynTask
					= abort ("(Assign) No task dynamic function stored for process "+++taskNrToString dTaskNr)
				| otherwise
					= (properties, fromJust curTask, fromJust dynTask, changeNr, tst)		
			Nothing
				# (toUser,tst)		= getUser toUserId tst
				# (currentUser,tst)	= getUser currentUserId tst 
				# (now,tst)			= (accWorldTSt time) tst
				# initProperties	= { systemProps =
									    {TaskSystemProperties
									    | processId 	= taskId
									    , subject		= taskLabel task
									    , manager		= (currentUser.User.userId, currentUser.User.displayName)
									    , issuedAt		= now
									    , firstEvent	= Nothing
									    , latestEvent	= Nothing
									    },
									    managerProps =
									    {TaskManagerProperties
									    | worker		= (toUser.User.userId, toUser.User.displayName)
									    , priority		= initPriority
									    , deadline		= initDeadline
									    },
									    workerProps =
									    {TaskWorkerProperties
									    | progress		= TPActive
									    }
									  }					  
				# process			= mkProcessEntry (taskLabel task) now (toUser.User.userId, toUser.User.displayName) (currentUser.User.userId, currentUser.User.displayName) Active currentMainTask
				# (processId, tst) 	= createProcess  ({Process | process & processId = taskId, properties = initProperties}) tst
				# dynTask			= createDynamicTask task
				# tst				= storeTaskFunctionStatic  dTaskNr task tst
				# tst				= storeTaskFunctionDynamic dTaskNr dynTask tst
				= (initProperties, task, dynTask, 0, tst)
	
	//Apply all active changes (oldest change first, hence the 'reverse changes')
	| firstRun
		= all_changes taskNr taskInfo taskProperties changeNr task curTask dynTask (reverse changes) {TSt|tst & changes = []}
	//Apply the current change change
	| doChange
		= case changes of
			[Just (clt,cid,cdyn):rest]
				= one_change taskNr taskInfo taskProperties changeNr task curTask dynTask (clt,cid,cdyn) rest tst
			other
				= do_task taskNr taskInfo taskProperties changeNr dynTask tst
	| otherwise
		= do_task taskNr taskInfo taskProperties changeNr dynTask tst

//Just execute the task
all_changes :: TaskNr TaskInfo TaskProperties Int (Task a) (Task a) (Task Dynamic) [Maybe (ChangeLifeTime,DynamicId,Dynamic)] *TSt -> (a,*TSt) | iTask a
all_changes taskNr taskInfo taskProperties changeNr origTask curTask dynTask [] tst
	= do_task taskNr taskInfo taskProperties changeNr dynTask tst
	
all_changes taskNr taskInfo taskProperties changeNr origTask curTask dynTask [Just (clt,cid,cdyn):cs] tst=:{TSt|changes}
	# processId = taskNrToString (drop 2 taskNr)
	# (mbProperties,mbTask,mbChange) = appChange cdyn taskProperties curTask origTask
	# changes = case mbChange of
		(Just change)	= [Just (clt,cid,change):changes]
		Nothing			= [Nothing:changes]
	//Update task (and properties when changed) 	
	| isJust mbTask
		# changeNr			= inc changeNr 
		# taskProperties	= if (isJust mbProperties) (fromJust mbProperties) taskProperties
		# curTask			= fromJust mbTask					
		# dynTask			= createDynamicTask curTask
		# tst				= storeTaskFunctionStatic (drop 2 taskNr) curTask tst
		# tst				= storeTaskFunctionDynamic (drop 2 taskNr) dynTask tst
		# (_,tst) 			= updateProcess processId (\p -> {p & properties = taskProperties, changeNr = changeNr}) tst
		# (a,tst)			= do_task taskNr taskInfo taskProperties changeNr dynTask {TSt|tst & changes = changes}
		= case cs of
			[]	-> (a,tst)
			_	-> all_changes taskNr taskInfo taskProperties changeNr origTask curTask dynTask cs {TSt|tst & changes = changes}
	//Only add properties
	| isJust mbProperties
		# taskProperties	= fromJust mbProperties
		# (_,tst) 			= updateProcess processId (\p -> {p & properties = taskProperties}) tst
		# (a,tst) 			= do_task taskNr taskInfo taskProperties changeNr dynTask {TSt|tst & changes = changes}
		= case cs of
			[]	= (a,tst)
			_	= all_changes taskNr taskInfo taskProperties changeNr origTask curTask dynTask cs {TSt|tst & changes = changes}
	// Task and properties unchanged
	| otherwise
		= all_changes taskNr taskInfo taskProperties changeNr origTask curTask dynTask cs {TSt|tst & changes = changes}	

all_changes taskNr taskInfo taskProperties changeNr origTask curTask dynTask [c:cs] tst=:{TSt|changes}
	= all_changes taskNr taskInfo taskProperties changeNr origTask curTask dynTask cs {TSt|tst & changes = [c:changes]}

one_change :: TaskNr TaskInfo TaskProperties Int (Task a) (Task a) (Task Dynamic) (ChangeLifeTime, DynamicId, Dynamic) [Maybe (ChangeLifeTime,DynamicId,Dynamic)] *TSt -> (a,*TSt) | iTask a
one_change taskNr taskInfo taskProperties changeNr origTask curTask dynTask (changeLifeTime, changeId, changeDyn) rest tst
 	# processId = taskNrToString (drop 2 taskNr)
 	# (mbProperties, mbTask, mbChange) = appChange changeDyn taskProperties (setTaskContext [0,changeNr: drop 2 taskNr] curTask) origTask
	//Determine new change list
	# changes = case mbChange of
			(Just change)	= [Just (changeLifeTime,changeId,change):rest]
			Nothing			= [Nothing:rest]
	//Update task (and properties when changed) 	
	| isJust mbTask
		# changeNr			= inc changeNr 
		# taskProperties	= if (isJust mbProperties) (fromJust mbProperties) taskProperties
		# curTask			= fromJust mbTask					
		# (_,tst) 			= updateProcess processId (\p -> {p & properties = taskProperties, changeNr = changeNr}) tst
		# dynTask			= createDynamicTask curTask 										//Convert the changed task with context into a dynamic task
		# tst				= storeTaskFunctionStatic (drop 2 taskNr) curTask tst				//Store the changed task with context
		# tst				= storeTaskFunctionDynamic (drop 2 taskNr) dynTask tst				//Store the changed taks as dynamic
		= do_task taskNr taskInfo taskProperties changeNr dynTask {TSt|tst & changes = changes} //Execute
	//Only add properties
	| isJust mbProperties
		# taskProperties	= fromJust mbProperties
		# (_,tst) 			= updateProcess processId (\p -> {p & properties = taskProperties}) tst
		= do_task taskNr taskInfo taskProperties changeNr dynTask {TSt|tst & changes = changes}
		//= do_task taskNr taskInfo taskProperties changeNr curTask {TSt|tst & changes = changes}
	// Task and properties unchanged
	| otherwise
		= do_task taskNr taskInfo taskProperties changeNr dynTask {TSt|tst & changes = changes}
		//= do_task taskNr taskInfo taskProperties changeNr curTask {TSt|tst & changes = changes}

do_task :: TaskNr TaskInfo TaskProperties Int (Task Dynamic) *TSt -> (a,*TSt) | iTask a
do_task taskNr taskInfo taskProperties changeNr curTask tst=:{userId,delegatorId, mainTask}
	# tst		= {tst & tree = TTMainTask taskInfo taskProperties []
					, taskNr		= [0,changeNr: drop 2 taskNr]
					, mainTask		= taskNrToString (drop 2 taskNr)
					, userId		= fst taskProperties.managerProps.worker
					, delegatorId	= fst taskProperties.systemProps.manager
					}
	# (dyn, tst)		= applyTask curTask tst
	# (def, tst) 		= accWorldTSt defaultValue tst
	# (finished,tst)	= taskFinished tst
	| finished
		= case dyn of
			(val :: a^)	= (val, {TSt | tst & userId = userId, delegatorId = delegatorId, mainTask = mainTask})
			_			= (def, {TSt | tst & userId = userId, delegatorId = delegatorId, mainTask = mainTask})
	| otherwise
		= (def, {TSt | tst & userId = userId, delegatorId = delegatorId, mainTask = mainTask})

//The tricky dynamic part of applying changes
appChange :: !Dynamic !TaskProperties !(Task a) !(Task a) -> (Maybe TaskProperties,Maybe (Task a), Maybe Dynamic) | iTask a
appChange (fun :: A.c: Change c | iTask c) properties curTask origTask
	= fun properties curTask origTask
appChange (fun :: Change a^) properties curTask origTask
	= fun properties curTask origTask
appChange dyn properties curTask origTask
	= (Nothing, Nothing, Just dyn)
	
setTaskContext :: TaskNr (Task a) -> (Task a)
setTaskContext cxt (Task name _ tf) = Task name (Just cxt) tf


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