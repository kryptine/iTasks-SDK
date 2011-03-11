implementation module CoreCombinators

import StdList, StdArray, StdTuple, StdMisc, StdBool
import TSt, Util, HTTP, GenUpdate, UserDB, ProcessDB, Store, Types, Text, TuningCombinators, Shared, MonitorTasks, InteractiveTasks, InteractionTasks, CommonCombinators
from ProcessDBTasks		import sharedProcess, sharedProcessResult, class toProcessId, instance toProcessId ProcessRef
from StdFunc			import id, const, o, seq
from TaskTree			import :: TaskParallelType
from CommonCombinators	import transform

derive class iTask SchedulerState, PAction

//Standard monadic operations:
(>>=) infixl 1 :: !(Task a) !(a -> Task b) -> Task b | iTask a & iTask b
(>>=) taska taskb = mkSequenceTask (dotdot (taskTitle taska), taskDescription taska) (tbindE,tbindC)
where
	tbindE tst
		# (result,tst) = applyTaskEdit taska tst
		= case result of
			TaskFinished a	= snd (applyTaskEdit (taskb a) tst)
			_				= tst
			
	tbindC tst
		# (result,tst) = applyTaskCommit taska tst
		= case result of
			TaskBusy
				= (TaskBusy, tst)
			TaskFinished a
				//Pass the argument and do the second part
				= applyTaskCommit (taskb a) tst
			TaskException e
				= (TaskException e,tst)

	dotdot s	= if (endsWith "..." s) s (s +++ "...")

(>>|) infixl 1 :: !(Task a) (Task b) -> Task b | iTask a & iTask b
(>>|) taska taskb = taska >>= \_ -> taskb

return :: !a -> (Task a) | iTask a
return a  = mkInstantTask ("return", "Return a value") (\tst -> (TaskFinished a,tst))

//Repetition and loops:
(<!) infixl 6 :: !(Task a) !(a -> .Bool) -> Task a | iTask a
(<!) task pred = mkSequenceTask (taskTitle task, taskDescription task) (doTaskE,doTaskC)
where
	doTaskE tst=:{taskNr}
		# (loop,tst)				= getCounter tst
		# (result,tst) 				= applyTaskEdit task {tst & taskNr = [loop:tl taskNr]}
		= tst
				
		
	doTaskC tst=:{taskNr} 
		# (loop,tst)	= getCounter tst
		# (result,tst) 	= applyTaskCommit task {tst & taskNr = [loop:tl taskNr]}
		= case result of
			TaskBusy
				= (TaskBusy,tst)
			TaskFinished a
				| not (pred a)	
					# tst = deleteTaskStates (tl taskNr) tst
					# tst = deleteSubProcesses (taskNrToString (tl taskNr)) tst
					# tst = resetSequence tst
					# tst = {tst & taskNr = tl taskNr}	
					# tst = setTaskStore "counter" (inc loop) tst
					= doTaskC {tst & taskNr = taskNr}
				| otherwise
					= (TaskFinished a, {tst & taskNr = taskNr})
			TaskException e
				= (TaskException e, {tst & taskNr = taskNr})
				
	getCounter tst=:{taskNr}
		# (mbLoop,tst)	= getTaskStore "counter" {tst & taskNr = tl taskNr}
		= (fromMaybe 0 mbLoop,tst)
		
// Sequential composition
sequence :: !String ![Task a] -> (Task [a])	| iTask a
sequence label tasks = mkSequenceTask (label, description tasks) (doseqTasksE tasks,\tst -> doseqTasksC tasks [] tst)
where
	doseqTasksE [] tst			= tst
	doseqTasksE [task:ts] tst
		# (result,tst)			= applyTaskEdit task tst
		= case result of
			TaskFinished _		= doseqTasksE ts tst
			_					= tst

	doseqTasksC [] accu tst				= (TaskFinished (reverse accu), tst)
	doseqTasksC [task:ts] accu tst 	
		# (result,tst)					= applyTaskCommit task tst
		= case result of
			TaskBusy					= (TaskBusy,tst)
			TaskFinished a				= doseqTasksC ts [a:accu] tst
			TaskException e				= (TaskException e,tst)
	
	description tasks = "Do the following tasks one at a time:<br /><ul><li>" +++ (join "</li><li>" (map taskTitle tasks)) +++ "</li></ul>"
// Parallel / Grouped composition
derive JSONEncode PSt, PStTask
derive JSONDecode PSt, PStTask
derive bimap Maybe, (,)

:: PSt a b =
	{ state 		:: !b
	, tasks 		:: ![(!Int,!PStTask a b)]
	, nextIdx		:: !Int
	}
	
:: PStTask a acc = InitTask !Int | AddedTask !(Task a) | InitControlTask !Int | AddedControlTask !(CTask a acc)

container :: !TaskContainerType !(Task a) -> Task a | iTask a
container type task = {Task|task & containerType = type}

parallel :: !d !(ValueMerger taskResult pState pResult) ![CTask taskResult pState] ![Task taskResult] -> Task pResult | iTask taskResult & iTask pState & iTask pResult & descr d
parallel d (initState,accuFun,resultFun) initCTasks initTasks
	= mkParallelTask d (parallelE,parallelC)
where
	parallelE tst=:{taskNr}
		| isEmpty initTasks = tst
		# (pst,tst) = accIWorldTSt (loadPSt taskNr) tst
		= processAllTasksE pst 0 tst
		
	processAllTasksE pst n tst=:{taskNr}
		| (length pst.tasks) == n	= tst
		# (idx,task)				= getTaskFromPSt taskNr n pst
		# (_,tst)					= applyTaskEdit task {tst & taskNr = [idx:taskNr]} 
		= processAllTasksE pst (inc n) {tst & taskNr = taskNr}

	parallelC tst=:{taskNr,properties}
		// Load the internal state
		# (pst,tst)			= accIWorldTSt (loadPSt taskNr) tst
		// Evaluate the subtasks for all currently active tasks
 		# (res,pst,tst)		= processAllTasksC pst tst
 		// If all tasks finished return the transformed initial state
		| isEmpty pst.tasks
			= (TaskFinished (resultFun AllRunToCompletion initState), tst)
		// Store the internal state
		# tst				= storePSt taskNr pst tst
		// The result of the combined evaluation of all parallel subtasks
		= case res of
			//There are still active tasks
			TaskBusy		= (TaskBusy,tst)
			//One the tasks raised an execption
			TaskException e	= (TaskException e, tst)
			//The accuFun returned a stop action, or all tasks are completed
			TaskFinished (r,terminationStatus)
				//Remove the extra workers for this parallel combination
				//# tst = clearSubTaskWorkers (taskNrToString taskNr) (Just parType) tst
				= (TaskFinished (resultFun terminationStatus r),tst)
	
	//Load or create the internal state
	loadPSt taskNr iworld
		# (mbPSt,iworld) = getTaskStoreFor taskNr "pst" iworld
		= case mbPSt of
			Just pst
				= (pst,iworld)
			Nothing
				# iworld = updateTimestamp taskNr iworld
				= (	{ PSt
					| state = initState
					, tasks = [(n,InitControlTask n) \\ n <- indexList initCTasks] ++ [(n + length initCTasks,InitTask n) \\ n <- indexList initTasks]
					, nextIdx = length initTasks + length initCTasks
					},iworld)
	
	storePSt taskNr pst tst
		# tst = appIWorldTSt (updateTimestamp taskNr) tst
		= appIWorldTSt (setTaskStoreFor taskNr "pst" pst) tst
		
	getTaskFromPSt taskNr n pst
		# (idx,pstTask) = pst.tasks !! n
		= (idx,getPStTask taskNr pstTask)
			
	getPStTask _		(InitTask n)			= mapTask Left	(initTasks !! n)
	getPStTask _		(AddedTask task)		= mapTask Left	task
	getPStTask taskNr	(InitControlTask n)		= mapTask Right	((initCTasks !! n) (sharedParallelState taskNr))
	getPStTask taskNr	(AddedControlTask task)	= mapTask Right	(task (sharedParallelState taskNr))
	
	updateTimestamp taskNr iworld=:{IWorld|timestamp} = setTaskStoreFor taskNr "lastUpdate" timestamp iworld
	
	sharedParallelState taskNr = Shared read write getTimestamp
	where
		read iworld
			# (state,procs,iworld) = getProcessesAndState iworld
			= (Ok (state,map (\{Process|properties} -> properties) procs),iworld)
			
		write mprops iworld
			# iworld			= updateTimestamp taskNr iworld
			# (_,procs,iworld)	= getProcessesAndState iworld
			= (Ok Void,seq [\iworld ->  snd (updateProcessProperties taskId (\p -> {p & managerProperties = mprop}) iworld) \\ {Process|taskId} <- procs & mprop <- mprops] iworld)
		
		getTimestamp iworld
			# (mbLastUpdate,iworld) = getTaskStoreFor taskNr "lastUpdate" iworld
			= case mbLastUpdate of
				Just lastUpdate	= (Ok lastUpdate,iworld)
				Nothing			= (Error "no timestamp",iworld)
			
		getProcessesAndState iworld
			# (pst,iworld)		= loadPSt taskNr iworld
			# (tasks,state)		= getTasksAndState pst
			# (procs,iworld)	= getProcessesById (map (\(idx,_) -> taskNrToString [idx:taskNr]) tasks) iworld
			= (state,procs,iworld)
		
		getTasksAndState :: !(PSt Void s) -> (![(!Int,!PStTask Void s)],s)
		getTasksAndState {tasks,state} = (tasks,state)
		
	processAllTasksC pst=:{PSt|state,tasks} tst=:{TSt|taskNr,properties}
		= case tasks of
			//We have processed all results
			[] = (TaskBusy, pst, tst)
			//Process another task
			[t=:(idx,pstTask):ts]
				# task = getPStTask taskNr pstTask
				# (result,tst) = case task.Task.containerType of
					DetachedTask _ _
						//IMPORTANT: Task is evaluated with a shifted task number!!!
						# (result,tree,tst)	= createOrEvaluateTaskInstance Nothing task {tst & taskNr = [idx:taskNr]}
						// Add the tree to the current node
						# tst				= addTaskNode tree tst
						= (result,tst)
					_
						= applyTaskCommit task {tst & taskNr = [idx:taskNr]}
				= case result of
					TaskBusy
						//Process the other tasks
						# (result,pst,tst) = processAllTasksC {pst & state = state, tasks = ts} {tst & taskNr = taskNr}
						= (result, {PSt| pst & tasks = [t:pst.tasks]}, {tst & taskNr = taskNr})  
					TaskFinished a
						//Apply the process function
						# (state,mbAction) = case a of
							Left v	= accuFun idx v state
							Right a	= (state,Just a)
						= case mbAction of
							Nothing
								//Process the other tasks
								# (result,pst,tst) = processAllTasksC {pst & state = state, tasks = ts} {tst & taskNr = taskNr}
								= (result, {PSt | pst & tasks = pst.tasks}, {tst & taskNr = taskNr})
								//Process the other tasks extended with the new tasks
							Just Stop
								//Don't process the other tasks, return the state as result
								= (TaskFinished (state,Stopped),pst,tst)
							
							Just (Extend tasks)
								# (result,pst,tst) = processAllTasksC {pst & state = state, tasks = ts ++ [(idx,AddedTask t) \\ t <- tasks & idx <- [pst.nextIdx..]], nextIdx = pst.nextIdx + length tasks + pst.nextIdx} {tst & taskNr = taskNr}
								= (result, {PSt | pst & tasks = pst.tasks}, {tst & taskNr = taskNr})
					TaskException e
						//Don't process the other tasks, just let the exception through
						= (TaskException e,pst,tst)

createOrEvaluateTaskInstance :: !(Maybe TaskParallelType) !(Task a) !*TSt -> (!TaskResult a, !NonNormalizedTree, !*TSt) | iTask a
createOrEvaluateTaskInstance mbpartype task tst=:{TSt|taskNr,events}
	//Try to load the stored process for this subtask
	# taskId		 = taskNrToString taskNr
	# (mbProc,tst)	 = getProcess taskId tst
	= case mbProc of
		//Nothing found, create a task instance
		Nothing	
			# tst				  		= addSubTaskWorker taskId (taskUser task) mbpartype tst
			# (procId,result,tree,tst)	= createTaskInstance (createThread task) False mbpartype True False tst
			= case result of
				TaskBusy				= (TaskBusy, tree, tst)
				TaskFinished (a :: a^)	= (TaskFinished a, tree, tst)
				TaskFinished _			= (TaskException (dynamic "createOrEvaluateTaskIntance: task result of invalid type!"),tree,tst)
				TaskException e			= (TaskException e, tree, tst)
		//When found, evaluate
		Just proc
			//add temp users before(!) the new proc is evaluated, because then the tst still contains the parent info
			# user				= proc.Process.properties.managerProperties.worker
			# tst				= addSubTaskWorker taskId user mbpartype tst
			// -> TSt in subprocess
			# (result,tree,tst)	= evaluateTaskInstance proc events Nothing False False tst
			// <- TSt back to current process				
			//Add parallel type after the new proc is evaluated
			= case result of
				TaskBusy				= (TaskBusy, tree, tst)
				TaskFinished (a :: a^) 	= (TaskFinished a, tree, tst)
				TaskFinished _			
					# tst = removeSubTaskWorker proc.Process.taskId user mbpartype tst
					= (TaskException (dynamic "assign: result of wrong type returned"), tree, tst)
				TaskException e			
					# tst = removeSubTaskWorker proc.Process.taskId user mbpartype tst
					= (TaskException e, tree, tst)

addSubTaskWorker :: !ProcessId !User !(Maybe TaskParallelType) !*TSt -> *TSt
addSubTaskWorker procId user mbpartype tst
		= case mbpartype of
			Nothing 		= tst
			(Just Closed) 	= tst
			(Just Open)		= {TSt | tst & properties = 
								{tst.TSt.properties & systemProperties = 
									{tst.TSt.properties.systemProperties & subTaskWorkers = 
										//filter the process from the current list of subtask workers before adding, as there can be only one worker on a subtask.
										removeDup [(procId,user):(filter (\(pid,_) -> pid <> procId) tst.TSt.properties.systemProperties.subTaskWorkers)]}}} 

removeSubTaskWorker :: !ProcessId !User !(Maybe TaskParallelType) !*TSt -> *TSt			
removeSubTaskWorker procId user mbpartype tst
		= case mbpartype of
			Nothing 		= tst
			(Just Closed) 	= tst
			(Just Open)		= {TSt | tst & properties = {tst.TSt.properties & systemProperties = {tst.TSt.properties.systemProperties & subTaskWorkers = removeMember (procId,user) tst.TSt.properties.systemProperties.subTaskWorkers }}} 

clearSubTaskWorkers :: !ProcessId !(Maybe TaskParallelType) !*TSt -> *TSt
clearSubTaskWorkers procId mbpartype tst
	= case mbpartype of
		Nothing			= tst
		(Just Closed)	= tst
		(Just Open)		= {TSt | tst & properties = {tst.TSt.properties & systemProperties = {tst.TSt.properties.systemProperties & subTaskWorkers = [(pId,u) \\ (pId,u) <- tst.TSt.properties.systemProperties.subTaskWorkers | not (startsWith procId pId)] }}}

spawnProcess :: !Bool !Bool !(Task a) -> Task (ProcessRef a) | iTask a
spawnProcess activate gcWhenDone task = mkInstantTask ("Spawn process", "Spawn a new task instance") spawnProcess`
where
	spawnProcess` tst
		# (pid,_,_,tst)	= createTaskInstance (createThread task) True Nothing activate gcWhenDone tst
		= (TaskFinished (ProcessRef pid), tst)

killProcess :: !(ProcessRef a) -> Task Void | iTask a
killProcess (ProcessRef pid) = mkInstantTask ("Kill process", "Kill a running task instance") killProcess`
where
	killProcess` tst 
		# tst = deleteTaskInstance pid tst
		= (TaskFinished Void, tst)

waitForProcess :: !Bool !(ProcessRef a) -> Task (Maybe a) | iTask a
waitForProcess autoContinue pref =
		monitor ("Wait for task", "Wait for an external task to finish") waitForProcessView waitForProcessPred autoContinue (sharedDescriptionAndStatus pref |+| sharedProcessResult pref)
	>>=	transform snd
	
waitForProcessCancel :: !Bool !(ProcessRef a) -> Task (Maybe a) | iTask a
waitForProcessCancel autoContinue pref =
		monitorA ("Wait for task", "Wait for an external task to finish or cancel") waitForProcessView actions autoEvents (sharedDescriptionAndStatus pref |+| sharedProcessResult pref)
	>>=	transform (maybe Nothing snd o snd)
where
	actions = [(ActionCancel,always)] ++ if autoContinue [] [(ActionContinue,pred`)]
	
	autoEvents v
		| autoContinue && pred` v	= Just ActionContinue
		| otherwise					= Nothing
		
	pred` Invalid	= False
	pred` (Valid v)	= waitForProcessPred v

// map entire process to only description & status before giving it to editor,
// because empty lists in propreties lead to invalid editor states
sharedDescriptionAndStatus pref = mapSharedRead f (sharedProcess pref)
	where
		f Nothing = Nothing
		f (Just {Process|properties=p=:{systemProperties=s=:{status}, taskProperties=t=:{taskDescription}}}) = Just (taskDescription,status)

waitForProcessView (Nothing,res) = finishedView res
waitForProcessView (Just (desc,status),res) = case status of
	Active		= toHtmlDisplay [Text "Waiting for result of task ",title]
	Suspended	= toHtmlDisplay [Text "Task ", title ,Text" is suspended."]
	_			= finishedView res
where
	title = StrongTag [] [Text "\"",Text desc.TaskDescription.title,Text "\""]
	
finishedView Nothing 	= toHtmlDisplay [Text "Task finished."]
finishedView (Just res)	= toHtmlDisplay [Text "Task finished. Result: ", visualizeAsHtmlDisplay res]

waitForProcessPred (Nothing,_) = True
waitForProcessPred (Just (_,status),_) = case status of
	Active		= False
	Suspended	= False
	_			= True

getStatus {Process|properties=p=:{systemProperties=s=:{status}}} = status

scheduledSpawn	:: (DateTime -> DateTime) (Task a) -> Task (ReadOnlyShared (SchedulerState,[ProcessRef a])) | iTask a
scheduledSpawn when task = abort "not implemented"
