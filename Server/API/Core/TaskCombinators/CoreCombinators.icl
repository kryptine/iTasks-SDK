implementation module CoreCombinators

import StdList, StdArray, StdTuple, StdMisc, StdBool
import TSt, Util, HTTP, GenUpdate, UserDB, Store, Types, Text, TuningCombinators, Shared, MonitorTasks, InteractiveTasks, InteractionTasks, CommonCombinators
from StdFunc			import id, const, o, seq
from CommonCombinators	import transform
from ProcessDB			import :: Process{..}
from ProcessDB			import qualified class ProcessDB(..), instance ProcessDB TSt, instance ProcessDB IWorld

derive class iTask ParallelTaskInfo, SchedulerState, PAction
derive bimap Maybe, (,)

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
			TaskException e str
				= (TaskException e str,tst)

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
					# tst = 'ProcessDB'.deleteSubProcesses (taskNrToString (tl taskNr)) tst
					# tst = resetSequence tst
					# tst = {tst & taskNr = tl taskNr}	
					# tst = setTaskStore "counter" (inc loop) tst
					= doTaskC {tst & taskNr = taskNr}
				| otherwise
					= (TaskFinished a, {tst & taskNr = taskNr})
			TaskException e str
				= (TaskException e str, {tst & taskNr = taskNr})
				
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
			TaskException e str			= (TaskException e str,tst)
	
	description tasks = "Do the following tasks one at a time:<br /><ul><li>" +++ (join "</li><li>" (map taskTitle tasks)) +++ "</li></ul>"
	
// Parallel composition
derive JSONEncode PSt, PStTask
derive JSONDecode PSt, PStTask

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
 		// If all non-control tasks finished return the transformed initial state
		| isEmpty (filter (\(_,task) -> not (isControlTask task)) pst.tasks)
			= (TaskFinished (resultFun AllRunToCompletion initState), tst)
		// Store the internal state
		# tst				= storePSt taskNr pst tst
		# tst = appIWorldTSt flushCache tst
		// The result of the combined evaluation of all parallel subtasks
		= case res of
			//There are still active tasks
			TaskBusy		= (TaskBusy,tst)
			//One the tasks raised an execption
			TaskException e	str = (TaskException e str, tst)
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
				# iworld	= updateTimestamp taskNr iworld
				# tasks		= [(n,InitControlTask n) \\ n <- indexList initCTasks] ++ [(n + length initCTasks,InitTask n) \\ n <- indexList initTasks]
				= (	{ PSt
					| state = initState
					, tasks = (filter isDetached tasks) ++ (filter (not o isDetached) tasks) // initially first start up processes
					, nextIdx = length initTasks + length initCTasks
					},iworld)
	where
		isDetached (_,pstTask) = case (getPStTask taskNr pstTask).Task.containerType of
			DetachedTask _ _	= True
			_					= False
	
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
	
	isControlTask (InitControlTask _)	= True
	isControlTask (AddedControlTask _)	= True
	isControlTask _						= False
	
	updateTimestamp taskNr iworld=:{IWorld|timestamp} = setTaskStoreFor taskNr "lastUpdate" timestamp iworld
	
	sharedParallelState taskNr = Shared read write getTimestamp
	where
		read iworld
			# (pst,iworld)			= loadPSt taskNr iworld
			# (taskInfos,iworld)	= mapSt toParallelTaskInfo pst.tasks iworld
			= (Ok (pst.PSt.state,taskInfos),iworld)
			
		write mprops iworld
			# iworld			= seq (map updateProcProps mprops) iworld
			# iworld			= updateTimestamp taskNr iworld
			= (Ok Void,iworld)
		
		getTimestamp iworld
			# (mbLastUpdate,iworld) = getTaskStoreFor taskNr "lastUpdate" iworld
			= case mbLastUpdate of
				Just lastUpdate	= (Ok lastUpdate,iworld)
				Nothing			= (Error "no timestamp",iworld)
			
		toParallelTaskInfo (idx,pstTask) iworld
			# task							= getPStTask taskNr pstTask
			# (mbProc,iworld)				= 'ProcessDB'.getProcess (taskNrToString [idx:taskNr]) iworld
			# info =	{ index				= idx
						, taskProperties	= task.Task.properties
						, processProperties	= fmap (\{Process|properties} -> properties) mbProc
						, controlTask		= isControlTask pstTask
						}
			= (info,iworld)
				
		updateProcProps (idx,mprops) iworld
			# (_,iworld) = 'ProcessDB'.updateProcessProperties (taskNrToString [idx:taskNr]) (\pprops -> {pprops & managerProperties = mprops}) iworld
			= iworld
		
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
						# (result,tree,tst)	= createOrEvaluateTaskInstance task {tst & taskNr = [idx:taskNr]}
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
					TaskException e str
						//Don't process the other tasks, just let the exception through
						= (TaskException e str,pst,tst)

createOrEvaluateTaskInstance :: !(Task a) !*TSt -> (!TaskResult a, !NonNormalizedTree, !*TSt) | iTask a
createOrEvaluateTaskInstance task tst=:{TSt|taskNr,events}
	//Try to load the stored process for this subtask
	# taskId		 = taskNrToString taskNr
	# (mbProc,tst)	 = 'ProcessDB'.getProcess taskId tst
	= case mbProc of
		//Nothing found, create a task instance
		Nothing	
			# (procId,result,tree,tst)	= createTaskInstance (createThread task) False True False tst
			= case result of
				TaskBusy				= (TaskBusy, tree, tst)
				TaskFinished (a :: a^)	= (TaskFinished a, tree, tst)
				TaskFinished _			= (taskException invalidType,tree,tst)
				TaskException e str		= (TaskException e str, tree, tst)
		//When found, evaluate
		Just proc
			# user				= proc.Process.properties.managerProperties.worker
			// -> TSt in subprocess
			# (result,tree,tst)	= evaluateTaskInstance proc events Nothing False False tst
			// <- TSt back to current process				
			//Add parallel type after the new proc is evaluated
			= case result of
				TaskBusy				= (TaskBusy, tree, tst)
				TaskFinished (a :: a^) 	= (TaskFinished a, tree, tst)
				TaskFinished _			
					= (taskException invalidType, tree, tst)
				TaskException e	str		
					= (TaskException e str, tree, tst)
	where
		invalidType = "createOrEvaluateTaskIntance: task result of invalid type!"

spawnProcess :: !Bool !Bool !(Task a) -> Task (!ProcessId,!SharedProc,!SharedProcResult a) | iTask a
spawnProcess activate gcWhenDone task = mkInstantTask ("Spawn process", "Spawn a new task instance") spawnProcess`
where
	spawnProcess` tst
		# (pid,_,_,tst)	= createTaskInstance (createThread task) True activate gcWhenDone tst
		= (TaskFinished (pid,sharedProc pid,sharedRes pid), tst)
	
	sharedProc pid = makeReadOnlyShared ('ProcessDB'.getProcess pid)
			
	sharedRes pid = makeReadOnlyShared read
	where
		read iworld
			# (mbProc,iworld) = 'ProcessDB'.getProcess pid iworld
			= case mbProc of
				Just {Process|properties} = case properties.systemProperties.status of
					Finished		= loadResult iworld
					_				= (Nothing,iworld)
				Nothing				= loadResult iworld
			
		loadResult iworld
			# (mbResult,iworld)	= loadProcessResult (taskNrFromString pid) iworld
			# mbResult = case mbResult of
				Just (TaskFinished a) = case a of
					(a :: a^)	= Just (Just a)	// proc finished
					_			= Just Nothing	// proc finished but invalid result
				_				= Just Nothing	// proc finished but result deleted
			= (mbResult,iworld)

killProcess :: !ProcessId -> Task Void
killProcess pid = mkInstantTask ("Kill process", "Kill a running task instance") killProcess`
where
	killProcess` tst 
		# tst = deleteTaskInstance pid tst
		= (TaskFinished Void, tst)

scheduledSpawn	:: !(DateTime -> DateTime) !(Task a) -> Task (ReadOnlyShared (!SchedulerState,![ProcessId])) | iTask a
scheduledSpawn when task = abort "not implemented"
