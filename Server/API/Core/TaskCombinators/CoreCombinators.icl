implementation module CoreCombinators

import StdList, StdArray, StdTuple, StdMisc, StdBool
import TSt, Util, HTTP, GenUpdate, UserDB, Store, Types, Text, TuningCombinators, Shared, MonitorTasks, InteractiveTasks, InteractionTasks, CommonCombinators
from StdFunc			import id, const, o, seq
from CommonCombinators	import transform
from ProcessDB			import :: Process{..}
from ProcessDB			import qualified class ProcessDB(..), instance ProcessDB TSt, instance ProcessDB IWorld

derive class iTask ParallelTaskInfo, SchedulerState, Control, ParamTaskContainer
// Generic functions for menus not needed because only functions generating menus (no actual menu structures) are serialised
JSONEncode{|Menu|} _		= abort "not implemented"
JSONEncode{|MenuItem|} _	= abort "not implemented"
JSONDecode{|Menu|} _		= abort "not implemented"
JSONDecode{|MenuItem|} _	= abort "not implemented"
gEq{|Menu|} _ _				= abort "not implemented"
gEq{|MenuItem|} _ _			= abort "not implemented"
gVisualize{|Menu|} _ _		= abort "not implemented"
gVisualize{|MenuItem|} _ _	= abort "not implemented"
gVerify{|Menu|} _ _			= abort "not implemented"
gVerify{|MenuItem|} _ _		= abort "not implemented"
gUpdate{|Menu|} _ _			= abort "not implemented"
gUpdate{|MenuItem|} _ _		= abort "not implemented"
gDefaultMask{|Menu|} _		= abort "not implemented"
gDefaultMask{|MenuItem|} _	= abort "not implemented"
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
		# (result,tst) = applyTaskCommit taska Nothing tst
		= case result of
			TaskBusy
				= (TaskBusy, tst)
			TaskFinished a
				//Pass the argument and do the second part
				= applyTaskCommit (taskb a) Nothing tst
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
		# (result,tst) 	= applyTaskCommit task Nothing {tst & taskNr = [loop:tl taskNr]}
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
		# (result,tst)					= applyTaskCommit task Nothing tst
		= case result of
			TaskBusy					= (TaskBusy,tst)
			TaskFinished a				= doseqTasksC ts [a:accu] tst
			TaskException e str			= (TaskException e str,tst)
	
	description tasks = "Do the following tasks one at a time:<br /><ul><li>" +++ (join "</li><li>" (map taskTitle tasks)) +++ "</li></ul>"
	
// Parallel composition
derive JSONEncode PSt, PStTask, PStCTask, TaskContainerType
derive JSONDecode PSt, PStTask, PStCTask, TaskContainerType

:: PSt a acc =
	{ state 	:: !acc
	, tasks 	:: ![(!TaskIndex,!PStTask a acc)]
	, cTasks	:: ![(!TaskIndex,!PStCTask a acc)]
	, nextIdx	:: !TaskIndex
	}
	
:: PStTask a acc	= InitTask	!Int | AddedTask	!(!Task (Either a (Control a acc)),!TaskContainerType)
:: PStCTask a acc	= InitCTask	!Int | AddedCTask	!(!Task (Either a (Control a acc)),!TaskContainerType)

parallel :: !d !(ValueMerger taskResult pState pResult) ![ControlTaskContainer taskResult pState] ![TaskContainer taskResult] -> Task pResult | iTask taskResult & iTask pState & iTask pResult & descr d
parallel d (initState,accuFun,resultFun) initCTasks initTasks
	= mkParallelTask d (parallelE,parallelC)
where
	parallelE tst=:{taskNr}
		# (pst,tst)	= accIWorldTSt (loadPSt taskNr) tst
		// first process control tasks
		# tst 		= seqSt (processTaskE o getPStCTask taskNr)	pst.cTasks tst
		# tst 		= seqSt (processTaskE o getPStTask)			pst.tasks tst
		= {tst & taskNr = taskNr}
	where
		processTaskE (idx,task,ctype) tst = case ctype of
			CTDetached _ _	= tst
			_				= snd (applyTaskEdit task {tst & taskNr = [idx:taskNr]})
	
	parallelC tst=:{taskNr,properties,newTask}
		// Load the internal state
		# (pst,tst)		= accIWorldTSt (loadPSt taskNr) tst
		# tst			= if newTask (initProcs pst tst) tst
		// Evaluate the subtasks for all currently active tasks (control tasks first)
 		# (res,pst,tst)	= processAllTasksC (map (getPStCTask taskNr) pst.cTasks ++ map getPStTask pst.tasks) pst tst
		// The result of the combined evaluation of all parallel subtasks
		# (res,tst) = case res of
			//There are still active tasks
			TaskBusy
				// If all non-control tasks finished return the transformed initial state
				| isEmpty pst.tasks
					# tst = 'ProcessDB'.deleteSubProcesses (taskNrToString taskNr) tst
					= (TaskFinished (resultFun AllRunToCompletion pst.PSt.state), tst)
				| otherwise
					// Store the internal state
					# tst = storePSt taskNr pst tst
					= (TaskBusy,tst)
			//One of the tasks raised an execption
			TaskException e	str
				# tst = 'ProcessDB'.deleteSubProcesses (taskNrToString taskNr) tst
				= (TaskException e str, tst)
			//The accuFun returned a stop action
			TaskFinished r
				# tst = 'ProcessDB'.deleteSubProcesses (taskNrToString taskNr) tst
				= (TaskFinished (resultFun Stopped r),tst)
		= (res,{tst & taskNr = taskNr})
	where
		initProcs pst tst
			# tst = seqSt (filterF getPStTask) pst.tasks tst
			# tst = seqSt (filterF (getPStCTask taskNr)) pst.cTasks tst
			= tst
		where
			filterF getF task tst = case getF task of
				(idx,task,ctype=:CTDetached _ _)	= thd3 (createOrEvaluateTaskInstance task ctype {tst & taskNr = [idx:taskNr]})
				_									= tst
	
		processAllTasksC [] pst tst = (TaskBusy,pst,tst)
		
		processAllTasksC [t=:(idx,task,ctype):ts] pst=:{PSt|state,tasks,cTasks} tst
			# tst = {tst & taskNr = [idx:taskNr]} // Task is evaluated with a shifted task number
			# (result,tst) = case ctype of
				CTDetached _ _
					# (result,tree,tst)	= createOrEvaluateTaskInstance task ctype tst
					= (result,tst)
				_
					= applyTaskCommit task (Just (idx,ctype)) tst
			= case result of
				TaskBusy
					//Process the other tasks
					= processAllTasksC ts pst tst 
				TaskFinished a
					//Apply the process function
					# (pst,mbAction,ts,tst) = case a of
						Left v // normal task: remove from pst & run accu fun
							# (state,mbAction) = accuFun idx v state
							= ({pst & tasks = removeTaskPSt idx tasks, state = state},mbAction,ts,tst)
						Right a // control task: restart & generate action
							# tst = deleteTaskStates [idx:taskNr] tst
							# tst = removeFromTree [idx] tst
							= (pst,Just a,[t:ts],tst)
					= case mbAction of
						Nothing
							//Process the other tasks
							= processAllTasksC ts pst tst
						Just StopParallel
							//Don't process the other tasks, return the state as result
							= (TaskFinished state,pst,tst)
						Just (AppendTasks newContainers)
							//Process the other tasks extended with the new tasks
							# newTasks	= [let (task,ctype) = fromContainerToTask c in (idx,mapTask Left task,ctype) \\ c <- newContainers & idx <- [pst.nextIdx..]]
							# pst		= {pst & tasks = tasks ++ map (\(idx,task,ctype) -> (idx,AddedTask (task,ctype))) newTasks, nextIdx = pst.nextIdx + length tasks}
							= processAllTasksC (ts ++ newTasks) pst tst
						Just (AppendCTasks newContainers)
							# newTasks	= [let (task,ctype) = fromContainerToTask (applyParam (sharedParallelState taskNr) c) in (idx,mapTask Right (setControlTask task),ctype) \\ c <- newContainers & idx <- [pst.nextIdx..]]
							# pst		= {pst & cTasks = cTasks ++ map (\(idx,task,ctype) -> (idx,AddedCTask (task,ctype))) newTasks, nextIdx = pst.nextIdx + length tasks}
							= processAllTasksC (ts ++ newTasks) pst tst
						Just (StopTasks remIdxs)
							# ts		= filter (\(idx,_,_) -> not (isMember idx remIdxs)) ts
							# pst		= {pst & tasks = filter (\(idx,_) -> not (isMember idx remIdxs)) tasks, cTasks = filter (\(idx,_) -> not (isMember idx remIdxs)) cTasks}
							# tst		= removeFromTree remIdxs tst
							# tst		= addToTree [TTContainer idx CTInBody (TTFinishedTask (abort "undef task info") (Text "killed",JSONString "killed") False) False \\idx <- remIdxs] tst
							# tst		= seqSt (\taskId tst -> snd ('ProcessDB'.deleteProcess taskId tst)) (map (\idx -> taskNrToString [idx:taskNr]) remIdxs) tst
							= processAllTasksC ts pst tst
						_
							= abort "not implemented!"
				TaskException e str
					//Don't process the other tasks, just let the exception through
					= (TaskException e str,pst,tst)
		where
			removeTaskPSt delIdx tasks = filter (\(idx,_) -> idx <> delIdx) tasks
			
			addToTree containers tst=:{tree} = case tree of
				TTParallelTask ti children = {tst & tree = (TTParallelTask ti (children ++ containers))}
				_ = abort "parallel node expected"
				
			removeFromTree remIdxs tst=:{tree} = case tree of
				TTParallelTask ti children = {tst & tree = (TTParallelTask ti (filter (\(TTContainer idx _ _ _) -> not (isMember idx remIdxs)) children))}
				_ = abort "parallel node expected"
	
	//Load or create the internal state
	loadPSt taskNr iworld
		# (mbPSt,iworld) = getTaskStoreFor taskNr "pst" iworld
		= case mbPSt of
			Just pst
				= (pst,iworld)
			Nothing
				# iworld	= updateTimestamp taskNr iworld
				# cTasks	= [(n,InitCTask n)						\\ n <- indexList initCTasks]
				# tasks		= [(n + length initCTasks,InitTask n)	\\ n <- indexList initTasks]
				= (	{ PSt
					| state = initState
					, tasks = tasks
					, cTasks = cTasks
					, nextIdx = length initTasks + length initCTasks
					},iworld)
					
	storePSt taskNr pst tst
		# tst = appIWorldTSt (updateTimestamp taskNr) tst
		= appIWorldTSt (setTaskStoreFor taskNr "pst" pst) tst
			
	updateTimestamp taskNr iworld=:{IWorld|timestamp} = setTaskStoreFor taskNr "lastUpdate" timestamp iworld
	
	getPStTask (idx,pstTask) = case pstTask of
		InitTask lidx
			# (task,ctype) = fromContainerToTask (initTasks !! lidx)
			= (idx,mapTask Left task,ctype)
		AddedTask (task,ctype)
			= (idx,task,ctype)
	
	getPStCTask taskNr (idx,pstCTask) = case pstCTask of
		InitCTask lidx
			# (task,ctype) = fromContainerToTask (applyParam (sharedParallelState taskNr) (initCTasks !! lidx))
			= (idx,mapTask Right (setControlTask task),ctype)
		AddedCTask (task,ctype)
			= (idx,task,ctype)
			
	setControlTask task=:{Task|properties} = {Task|task & properties = {properties & isControlTask = True}}
				
	sharedParallelState taskNr = Shared read write getTimestamp
	where
		read iworld
			# (pst,iworld)			= loadPSt taskNr iworld
			# (cTaskInfos,iworld)	= mapSt (toParallelTaskInfo o getPStCTask taskNr)	pst.cTasks iworld
			# (taskInfos,iworld)	= mapSt (toParallelTaskInfo o getPStTask)			pst.tasks iworld
			= (Ok (pst.PSt.state, cTaskInfos ++ taskInfos),iworld)
			
		write mprops iworld
			# iworld			= seq (map updateProcProps mprops) iworld
			# iworld			= updateTimestamp taskNr iworld
			= (Ok Void,iworld)
		
		getTimestamp iworld
			# (mbLastUpdate,iworld) = getTaskStoreFor taskNr "lastUpdate" iworld
			= case mbLastUpdate of
				Just lastUpdate	= (Ok lastUpdate,iworld)
				Nothing			= (Error "no timestamp",iworld)
			
		toParallelTaskInfo (idx,task,ctype) iworld
			# (mbProc,iworld)				= 'ProcessDB'.getProcess (taskNrToString [idx:taskNr]) iworld
			# info =	{ index				= idx
						, taskProperties	= task.Task.properties
						, processProperties	= fmap (\{Process|properties} -> properties) mbProc
						}
			= (info,iworld)
				
		updateProcProps (idx,mprops) iworld
			# (_,iworld) = 'ProcessDB'.updateProcessProperties (taskNrToString [idx:taskNr]) (\pprops -> {pprops & managerProperties = mprops}) iworld
			= iworld

createOrEvaluateTaskInstance :: !(Task a) !TaskContainerType !*TSt -> (!TaskResult a, !NonNormalizedTree, !*TSt) | iTask a
createOrEvaluateTaskInstance task containerType tst=:{TSt|taskNr,events}
	//Try to load the stored process for this subtask
	# taskId		 = taskNrToString taskNr
	# (mbProc,tst)	 = 'ProcessDB'.getProcess taskId tst
	= case mbProc of
		//Nothing found, create a task instance
		Nothing	
			# (procId,result,tree,tst)	= createTaskInstance (createThread task) False False containerType tst
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

spawnProcess :: !Bool !(TaskContainer a) -> Task (!ProcessId,!SharedProc,!SharedProcResult a) | iTask a
spawnProcess gcWhenDone container = mkInstantTask ("Spawn process", "Spawn a new task instance") spawnProcess`
where
	spawnProcess` tst
		# (pid,_,_,tst)	= createTaskInstance (createThread task) True gcWhenDone containerType tst
		= (TaskFinished (pid,sharedProc pid,sharedRes pid), tst)
		
	(task,containerType) = fromContainerToTask container
	
	sharedProc pid = makeReadOnlyShared ('ProcessDB'.getProcess pid)
			
	sharedRes pid = makeReadOnlyShared read
	where
		read iworld
			# (mbProc,iworld) = 'ProcessDB'.getProcess pid iworld
			= case mbProc of
				Just {Process|properties} = case properties.systemProperties.SystemProperties.status of
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
