implementation module CoreCombinators

import StdList, StdArray, StdTuple, StdMisc, StdBool, StdOrdList
import TSt, Util, HTTP, GenUpdate, UserDB, Store, Types, Text, TuningCombinators, Shared, MonitorTasks, InteractiveTasks, InteractionTasks, CommonCombinators
from StdFunc			import id, const, o, seq
from CommonCombinators	import transform
from ProcessDB			import :: Process{..}
from ProcessDB			import qualified class ProcessDB(..), instance ProcessDB TSt, instance ProcessDB IWorld

derive class iTask ParallelTaskInfo, SchedulerState, Control, ControlTaskContainer, TaskContainer
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
	
:: PStTask a acc	= InitTask	!Int | AddedTask	!(!Task (Either a [Control a acc]),!TaskContainerType,!Either (AccuFun a acc) (AccuFunDetached a acc))
:: PStCTask a acc	= InitCTask	!Int | AddedCTask	!(!Task (Either a [Control a acc]),!TaskContainerType)

parallel :: !d !pState !(ResultFun pState pResult) ![ControlTaskContainer taskResult pState] ![TaskContainer taskResult pState] -> Task pResult | iTask taskResult & iTask pState & iTask pResult & descr d
parallel d initState resultFun initCTasks initTasks
	= mkParallelTask d (parallelE,parallelC)
where
	parallelE tst=:{taskNr}
		# (pst,tst)	= accIWorldTSt (loadPSt taskNr) tst
		// first process control tasks
		# tst 		= seqSt (processTaskE o getPStCTask taskNr)	pst.cTasks tst
		# tst 		= seqSt (processTaskE o getPStTask)			pst.tasks tst
		= {tst & taskNr = taskNr}
	where
		processTaskE (idx,task,ctype,_) tst = case ctype of
			CTDetached _ _	= tst
			_				= snd (applyTaskEdit task {tst & taskNr = [idx:taskNr]})
	
	parallelC tst=:{taskNr,properties,newTask}
		// Load the internal state
		# (pst,tst)		= accIWorldTSt (loadPSt taskNr) tst
		# tasks			= map (getPStCTask taskNr) pst.cTasks ++ map getPStTask pst.tasks
		# tst			= if newTask (createProcs tasks tst) tst
		// Evaluate the subtasks for all currently active tasks (control tasks first)
 		# (res,pst,tst)	= processAllTasksC tasks pst tst
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
		processAllTasksC [] pst tst = (TaskBusy,pst,tst)
		processAllTasksC [t=:(idx,task,ctype,accuFun):ts] pst tst
			# tst = {tst & taskNr = [idx:taskNr]} // Task is evaluated with a shifted task number
			# (mbResult,tst) = case ctype of
				CTDetached _ _	= evaluateDetached task tst
				_				= appFst Just (applyTaskCommit task (Just (idx,ctype)) tst)
			= case mbResult of
				Just TaskBusy
					//Process the other tasks
					= processAllTasksC ts pst tst 
				Just (TaskFinished a)
					//Apply the process function
					# (pst,controls,ts,tst) = case a of
						Left v // ordinary task: remove from pst & run accu fun
							# (state,controls) = case accuFun of
								Just (Left fun)		= fun v pst.PSt.state
								Just (Right fun)	= fun (Just v) pst.PSt.state
								_					= abort "no accu fun for ordinary task"
							= ({PSt | removeTasksPSt [idx] pst & state = state},controls,ts,tst)
						Right controls // control task: restart & process control signal
							# tst = deleteTaskStates [idx:taskNr] tst
							# tst = removeFromTree [idx] tst
							= (pst,controls,[t:ts],tst)
					= processControls controls ts pst tst
				Just (TaskException e str)
					//Don't process the other tasks, just let the exception through
					= (TaskException e str,pst,tst)
				Nothing
					// detached cancelled
					# pst = removeTasksPSt [idx] pst
					# (controls,pst) = case accuFun of
						Just (Right accuFun)
							# (state,controls) = accuFun Nothing pst.PSt.state
							= (controls,{PSt|pst & state = state})
						_ // cancelled control task has no effect
							= ([],pst)
					= processControls controls ts pst tst
		where
			processControls [] evalTs pst tst = processAllTasksC evalTs pst tst
			processControls [c:cs] evalTs pst tst = case c of
				StopParallel
					// don't process the other tasks, return the state as result
					= (TaskFinished pst.PSt.state,pst,tst)
				AppendTasks appContainers
					// add new ordinary tasks to PSt & evaluate at end of this iteration
					# newTasks			= mkTasks [(idx,c) \\ c <- appContainers & idx <- [pst.nextIdx..]]
					# pst				= updateNextIdx newTasks pst
					# (pst,tst)			= addTasksPSt newTasks pst tst
					= processControls cs (evalTs ++ newTasks) pst tst
				AppendCTasks appCContainers
					// add new control tasks to PSt & evaluate at end of this iteration
					# newTasks			= mkCTasks [(idx,c) \\ c <- appCContainers & idx <- [pst.nextIdx..]]
					# pst				= updateNextIdx newTasks pst
					# (pst,tst)			= addCTasksPSt newTasks pst tst
					= processControls cs (evalTs ++ newTasks) pst tst
				StopTasks rIdxs
					// removes tasks & add finished nodes to task tree
					# (evalTs,pst,tst)	= removeTasks rIdxs evalTs pst tst
					# tst				= addToTree [TTParallelContainer idx CTInBody (TTFinishedTask (abort "undef task info") (Text "killed",JSONString "killed") False) False \\idx <- rIdxs] tst
					= processControls cs evalTs pst tst
				ResetTasks rIdxs
					// reevaluate reset tasks which have already been evaluated at end of iteration & delete states of all reset tasks
					# reevalTs			= [getPStCTask taskNr t \\ t=:(idx`,_) <- pst.cTasks | isMember idx` rIdxs && idx` < idx] ++ [getPStTask t \\ t=:(idx`,_) <- pst.tasks | isMember idx` rIdxs && idx` < idx]
					# tst				= removeFromTree rIdxs tst
					# tst				= deleteStates rIdxs tst
					= processControls cs (evalTs ++ reevalTs) pst tst
				ReplaceTasks rTasks
					// replace tasks in PSt & evaluate at end of this iteration
					# newTasks			= mkTasks rTasks
					# rIdxs				= map fst rTasks
					# (evalTs,pst,tst)	= removeTasks rIdxs evalTs pst tst
					# (pst,tst)			= addTasksPSt newTasks pst tst
					= processControls cs (evalTs ++ newTasks) pst tst
				ReplaceCTasks rTasks
					// replace tasks in PSt & evaluate at end of this iteration
					# newTasks			= mkCTasks rTasks
					# rIdxs				= map fst rTasks
					# (evalTs,pst,tst)	= removeTasks rIdxs evalTs pst tst
					# (pst,tst)			= addCTasksPSt newTasks pst tst
					= processControls cs (evalTs ++ newTasks) pst tst
				_
					= abort "not implemented!"
			
			// makes new ordinary tasks from containers
			mkTasks containers = [let (task,ctype,accuFun) = fromContainerToTask c in (idx,mapTask Left task,ctype,Just accuFun) \\ (idx,c) <- containers]
			
			// makes new control tasks from containers
			mkCTasks containers = [let (task,ctype) = fromCContainerToTask (sharedParallelState taskNr) c in (idx,mapTask Right (setControlTask task),ctype,Nothing) \\ (idx,c) <- containers]
			
			// removes tasks from list of tasks to evaluate in this iteration, from PSt& from tree and remove their states (and possibly processes)
			removeTasks idxs evalTs pst tst
				# evalTs	= removeFromEvalTasks idxs evalTs
				# pst		= removeTasksPSt idxs pst
				# tst		= removeFromTree idxs tst
				# tst		= deleteStates idxs tst
				= (evalTs,pst,tst)
			
			// removes tasks with given indexes from list of tasks to evaluate in this iteration
			removeFromEvalTasks idxs ts = filter (\(idx,_,_,_) -> not (isMember idx idxs)) ts
			
			// deletes states of tasks (and possibly processes) with given indexes
			deleteStates idxs tst
				# taskNrs	= map (\idx -> [idx:taskNr]) idxs
				# tst		= seqSt (\taskNr tst -> deleteTaskStates taskNr tst)									taskNrs tst
				# tst		= seqSt (\taskNr tst -> snd ('ProcessDB'.deleteProcess (taskNrToString taskNr) tst))	taskNrs tst
				= tst
			
			// adds ordinary tasks to PSt
			addTasksPSt tasks pst tst
				# tst = createProcs tasks tst
				= ({pst & tasks = sortBy (\(a,_) (b,_) -> a < b) (pst.tasks ++ map (\(idx,task,ctype,Just accuFun) -> (idx,AddedTask (task,ctype,accuFun))) tasks)},tst)
			
			// adds control tasks to PSt
			addCTasksPSt cTasks pst tst
				# tst = createProcs cTasks tst
				= ({pst & cTasks = sortBy (\(a,_) (b,_) -> a < b) (pst.cTasks ++ map (\(idx,task,ctype,_) -> (idx,AddedCTask (task,ctype))) cTasks)},tst)
			
			// removes tasks with given indexes from PSt
			removeTasksPSt idxs pst = {pst & tasks = filter (\(idx,_) -> not (isMember idx idxs)) pst.tasks, cTasks = filter (\(idx,_) -> not (isMember idx idxs)) pst.cTasks}
			
			// updates the next task index in PSt
			updateNextIdx l pst = {pst & nextIdx = pst.nextIdx + length l}
			
			// adds given parallel task tree containers to tree
			addToTree containers tst=:{tree} = case tree of
				TTParallelTask ti children = {tst & tree = (TTParallelTask ti (children ++ containers))}
				_ = abort "parallel node expected"
			
			// removes given parallel task tree containers from tree	
			removeFromTree remIdxs tst=:{tree} = case tree of
				TTParallelTask ti children = {tst & tree = (TTParallelTask ti (filter (\(TTParallelContainer idx _ _ _) -> not (isMember idx remIdxs)) children))}
				_ = abort "parallel node expected"
	
		// create processes for detached tasks
		createProcs tasks tst = seqSt createProc tasks tst
		where
			createProc (idx,task,CTDetached managerP menu,_) tst
				# (_,_,_,tst) = createTaskInstance (createThread task) False False managerP menu {tst & taskNr = [idx:taskNr]}
				= tst
			createProc _ tst
				= tst
	
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
		= appIWorldTSt (storeValueAs SFDynamic (iTaskId taskNr "pst") pst) tst
			
	updateTimestamp taskNr iworld=:{IWorld|timestamp} = setTaskStoreFor taskNr "lastUpdate" timestamp iworld
	
	getPStTask (idx,pstTask) = case pstTask of
		InitTask lidx
			# (task,ctype,accuFun) = fromContainerToTask (initTasks !! lidx)
			= (idx,mapTask Left task,ctype,Just accuFun)
		AddedTask (task,ctype,accuFun)
			= (idx,task,ctype,Just accuFun)
	
	getPStCTask taskNr (idx,pstCTask) = case pstCTask of
		InitCTask lidx
			# (task,ctype) = fromCContainerToTask (sharedParallelState taskNr) (initCTasks !! lidx)
			= (idx,mapTask Right (setControlTask task),ctype,Nothing)
		AddedCTask (task,ctype)
			= (idx,task,ctype,Nothing)
			
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
			
		toParallelTaskInfo (idx,task,ctype,_) iworld
			# (mbProc,iworld)				= 'ProcessDB'.getProcess (taskNrToString [idx:taskNr]) iworld
			# info =	{ index				= idx
						, taskProperties	= task.Task.properties
						, processProperties	= fmap (\{Process|properties} -> properties) mbProc
						}
			= (info,iworld)
				
		updateProcProps (idx,mprops) iworld
			# (_,iworld) = 'ProcessDB'.updateProcessProperties (taskNrToString [idx:taskNr]) (\pprops -> {ProcessProperties|pprops & managerProperties = mprops}) iworld
			= iworld
	
	fromCContainerToTask :: !(Shared (!acc,![ParallelTaskInfo]) [(!TaskIndex,!ManagerProperties)]) !(ControlTaskContainer a acc) -> (!Task  [Control a acc],!TaskContainerType)
	fromCContainerToTask s container = case container of
		DetachedCTask p m	ct = (ct s,CTDetached p m)
		WindowCTask w m		ct = (ct s,CTWindow w m)
		DialogCTask w		ct = (ct s,CTDialog w)
		InBodyCTask			ct = (ct s,CTInBody)
		HiddenCTask			ct = (ct s,CTHidden)
		
	fromContainerToTask	:: !(TaskContainer a acc) -> (!Task a,!TaskContainerType,!Either (AccuFun a acc) (AccuFunDetached a acc))
	fromContainerToTask container = case container of
		DetachedTask p m t f	= (t,CTDetached p m,Right f)
		WindowTask w m t f		= (t,CTWindow w m,Left f)
		DialogTask w t f		= (t,CTDialog w,Left f)
		InBodyTask t f			= (t,CTInBody,Left f)
		HiddenTask t f			= (t,CTHidden,Left f)

	evaluateDetached :: !(Task a) !*TSt -> (!Maybe (TaskResult a), !*TSt) | iTask a
	evaluateDetached task tst=:{TSt|taskNr,events}
		//Try to load the stored process for this subtask
		# (mbProc,tst)	 = 'ProcessDB'.getProcess (taskNrToString taskNr) tst
		= case mbProc of
			//Nothing found, process cancelled
			Nothing	
				= (Nothing,tst)
			//When found, evaluate
			Just proc
				# (result,_,tst) = evaluateTaskInstance proc events Nothing False False tst
				= case result of
					TaskBusy				= (Just TaskBusy,tst)
					TaskFinished (a :: a^) 	= (Just (TaskFinished a),tst)
					TaskFinished _			= (Just (taskException invalidType),tst)
					TaskException e	str		= (Just (TaskException e str),tst)
	where
		invalidType = "createOrEvaluateTaskIntance: task result of invalid type!"

spawnProcess :: !Bool !ManagerProperties !ActionMenu !(Task a) -> Task (!ProcessId,!SharedProc,!SharedProcResult a) | iTask a
spawnProcess gcWhenDone managerProperties menu task = mkInstantTask ("Spawn process", "Spawn a new task instance") spawnProcess`
where
	spawnProcess` tst
		# (pid,_,_,tst)	= createTaskInstance (createThread task) True gcWhenDone managerProperties menu tst
		= (TaskFinished (pid,sharedProc pid,sharedRes pid), tst)
	
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
