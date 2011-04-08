implementation module CoreCombinators

import StdList, StdArray, StdTuple, StdMisc, StdBool, StdOrdList
import TSt, Util, HTTP, GenUpdate, UserDB, Store, Types, Text, TuningCombinators, Shared, MonitorTasks, InteractiveTasks, InteractionTasks, CommonCombinators
from StdFunc			import id, const, o, seq
from CommonCombinators	import transform
from ProcessDB			import :: Process{..}
from ProcessDB			import qualified class ProcessDB(..), instance ProcessDB TSt, instance ProcessDB IWorld
from iTasks				import JSONEncode, JSONDecode

derive class iTask ParallelTaskInfo, SchedulerState, Control, ControlTaskContainer
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

JSONEncode{|TaskContainer|} _ c		= dynamicJSONEncode c
JSONDecode{|TaskContainer|} _ [j:c]	= (dynamicJSONDecode j,c)
gUpdate{|TaskContainer|} fx UDCreate ust
	# (a,ust) = fx UDCreate ust
	= (InBodyTask (return Void) (\_ _ -> (a,[])),ust)
gUpdate{|TaskContainer|} _ (UDSearch t) ust = basicSearch t (\_ t -> t) ust
gDefaultMask{|TaskContainer|} _ _ = [Touched []]
gVerify{|TaskContainer|} _ _ vst = alwaysValid vst
gVisualize{|TaskContainer|} _ _ vst = ([TextFragment "task container"],vst)
gEq{|TaskContainer|} _ _ _ = False // containers are never equal

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
// JSON not need for PSt because it's stored as dynamic
JSONEncode{|PSt|} _ _ = abort "not implemented"
JSONDecode{|PSt|} _ _ = abort "not implemented"

:: PSt acc =
	{ state 	:: !acc
	, tasks 	:: ![(!TaskIndex,!PStTask acc)]
	, cTasks	:: ![(!TaskIndex,!PStCTask acc)]
	, nextIdx	:: !TaskIndex
	}
	
:: PStTask acc	= InitTask	!Int | E.a:	AddedTask	!(Task a) !(Either (AccuFun a acc) (AccuFunDetached a acc)) !TaskContainerType & iTask a
:: PStCTask acc	= InitCTask	!Int | 		AddedCTask	!(Task [Control acc]) !TaskContainerType

:: ParTaskInfo acc :== (!TaskIndex,!ParTask acc,!TaskContainerType)
:: ParTask acc = E.a: PTask !(Task a) !(Either (AccuFun a acc) (AccuFunDetached a acc)) & iTask a | PCTask !(Task [Control acc])

parallel :: !d !pState !(ResultFun pState pResult) ![ControlTaskContainer pState] ![TaskContainer pState] -> Task pResult | iTask pState & iTask pResult & descr d
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
		processTaskE (idx,task,ctype) tst = case ctype of
			CTDetached _ _	= tst
			_ = case task of
				PTask t _	= snd (applyTaskEdit t {tst & taskNr = [idx:taskNr]})
				PCTask t	= snd (applyTaskEdit t {tst & taskNr = [idx:taskNr]})
	
	parallelC tst=:{taskNr,properties,newTask}
		// Load the internal state
		# (pst,tst)		= accIWorldTSt (loadPSt taskNr) tst
		# tasks			= map (getPStCTask taskNr) pst.cTasks ++ map getPStTask pst.tasks
		# tst			= if newTask (createProcs taskNr tasks tst) tst
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
		processAllTasksC [t=:(idx,task,ctype):ts] pst tst
			# tst = {tst & taskNr = [idx:taskNr]} // Task is evaluated with a shifted task number
			# (res,reeval,pst,tst) = case ctype of
				CTDetached _ _	= evaluateDetached task idx ctype pst tst
				_				= evaluateLocal task idx ctype pst tst
			# ts = if reeval [t:ts] ts
			= case res of
				TaskBusy
					//Process the other tasks
					= processAllTasksC ts pst tst 
				TaskFinished controls
					= processControls controls ts pst tst
				TaskException e str
					//Don't process the other tasks, just let the exception through
					= (TaskException e str,pst,tst)
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
					# (pst,tst)			= addTasksPSt newTasks pst tst
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
					# (pst,tst)			= addTasksPSt newTasks pst tst
					= processControls cs (evalTs ++ newTasks) pst tst
				_
					= abort "not implemented!"
			
			// makes new ordinary tasks from containers
			mkTasks containers = [let (task,ctype) = fromContainerToTask c in (idx,task,ctype) \\ (idx,c) <- containers]
			
			// makes new control tasks from containers
			mkCTasks containers = [let (task,ctype) = fromCContainerToTask (sharedParallelState taskNr) c in (idx,task,ctype) \\ (idx,c) <- containers]
			
			// removes tasks from list of tasks to evaluate in this iteration, from PSt& from tree and remove their states (and possibly processes)
			removeTasks idxs evalTs pst tst
				# evalTs	= removeFromEvalTasks idxs evalTs
				# pst		= removeTasksPSt idxs pst
				# tst		= removeFromTree idxs tst
				# tst		= deleteStates idxs tst
				= (evalTs,pst,tst)
			
			// removes tasks with given indexes from list of tasks to evaluate in this iteration
			removeFromEvalTasks idxs ts = filter (\(idx,_,_) -> not (isMember idx idxs)) ts
			
			// deletes states of tasks (and possibly processes) with given indexes
			deleteStates idxs tst
				# taskNrs	= map (\idx -> [idx:taskNr]) idxs
				# tst		= seqSt (\taskNr tst -> deleteTaskStates taskNr tst)									taskNrs tst
				# tst		= seqSt (\taskNr tst -> snd ('ProcessDB'.deleteProcess (taskNrToString taskNr) tst))	taskNrs tst
				= tst
			
			// adds control and ordinary tasks to PSt
			addTasksPSt tasks pst tst
				# tst = createProcs taskNr tasks tst
				# pst = seqSt addTask tasks pst
				# pst = {pst & tasks	= sortBy (\(a,_) (b,_) -> a < b) pst.tasks}
				# pst = {pst & cTasks	= sortBy (\(a,_) (b,_) -> a < b) pst.cTasks}
				= (pst,tst)
			where
				addTask (idx,PTask task accufun,ctype)	pst = {pst & tasks	= [(idx,AddedTask task accufun ctype):pst.tasks]}
				addTask (idx,PCTask task,ctype)			pst = {pst & cTasks	= [(idx,AddedCTask task ctype):pst.cTasks]}
			
			// updates the next task index in PSt
			updateNextIdx l pst = {pst & nextIdx = pst.nextIdx + length l}
			
			// adds given parallel task tree containers to tree
			addToTree containers tst=:{tree} = case tree of
				TTParallelTask ti children = {tst & tree = (TTParallelTask ti (children ++ containers))}
				_ = abort "parallel node expected"
	
	// create processes for detached tasks
	createProcs taskNr tasks tst = seqSt createProc tasks tst
	where
		createProc (idx,ptask,CTDetached managerP menu) tst
			# thread = case ptask of
				PTask task _	= createThread task
				PCTask task		= createThread task
			# (_,_,_,tst) = createTaskInstance thread False False managerP menu {tst & taskNr = [idx:taskNr]}
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
		= appIWorldTSt (storeValueAs SFDynamic(iTaskId taskNr "pst") pst) tst
			
	updateTimestamp taskNr iworld=:{IWorld|timestamp} = setTaskStoreFor taskNr "lastUpdate" timestamp iworld
	
	getPStTask (idx,pstTask) = case pstTask of
		InitTask lidx
			# (task,ctype) = fromContainerToTask (initTasks !! lidx)
			= (idx,task,ctype)
		AddedTask task accufun ctype
			= (idx,PTask task accufun,ctype)
	
	getPStCTask taskNr (idx,pstCTask) = case pstCTask of
		InitCTask lidx
			# (task,ctype) = fromCContainerToTask (sharedParallelState taskNr) (initCTasks !! lidx)
			= (idx,task,ctype)
		AddedCTask task ctype
			= (idx,PCTask task,ctype)
			
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
			# taskProps = case task of
				PTask {Task|properties} _	= properties
				PCTask {Task|properties}	= properties
			# info =	{ index				= idx
						, taskProperties	= taskProps
						, processProperties	= fmap (\{Process|properties} -> properties) mbProc
						}
			= (info,iworld)
				
		updateProcProps (idx,mprops) iworld
			# (_,iworld) = 'ProcessDB'.updateProcessProperties (taskNrToString [idx:taskNr]) (\pprops -> {ProcessProperties|pprops & managerProperties = mprops}) iworld
			= iworld
	
	fromCContainerToTask :: !(Shared (!acc,![ParallelTaskInfo]) [(!TaskIndex,!ManagerProperties)]) !(ControlTaskContainer acc) -> (!ParTask acc,!TaskContainerType)
	fromCContainerToTask s container
		# (ct,type)= case container of
			DetachedCTask p m	ct = (ct,CTDetached p m)
			WindowCTask w m		ct = (ct,CTWindow w m)
			DialogCTask w		ct = (ct,CTDialog w)
			InBodyCTask			ct = (ct,CTInBody)
			HiddenCTask			ct = (ct,CTHidden)
		= (PCTask (setControlTask (ct s)),type)
		
	fromContainerToTask	:: !(TaskContainer acc) -> (!ParTask acc,!TaskContainerType)
	fromContainerToTask container = case container of
		DetachedTask p m t f	= (PTask t (Right f),CTDetached p m)
		WindowTask w m t f		= (PTask t (Left f),CTWindow w m)
		DialogTask w t f		= (PTask t (Left f),CTDialog w)
		InBodyTask t f			= (PTask t (Left f),CTInBody)
		HiddenTask t f			= (PTask t (Left f),CTHidden)
		
	evaluateLocal :: !(ParTask acc) !TaskIndex !TaskContainerType !(PSt acc) !*TSt -> (!TaskResult [Control acc],!Bool,!PSt acc,!*TSt) | iTask acc
	evaluateLocal pTask idx ctype pst tst=:{taskNr} = case pTask of
		PTask task accuFun = case applyTaskCommit task (Just (idx,ctype)) tst of
			(TaskFinished r,tst) // finished ordinary task: remove from pst & calculate new acc
				# pst			= removeTasksPSt [idx] pst
				# (state,controls) = case accuFun of
					Left fun	= fun r pst.PSt.state
					Right _		= abort "detached accu fun for ordinary task"
				= (TaskFinished controls,False,{PSt | pst & state = state},tst)
			(TaskBusy,tst)
				= (TaskBusy,False,pst,tst)
			(TaskException e str,tst)
				= (TaskException e str,False,pst,tst)
		PCTask task = case applyTaskCommit task (Just (idx,ctype)) tst of
			(TaskFinished controls,tst) // finished control task: restart & process control signal
				# tst = deleteTaskStates taskNr tst
				# tst = removeFromTree [idx] tst
				= (TaskFinished controls,True,pst,tst)
			(res,tst)
				= (res,False,pst,tst)

	evaluateDetached :: !(ParTask acc) !TaskIndex !TaskContainerType !(PSt acc) !*TSt -> (!TaskResult [Control acc],!Bool,!PSt acc,!*TSt) | iTask acc
	evaluateDetached task idx ctype pst tst=:{TSt|taskNr,events}
		//Try to load the stored process for this subtask
		# (mbProc,tst) = 'ProcessDB'.getProcess (taskNrToString taskNr) tst
		= case mbProc of
			//Nothing found, process cancelled
			Nothing
				# pst = removeTasksPSt [idx] pst
				# (acc,controls) = case task of
					PTask _ (Right accufun)	= accufun Nothing pst.PSt.state
					PCTask _				= (pst.PSt.state,[])
				= (TaskFinished controls,False,{PSt | pst & state = acc},tst)
			//When found, evaluate
			Just proc
				# (result,_,tst) = evaluateTaskInstance proc events Nothing False False tst
				= case result of
					TaskBusy				= (TaskBusy,False,pst,tst)
					TaskException e	str		= (TaskException e str,False,pst,tst)
					_ // finished task
						= case task of
							PTask _ accufun = f
							where
								f => case (dynamic accufun,result) of
									(Right fun :: Either (AccuFun a acc^) (AccuFunDetached a acc^),TaskFinished (r :: a))
										# pst = removeTasksPSt [idx] pst
										# (acc,controls) = fun (Just r) pst.PSt.state
										= (TaskFinished controls,False,{PSt | pst & state = acc},tst)
									_
									 	= (taskException invalidType,False,pst,tst)
							PCTask _ = case result of
								TaskFinished (controls :: [Control acc^])
									# tst = deleteTaskStates taskNr tst
									# tst = deleteTaskInstance (taskNrToString taskNr) tst
									# tst = createProcs (tl taskNr) [(idx,task,ctype)] tst
									= (TaskFinished controls,False,pst,tst)
								_
									= (taskException invalidType,False,pst,tst)
	where
		invalidType = "evaluateDetached: task result of invalid type!"
		
	// removes given parallel task tree containers from tree	
	removeFromTree remIdxs tst=:{tree} = case tree of
		TTParallelTask ti children = {tst & tree = (TTParallelTask ti (filter (\(TTParallelContainer idx _ _ _) -> not (isMember idx remIdxs)) children))}
		_ = abort "parallel node expected"
		
	// removes tasks with given indexes from PSt
	removeTasksPSt idxs pst = {pst & tasks = filter (\(idx,_) -> not (isMember idx idxs)) pst.tasks, cTasks = filter (\(idx,_) -> not (isMember idx idxs)) pst.cTasks}

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
