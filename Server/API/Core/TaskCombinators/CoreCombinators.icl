implementation module CoreCombinators

import StdList, StdArray, StdTuple, StdMisc, StdBool, StdOrdList
import TSt, Util, HTTP, GenUpdate, UserDB, Store, Types, Text, TuningCombinators, Shared, OutputTasks, InteractionTasks, CommonCombinators
from StdFunc			import id, const, o, seq
from CommonCombinators	import transform
from ProcessDB			import :: Process{..}
from ProcessDB			import qualified class ProcessDB(..), instance ProcessDB TSt, instance ProcessDB IWorld
from iTasks				import JSONEncode, JSONDecode, dynamicJSONEncode, dynamicJSONDecode
from SharedTasks		import sharedStore, :: SharedStoreId

derive class iTask ParallelTaskInfo, SchedulerState, Control
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
	= (InBodyTask (\_ _ -> return Void), ust)
gUpdate{|TaskContainer|} _ (UDSearch t) ust = basicSearch t (\Void t -> t) ust
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
	
// Parallel composition
JSONEncode{|PSt|} c		=  dynamicJSONEncode c
JSONDecode{|PSt|} [j:c]	= (dynamicJSONDecode j,c)

:: PSt =
	{ tasks 	:: ![(!TaskIndex,!TaskContainerType,!PStTask)]		//Set of tasks in the parallel set
	, stop		:: !Bool											//Flag to indicate that the parallel set is complete
	, nextIdx	:: !TaskIndex
	}
	
:: PStTask	
	= E.a:	ActiveTask !(Task a) & iTask a				//A task that is an active part of the set
	|		FinishedTask !TaskProperties				//A completed task
	| E.a:	AddedTask !(Task a) & iTask a				//A task that has been added by a control
	| 		RemovedTask 								//A task that is flagged to be removed by a control

parallel :: !d !s !(ResultFun s a) ![TaskContainer s] -> Task a | iTask s & iTask a & descr d
parallel d initState resultFun initTasks
	= mkParallelTask d (parallelE,parallelC)
where
	parallelE tst=:{taskNr,newTask}
		//Create the internal state when first evaluated, or load upon later access
		# (pst,tst)		= if newTask 	(accIWorldTSt (initPSt initState initTasks taskNr) tst)
										(accIWorldTSt (loadPSt taskNr) tst)
		# tst 		= seqSt processTaskE pst.tasks tst
		= {tst & taskNr = taskNr}
	where
		processTaskE (idx,ctype,ActiveTask t) tst = case ctype of
			CTDetached _ _	= tst													//Skip detached tasks
			_				= snd (applyTaskEdit t {tst & taskNr = [idx:taskNr]})	//Evaluate editors for all other tasks
		processTaskE _ tst 	= tst
				
	parallelC tst=:{taskNr,properties,newTask}
		// Load the internal state
		# (pst,tst)		= if newTask 	(accIWorldTSt (initPSt initState initTasks taskNr) tst)
										(accIWorldTSt (loadPSt taskNr) tst)
		// Evaluate the subtasks for all currently active tasks
		# (res,pst,tst)	= processAllTasksC taskNr 0 pst tst
		// Create processes for added tasks and remove tasks marked for removal from the list and task tree
		# (pst,tst)		= removeMarkedTasks taskNr pst tst
		# tst 			= appIWorldTSt (storePSt taskNr pst) tst
		// The result of the combined evaluation of all parallel subtasks
		# (res,tst) = case res of
			//There are still active tasks
			TaskBusy
				// If all tasks finished return the transformed initial state
				| noMoreActive pst.tasks
					//Load the current content of the shared parallel state
					# tst = 'ProcessDB'.deleteSubProcesses (taskNrToString taskNr) tst
					# (state,tst)	= accIWorldTSt (readShared (stateShare initState taskNr)) tst
					= case state of
						Ok state	= (TaskFinished (resultFun AllRunToCompletion state),tst)
						Error e		= (TaskException (dynamic e) e,tst)			
				| otherwise
					= (TaskBusy,tst)
			//One of the tasks raised an execption
			TaskException e	str
				# tst = 'ProcessDB'.deleteSubProcesses (taskNrToString taskNr) tst
				= (TaskException e str, tst)
			//The parallel set was stopped prematurely
			TaskFinished r
				# tst = 'ProcessDB'.deleteSubProcesses (taskNrToString taskNr) tst
				= (TaskFinished (resultFun Stopped r),tst)
		= (res,{tst & taskNr = taskNr})
	where
		processAllTasksC taskNr i pst tst
			//Check if all tasks have been processed
			| i > (length pst.tasks - 1)
				= (TaskBusy,pst,tst)	
			//Select the task
			# (idx,ctype,task)	= (pst.tasks !! i)
			//Check if the indicated task is still not removed or finished
			| isFinished task || isRemoved task
				= processAllTasksC taskNr (i + 1) pst tst		
			//Evaluate the task (with shifted task number)
			# (res,tst)	= case ctype of
				CTDetached _ _	= evaluateDetached taskNr (idx,ctype,task) i pst {tst & taskNr = [idx:taskNr]}
				_				= evaluateLocal taskNr (idx,ctype,task) i pst {tst & taskNr = [idx:taskNr]}
			//Reload the pst
			# (pst,tst)			= accIWorldTSt (loadPSt taskNr) tst
			//Check if the stop control has been set
			| pst.stop
				//Load the current content of the shared parallel state
				# (state,tst)	= accIWorldTSt (readShared (stateShare initState taskNr)) tst
				= case state of
					Ok state	= (TaskFinished state,pst,tst)
					Error e		= (TaskException (dynamic e) e,pst,tst)			
			//Determine how to continue
			= case res of
				TaskBusy
					//Process the other tasks
					= processAllTasksC taskNr (i + 1) pst tst
				TaskFinished _
					//Mark the task as finished
					# pst = {PSt|pst & tasks = [if (i == idx) (i,c,markFinished t) (i,c,t) \\ (i,c,t) <- pst.tasks]}
					# tst = appIWorldTSt (storePSt taskNr pst) tst
					//Process the other tasks
					= processAllTasksC taskNr (i + 1) pst tst
				TaskException e str
					//Don't process the other tasks, just let the exception through
					= (TaskException e str,pst,tst)
		
		isActive (ActiveTask _)		= True
		isActive _					= False
		
		isFinished (FinishedTask _) = True
		isFinished _				= False

		isRemoved (RemovedTask)		= True
		isRemoved _					= False

		noMoreActive tasks = isEmpty [t \\ (_,_,t) <- tasks | isActive t]

		markFinished (ActiveTask {Task|properties})	= FinishedTask properties
		markFinished t								= t

	//Create the initial parallel task set
	initPSt	:: !s ![TaskContainer s] !TaskNr !*IWorld -> (!PSt,!*IWorld) | iTask s
	initPSt initState initTasks taskNr iworld
		# pst		= {PSt| tasks = [unpack (stateShare initState taskNr) (controlShare initState taskNr) idx t \\ t <- initTasks & idx <- [0..]]
						  , stop = False
						  , nextIdx = length initTasks}
		# iworld	= setTaskStoreFor taskNr "pst" pst iworld
		# iworld	= updateTimestamp taskNr iworld
		= (pst,iworld)
	where
		unpack s c i (DetachedTask p m t)	= (i, (CTDetached p m), AddedTask  (t s c))
		unpack s c i (WindowTask w m t)		= (i, (CTWindow w m), AddedTask (t s c))
		unpack s c i (DialogTask w t)		= (i, (CTDialog w), AddedTask (t s c))
		unpack s c i (InBodyTask t)			= (i, CTInBody, AddedTask (t s c))
		unpack s c i (HiddenTask t)			= (i, CTHidden, AddedTask (t s c))
		
	//Load or the internal state
	loadPSt :: !TaskNr !*IWorld -> (!PSt,!*IWorld)
	loadPSt taskNr iworld
		# (mbPSt,iworld) = getTaskStoreFor taskNr "pst" iworld
		= case mbPSt of
			Just pst	= (pst,iworld)
			Nothing		= abort "Could not load parallel task info"

	//Store the internal state
	storePSt :: !TaskNr !PSt !*IWorld -> *IWorld
	storePSt taskNr pst iworld
		= setTaskStoreFor taskNr "pst" pst ((updateTimestamp taskNr) iworld)
				
	evaluateLocal :: !TaskNr (!TaskIndex,!TaskContainerType,!PStTask) !Int !PSt !*TSt -> (!TaskResult Void,!*TSt)
	evaluateLocal taskNr (idx,ctype,AddedTask task) i pst tst		
		//Just mark the task as active, and store the PSt
		# t		= (idx,ctype,ActiveTask task)
		# pst	= {PSt|pst & tasks = updateAt i t pst.tasks}
		# tst 	= appIWorldTSt (storePSt taskNr pst) tst
		= evaluateLocal taskNr t i pst tst
	evaluateLocal taskNr (idx,ctype,ActiveTask task) i pst tst
		= case applyTaskCommit task (Just (idx,ctype)) tst of 
			(TaskBusy,tst)
				= (TaskBusy,tst)
			(TaskFinished r,tst)
				= (TaskFinished Void,tst)
			(TaskException e str,tst)
				= (TaskException e str,tst)
		
	evaluateDetached :: !TaskNr (!TaskIndex,!TaskContainerType,!PStTask) !Int !PSt !*TSt -> (!TaskResult Void,!*TSt)
	evaluateDetached taskNr (idx,CTDetached mProperties menu,task) i pst tst
		= case task of
			//Create and evaluate a process for this task
			(AddedTask task)
				# pst				= {PSt|pst & tasks = updateAt i (idx,CTDetached mProperties menu,ActiveTask task) pst.tasks}
				# tst 				= appIWorldTSt (storePSt taskNr pst) tst
				# thread			= createThread task
				# (_,result,_,tst)	= createTaskInstance thread False False mProperties menu tst
				= case result of
					TaskBusy				= (TaskBusy,tst)
					TaskException e	str		= (TaskException e str,tst)
					_						= (TaskFinished Void,tst)
			(ActiveTask task)
				//Try to load the stored process for this subtask
				# (mbProc,tst) = 'ProcessDB'.getProcess (taskNrToString [idx:taskNr]) tst
				= case mbProc of
					//Nothing found, process cancelled
					Nothing		
						= (TaskFinished Void,tst)
					//When found, evaluate
					Just proc
						# (result,_,tst) = evaluateTaskInstance proc Nothing False False tst
						= case result of
							TaskBusy				= (TaskBusy,tst)
							TaskException e	str		= (TaskException e str,tst)
							_						= (TaskFinished Void,tst)
	
	//Definition of the shared parallel state
	stateShare :: !s !TaskNr -> SymmetricShared s | iTask s
	stateShare	initState taskNr = sharedStore (iTaskId taskNr "psv") initState
						
	//Definition of the shared task status information
	controlShare :: !s !TaskNr -> Shared [ParallelTaskInfo] [Control s] | iTask s
	controlShare initState taskNr = Shared read write timestamp
	where
		read iworld
			//Load task set
			# (pst,iworld)			= loadPSt taskNr iworld	
			//Find the task information for the tasks in the set
			# (taskInfos,iworld)	= buildTaskInfo pst.tasks iworld
			= (Ok taskInfos,iworld)
		where
			buildTaskInfo [] iworld = ([],iworld)
			buildTaskInfo [(idx, ctype, ActiveTask {Task|properties}):ts] iworld
				# (info,iworld)		= buildTaskInfo ts iworld
				= case ctype of
					CTDetached _ _
						# (mbProc,iworld)	= 'ProcessDB'.getProcess (taskNrToString [idx:taskNr]) iworld
						= case mbProc of
							Just {Process|properties}	= ([{ParallelTaskInfo|index = idx, properties = Right properties}:info],iworld)
							Nothing						= (info,iworld)
					_
						= ([{ParallelTaskInfo|index = idx, properties = Left properties}:info],iworld)
			buildTaskInfo [(idx, ctype, FinishedTask properties):ts] iworld
				# (info,iworld)		= buildTaskInfo ts iworld
				= ([{ParallelTaskInfo|index = idx, properties = Left properties}:info],iworld)
			buildTaskInfo [(idx, ctype, AddedTask {Task|properties}):ts] iworld
				# (info,iworld) = buildTaskInfo ts iworld
				= case ctype of
					CTDetached managerProperties menu
						//Construct a 'fake' ProcessProperties record for the task which has not been evaluated yet
						//Because the task is detached, views on the parallel info will expect full process properties
						//even though no process has been created yet.
						# systemProperties =
							{SystemProperties
							|taskId = taskNrToString [idx:taskNr]
							,status = Running
							,parent = Nothing		//TODO: Somehow determine the right value for this (by putting it in the AddedTask constructor)
							,issuedAt = Timestamp 0	//TODO: Somehow determine the right value for this
							,firstEvent = Nothing
							,latestEvent = Nothing
							,deleteWhenDone = False
							,menu = menu
							}
						# properties =
							{ProcessProperties
							|taskProperties = properties
							,managerProperties = managerProperties
							,systemProperties = systemProperties
							,progress = TPActive
							}
						= ([{ParallelTaskInfo|index = idx, properties = Right properties}:info],iworld)
					_
						= ([{ParallelTaskInfo|index = idx, properties = Left properties}:info],iworld)
			buildTaskInfo [(idx, ctype, RemovedTask):ts] iworld
				= buildTaskInfo ts iworld
			
		write controls iworld
			# (pst,iworld)		= loadPSt taskNr iworld
			# (pst,iworld)		= processControls controls pst iworld
			# iworld			= storePSt taskNr pst iworld
			= (Ok Void,iworld)
		where
			processControls [] pst iworld = (pst,iworld)
			processControls [c:cs] pst iworld = case c of
				//Set the stop flag in the state
				StopParallel
					= processControls cs {PSt|pst& stop = True} iworld
				AppendTask container
					# newtask			= unpack (stateShare initState taskNr) (controlShare initState taskNr) pst.nextIdx container
					= processControls cs {PSt|pst & tasks = pst.tasks ++ [newtask], nextIdx = pst.nextIdx + 1} iworld
				RemoveTask idx
					= processControls cs {PSt|pst & tasks = [if (i == idx) (i,c,RemovedTask) (i,c,t) \\ (i,c,t) <- pst.tasks]} iworld
				UpdateProperties idx mprops
					//'dumb' update assuming that the task at index is a detached task. This can be improved.
					# (_,iworld) = 'ProcessDB'.updateProcessProperties (taskNrToString [idx:taskNr]) (\pprops -> {ProcessProperties|pprops & managerProperties = mprops}) iworld
					= processControls cs pst iworld
				_	
					= processControls cs pst iworld

			unpack s c i (DetachedTask p m t)	= (i, CTDetached p m, AddedTask (t s c))
			unpack s c i (WindowTask w m t)		= (i, CTWindow w m, AddedTask (t s c))
			unpack s c i (DialogTask w t)		= (i, CTDialog w, AddedTask (t s c))
			unpack s c i (InBodyTask t)			= (i, CTInBody, AddedTask (t s c))
			unpack s c i (HiddenTask t)			= (i, CTHidden, AddedTask (t s c))

		timestamp iworld
			# (mbLastUpdate,iworld) = getTaskStoreFor taskNr "lastUpdate" iworld
			= case mbLastUpdate of
				Just lastUpdate	= (Ok lastUpdate,iworld)
				Nothing			= (Error "no timestamp",iworld)

	updateTimestamp :: !TaskNr !*IWorld -> *IWorld
	updateTimestamp taskNr iworld=:{IWorld|timestamp}
		= setTaskStoreFor taskNr "lastUpdate" timestamp iworld

	removeMarkedTasks :: !TaskNr !PSt !*TSt -> (!PSt,!*TSt)
	removeMarkedTasks taskNr pst tst
		# (tasks,idxs,tst)	= remove pst.tasks tst
		# tst				= removeFromTree idxs tst
		# tst				= deleteStates idxs tst
		= ({PSt|pst & tasks = tasks},tst)
	where
		remove [] tst = ([],[],tst)
		remove [(idx,ctype,RemovedTask):ts] tst
			# (ts,idxs,tst)	= remove ts tst
			= (ts,[idx:idxs],tst)
		remove [t:ts] tst
			# (ts,idxs,tst)	= remove ts tst
			= ([t:ts],idxs,tst)
				
		// removes given parallel task tree containers from tree	
		removeFromTree remIdxs tst=:{tree} = case tree of
			TTParallelTask ti children
				= {tst & tree = (TTParallelTask ti (filter (\(TTParallelContainer idx _ _) -> not (isMember idx remIdxs)) children))}
			_	= abort "parallel node expected"	
			
		// deletes states of tasks (and possibly processes) with given indexes
		deleteStates idxs tst
			# taskNrs	= map (\idx -> [idx:taskNr]) idxs
			# tst		= seqSt (\taskNr tst -> deleteTaskStates taskNr tst)									taskNrs tst
			# tst		= seqSt (\taskNr tst -> snd ('ProcessDB'.deleteProcess (taskNrToString taskNr) tst))	taskNrs tst
			= tst

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
