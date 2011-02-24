implementation module CoreCombinators

import StdList, StdArray, StdTuple, StdMisc, StdBool
import TSt, Util, HTTP, GenUpdate, UserDB, ProcessDB, Store, Types, Text, TuningCombinators, Shared, MonitorTasks, InteractiveTasks
from ProcessDBTasks		import sharedProcessStatus, sharedProcessResult, class toProcessId, instance toProcessId ProcessRef
from StdFunc			import id, const, o
from TaskTree			import :: TaskParallelType
from CommonCombinators	import transform

derive class iTask SchedulerState, TaskStatus

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
derive class iTask PSt
derive bimap Maybe, (,)

:: PSt a b =
	{ state :: b
	, tasks :: [(Task a,Bool)]
	}

group :: !d !((taskResult,Int) gState -> (gState,PAction (Task taskResult))) (gState -> gResult) !gState ![Task taskResult] ![GroupAction gState] (GroupActionGenFunc taskResult) -> Task gResult | iTask taskResult & iTask gState & iTask gResult & descr d
group d procFun parseFun initState initTasks groupActions groupAGenFunc = mkGroupedTask d (execInGroupE,execInGroupC)
where
	execInGroupE tst=:{taskNr}
		# (pst,tst) = loadPSt taskNr tst
		= processAllTasksE pst 0 tst
		
	processAllTasksE pst idx tst=:{taskNr}
		| (length pst.tasks) == idx = tst
		# (task,_)					= pst.tasks !! idx
		# (_,tst)					= applyTaskEdit task {tst & taskNr = [idx:taskNr]} 
		= processAllTasksE pst (inc idx) {tst & taskNr = taskNr}
		
	execInGroupC tst=:{taskNr,request}
		# (pst,tst)   		= loadPSt taskNr tst
		# (mbAction,tst)	= getEventGroupActionEvent groupActions tst
		# (gActionStop,mbFocus,pst) 
			= case mbAction of
				Nothing
					= (False,Nothing,pst)
				Just (action,data)
					# (nSt,act) = procFun (groupAGenFunc (action, data), -1) pst.PSt.state
					# pst = {PSt | pst & state = nSt}
					= case act of
						Stop			= (True,Nothing,pst)
						Continue		= (False,Nothing,pst)
						Extend tlist	= (False,Nothing,{PSt | pst & tasks = pst.tasks ++ [(task,False) \\ task <- tlist]})
						Focus tag		= (False,Just tag,pst)
				
		# (result,pst,tst,mbFocus) 	= processAllTasksC pst 0 tst mbFocus
		# tst						= appIWorldTSt (setTaskStoreFor taskNr "pst" pst) tst
		= case result of
			TaskException e = (TaskException e,tst)
			TaskFinished  r = (TaskFinished (parseFun r),tst)
			TaskBusy
				| gActionStop	= (TaskFinished (parseFun pst.PSt.state),tst)
				| otherwise
					# tst = setGroupActions (evaluateConditions groupActions pst.PSt.state) tst
					# tst = case mbFocus of
						Just (Tag t)	= setFocusCommand (toString t) tst
						Nothing			= tst
					= (TaskBusy,tst)

	processAllTasksC pst idx tst=:{taskNr} mbFocus
		| (length pst.tasks) == idx = (TaskBusy,pst,tst,mbFocus)
		# (task,done)				= pst.tasks !! idx
		# (res,tst)					= applyTaskCommit task {tst & taskNr = [idx:taskNr]} 
		= case res of
			TaskException e = (TaskException e,pst,{tst & taskNr = taskNr},mbFocus)
			TaskBusy		= processAllTasksC pst (inc idx) {tst & taskNr = taskNr} mbFocus
			TaskFinished a	
				| done			= processAllTasksC pst (inc idx) {tst & taskNr = taskNr} mbFocus
				# (nSt,act)		= procFun (a,idx) pst.PSt.state
				# pst			= markProcessed {PSt | pst & state = nSt} idx
				= case act of
					Stop 		= (TaskFinished pst.PSt.state,pst,{tst & taskNr = taskNr},mbFocus)
					Continue	= processAllTasksC pst (inc idx) {tst & taskNr = taskNr} mbFocus
					Extend tlist
						# pst = {PSt | pst & tasks = pst.tasks ++ [(task,False) \\ task <- tlist]}
						= processAllTasksC pst (inc idx) {tst & taskNr = taskNr} mbFocus
					Focus tag	= processAllTasksC pst (inc idx) {tst & taskNr = taskNr} (Just tag)

	loadPSt taskNr tst
		# (mbPSt,tst) = accIWorldTSt (getTaskStoreFor taskNr "pst") tst
		= case mbPSt of
			(Just p) = (p,tst)
			Nothing  = initPSt taskNr tst
	
	initPSt taskNr tst
		# pst = { PSt
				| state = initState
				, tasks = [(task, False) \\ task <- initTasks]
				}
		# tst = appIWorldTSt (setTaskStoreFor taskNr "pst" pst) tst
		= (pst,tst)

	markProcessed pst idx
		# (t,b) 	= pst.tasks !! idx
		# tasks 	= updateAt idx (t,True) pst.tasks
		= {PSt | pst & tasks = tasks}
		
	evaluateConditions actions state = [(action, evaluateCondition condition) \\ (action, condition) <-  actions]
	where
		evaluateCondition Always					= Left	True
		evaluateCondition (StatePredicate p)		= Left	(p state)
		evaluateCondition (SharedPredicate id p)	= Right	(checkSharedPred id p)
		
		checkSharedPred shared p iworld
			# (val,iworld) = appFst fromOk (readShared shared iworld)
			= (p val, iworld)
				
	getEventGroupActionEvent groupActions tst
		# (mbActionEvent,tst)	= getActionEvent tst
		# mbActionEvent			= maybe Nothing getNameAndData mbActionEvent
		# res = case mbActionEvent of
			Nothing
				= Nothing
			Just (name,data)
				= case [(action, data) \\ (action,pred) <- groupActions | actionName action == name] of
					[actionEvent]	= Just actionEvent
					_				= Nothing
		= (res,tst)
	where
		getNameAndData (JSONString key) 							= Just (key, "")
		getNameAndData (JSONArray [JSONString key,JSONString data])	= Just (key, data)
		getNameAndData _											= Nothing
		
parallel :: !TaskParallelType !d !((a,Int) b -> (b,PAction (Task a))) (b -> c) !b ![Task a] -> Task c | iTask a & iTask b & iTask c & descr d
parallel parType d procFun parseFun initState initTasks
	= mkParallelTask d parType (parallelE,parallelC)
where
	parallelE tst=:{taskNr}
		| isEmpty initTasks = tst
		# (pst,tst) = loadPSt taskNr tst
		= processAllTasksE pst 0 tst
		
	processAllTasksE pst idx tst=:{taskNr}
		| (length pst.tasks) == idx = tst
		# (task,_)					= pst.tasks !! idx
		# (_,tst)					= applyTaskEdit task {tst & taskNr = [idx:taskNr]} 
		= processAllTasksE pst (inc idx) {tst & taskNr = taskNr}

	parallelC tst=:{taskNr,properties}
		// When the initial list of tasks is empty just return the transformed initial state
		| isEmpty initTasks
			= (TaskFinished (parseFun initState), tst)
		// Load the internal state
		# (pst,tst)			= loadPSt taskNr tst
		// Evaluate the subtasks for all currently active tasks
 		# (res,pst,tst)		= processAllTasksC 0 pst tst
		// Store the internal state
		# tst				= storePSt taskNr pst tst
		// The result of the combined evaluation of all parallel subtasks
		= case res of
			//There are still active tasks
			TaskBusy		= (TaskBusy,tst)
			//One the tasks raised an execption
			TaskException e	= (TaskException e, tst)
			//The procFun returned a stop action, or all tasks are completed
			TaskFinished r
				//Remove the extra workers for this parallel combination
				# tst = clearSubTaskWorkers (taskNrToString taskNr) (Just parType) tst
				= (TaskFinished (parseFun r),tst)
	
	//Load or create the internal state
	loadPSt taskNr tst=:{TSt|properties}
		# (mbPSt,tst) = accIWorldTSt (getTaskStoreFor taskNr "pst") tst
		= case mbPSt of
			Just pst	= (pst,tst)
			Nothing 	= ({PSt | state = initState, tasks = [(task,False) \\ task <- initTasks]},tst)
	
	storePSt taskNr pst tst
		= appIWorldTSt (setTaskStoreFor taskNr "pst" pst) tst
	
	processAllTasksC idx pst=:{PSt|state,tasks} tst=:{TSt|taskNr,properties}
		= case tasks of
			//We have processed all results
			[]					= (TaskBusy, {PSt|state = state, tasks = []}, tst)
			//Process another task
			[(task,done):ts]
				//IMPORTANT: Task is evaluated with a shifted task number!!!
				# (result,tree,tst)	= createOrEvaluateTaskInstance (Just parType) task {tst & taskNr = [idx:taskNr]}
				// Add the tree to the current node
				# tst				= addTaskNode tree tst
				= case result of
					TaskBusy
						//Process the other tasks
						# (result,pst,tst) = processAllTasksC (inc idx) {PSt|state = state, tasks = ts} {tst & taskNr = taskNr}
						= (result, {PSt| pst & tasks = [(task,False):pst.tasks]}, {tst & taskNr = taskNr})  
					TaskFinished a
						//When we have applied the process function already, don't do it a second time
						| done
							//Process the other tasks
							# (result,pst,tst) = processAllTasksC (inc idx) {PSt| state = state, tasks = ts} {tst & taskNr = taskNr}
							= (result, {PSt | pst & tasks = [(task,True):pst.tasks]}, {tst & taskNr = taskNr}) 
						//Apply the process function
						# (state,action)	= procFun (a,idx) state
						= case action of
							Stop
								//Don't process the other tasks, return the state as result
								= (TaskFinished state, {PSt|pst & state = state, tasks = [(task,True):ts]}, {tst & taskNr = taskNr})
							Continue
								//Process the other tasks
								# (result,pst,tst) = processAllTasksC (inc idx) {PSt| state = state, tasks = ts} {tst & taskNr = taskNr}
								= (result, {PSt | pst & tasks = [(task,True):pst.tasks]}, {tst & taskNr = taskNr})
								//Process the other tasks extended with the new tasks
							Extend tasks
								# (result,pst,tst) = processAllTasksC (inc idx) {PSt| state = state, tasks = ts ++ [(t,False) \\ t <- tasks]} {tst & taskNr = taskNr}
								= (result, {PSt | pst & tasks = [(task,True):pst.tasks]}, {tst & taskNr = taskNr})
					TaskException e
						//Don't process the other tasks, just let the exception through
						= (TaskException e, {PSt| pst & tasks = [(task,True):ts]}, {tst & taskNr = taskNr})
/*
* When a task is assigned to a user a synchronous task instance process is created.
* It is created once and loaded and evaluated on later runs.
*/
assign :: !User !(Task a) -> Task a | iTask a	
assign user task = mkMainTask (taskTitle task, taskDescription task) assign`
where
	assign` tst
		# (result,node,tst) = createOrEvaluateTaskInstance Nothing (task <<@ user) tst
		= (result,{TSt|tst & tree = node})
		
createOrEvaluateTaskInstance :: !(Maybe TaskParallelType) !(Task a) !*TSt -> (!TaskResult a, !TaskTree, !*TSt) | iTask a
createOrEvaluateTaskInstance mbpartype task tst=:{TSt|taskNr,events,treeType}
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
			# (result,tree,tst)	= evaluateTaskInstance proc treeType events Nothing False False tst
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
		monitorTask ("Wait for task", "Wait for an external task to finish") waitForProcessView waitForProcessPred autoContinue (sharedProcessStatus pref >+< sharedProcessResult pref)
	>>=	transform snd
	
waitForProcessCancel :: !Bool !(ProcessRef a) -> Task (Maybe a) | iTask a
waitForProcessCancel autoContinue pref =
		monitorTaskA ("Wait for task", "Wait for an external task to finish or cancel") waitForProcessView actions autoEvents (sharedProcessStatus pref >+< sharedProcessResult pref)
	>>=	transform (maybe Nothing snd o snd)
where
	actions = [(ActionCancel,always)] ++ if autoContinue [] [(ActionContinue,pred`)]
	
	autoEvents v
		| autoContinue && pred` v	= Just (ActionContinue,"")
		| otherwise					= Nothing
		
	pred` Invalid	= False
	pred` (Valid v)	= waitForProcessPred v
	
waitForProcessView (Active,_)		= "Task is running."
waitForProcessView (Suspended,_)	= "Task is suspended."
waitForProcessView (_,res)			= "Task finished. Result: " <+++ res
//waiting = html [Text "Waiting for result of task ",StrongTag [] [Text "\"",Text properties.managerProperties.ManagerProperties.taskDescription.TaskDescription.title,Text "\""]]

waitForProcessPred (Active,_)		= False
waitForProcessPred (Suspended,_)	= False
waitForProcessPred _				= True

scheduledSpawn	:: (DateTime -> DateTime) (Task a) -> Task (Shared (SchedulerState,[ProcessRef a]) Void) | iTask a
scheduledSpawn when task = abort "not implemented"