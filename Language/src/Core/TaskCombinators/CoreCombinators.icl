implementation module CoreCombinators

import	StdList, StdArray, StdTuple, StdMisc, StdBool
from	StdFunc import id, const
from	TaskTree import :: TaskParallelType

import	TSt
import	Util, Http
import	GenUpdate
import	UserDB, ProcessDB
import  Store
import	TuningCombinators
import  Types
import	Text

//Standard monadic operations:
(>>=) infixl 1 :: !(Task a) !(a -> Task b) -> Task b | iTask a & iTask b
(>>=) taska taskb = mkSequenceTask ">>=" tbind
where
	tbind tst
		# (result,tst)		= applyTask taska tst
		= case result of
			TaskBusy
				= (TaskBusy, tst)
			TaskFinished a
				//Pass the argument and do the second part
				= applyTask (taskb a) tst
			TaskException e
				= (TaskException e,tst)
				
(>>|) infixl 1 :: !(Task a) (Task b) -> Task b | iTask a & iTask b
(>>|) taska taskb = taska >>= \_ -> taskb

return :: !a -> (Task a) | iTask a
return a  = mkInstantTask "return" (\tst -> (TaskFinished a,tst))

//Repetition and loops:

forever :: !(Task a) -> Task a | iTask a
forever task = mkSequenceTask "forever" forever`
where
	forever` tst=:{taskNr} 
		# (result,tst)= applyTask task tst					
		= case result of
			TaskFinished _			
				# tst = deleteTaskStates (tl taskNr) tst
				// Delete subprocesses in the process table
				# tst = deleteSubProcesses (taskNrToString (tl taskNr)) tst 
				# tst = resetSequence tst
				= forever` tst				
			_
				= (result,tst)

(<!) infixl 6 :: !(Task a) !(a -> .Bool) -> Task a | iTask a
(<!) task pred = mkSequenceTask "<!" doTask
where
	doTask tst=:{taskNr}
		# (result,tst) 	= applyTask task tst
		= case result of
			TaskBusy
				= (TaskBusy,tst)
			TaskFinished a
				| not (pred a)			
					# tst = deleteTaskStates (tl taskNr) tst
					# tst = deleteSubProcesses (taskNrToString (tl taskNr)) tst
					# tst = resetSequence tst
					= doTask tst
				| otherwise
					= (TaskFinished a,tst)
			TaskException e
				= (TaskException e,tst)
		
iterateUntil :: !(Task a) !(a -> Task a) !(a -> .Bool) -> Task a | iTask a
iterateUntil init cont pred = mkSequenceTask "Iterate Until" doTask
where
	doTask tst=:{taskNr}
		# key 			= "iu-temp"
		# (mbVal,tst)	= getTaskStore key tst
		# (result,tst)  = case mbVal of
							Nothing = applyTask init 	 tst
							Just v  = applyTask (cont v) tst
		= case result of
			TaskFinished a
				| pred a
					= (TaskFinished a,tst)
				| otherwise	
					# tst = deleteTaskStates (tl taskNr) tst
					# tst = resetSequence tst
					# tst = setTaskStore key a tst
					= doTask tst
			_
					= (result, tst)
// Sequential composition
sequence :: !String ![Task a] -> (Task [a])	| iTask a
sequence label tasks = mkSequenceTask label (\tst -> doseqTasks tasks [] tst)
where
	doseqTasks [] accu tst				= (TaskFinished (reverse accu), tst)
	doseqTasks [task:ts] accu tst 	
		# (result,tst)					= applyTask task tst
		= case result of
			TaskBusy					= (TaskBusy,tst)
			TaskFinished a				= doseqTasks ts [a:accu] tst
			TaskException e				= (TaskException e,tst)
			
// Parallel / Grouped composition
derive gPrint 		PSt
derive gParse 		PSt
derive gVisualize 	PSt
derive gUpdate 		PSt
derive gError		PSt
derive gHint		PSt

derive bimap Maybe, (,)

:: PSt a b =
	{ state :: b
	, tasks :: [(Task a,Bool)]
	}

group :: !String !String !((a,Int) b -> (b,PAction (Task a) tag)) (b -> c) !b ![Task a] ![GroupAction a b s] -> Task c | iTask a & iTask b & iTask c & iTask s
group label description procFun parseFun initState initTasks groupActions = mkGroupedTask label execInGroup
where
	execInGroup tst=:{taskNr,request}
		# grTaskNr			= drop 1 taskNr // get taskNr of group-task
		# (events,tst)		= getEventsFor (taskNrToString grTaskNr) True tst
		# (pst,tst)   		= loadPSt grTaskNr tst
		# gAction			= case parseString (http_getValue "group" events "") of
								Nothing = parseString (http_getValue "menuAndGroup" events "")
								res = res
		# (gActionStop,mbFocus,pst) 
							= case gAction of
								Just action	= case filter (\act -> (getAction act) == action) groupActions of
									[gAction:_]
										# (nSt,act) = procFun (getResult action gAction,-1) pst.state
										# pst = {pst & state = nSt}
										= case act of
											Stop			= (True,Nothing,pst)
											Continue		= (False,Nothing,pst)
											Extend tlist	= (False,Nothing,{PSt | pst & tasks = pst.tasks ++ [(task,False) \\ task <- tlist]})
											Focus tag		= (False,Just tag,pst)
									_ = (False,Nothing,pst)
								Nothing = (False,Nothing,pst)
		# (result,pst,tst,mbFocus) 	= processAllTasks pst 0 tst mbFocus
		# tst						= setTaskStoreFor grTaskNr "pst" pst tst
		= case result of
			TaskException e = (TaskException e,tst)
			TaskFinished  r = (TaskFinished (parseFun r),tst)
			TaskBusy
				| gActionStop	= (TaskFinished (parseFun pst.state),tst)
				| otherwise
					# tst = setGroupActions (evaluateConditions groupActions pst.state) tst
					# tst = case mbFocus of
						Just (Tag t)	= setFocusCommand (toString t) tst
						Nothing			= tst
					= (TaskBusy,tst)

	processAllTasks pst idx tst mbFocus
		| (length pst.tasks) == idx = (TaskBusy,pst,tst,mbFocus)
		# (task,done)				= pst.tasks !! idx
		# (res,tst)					= applyTask task tst
		= case res of
			TaskException e = (TaskException e,pst,tst,mbFocus)
			TaskBusy		= processAllTasks pst (inc idx) tst mbFocus
			TaskFinished a	
				| done			= processAllTasks pst (inc idx) tst mbFocus
				# (nSt,act)		= procFun (a,idx) pst.state
				# pst			= markProcessed {PSt | pst & state = nSt} idx
				= case act of
					Stop 		= (TaskFinished pst.state,pst,tst,mbFocus)
					Continue	= processAllTasks pst (inc idx) tst mbFocus
					Extend tlist
						# pst = {PSt | pst & tasks = pst.tasks ++ [(task,False) \\ task <- tlist]}
						= processAllTasks pst (inc idx) tst mbFocus
					Focus tag	= processAllTasks pst (inc idx) tst (Just tag)

	loadPSt taskNr tst
		# (mbPSt,tst) = getTaskStoreFor taskNr "pst" tst
		= case mbPSt of
			(Just p) = (p,tst)
			Nothing  = initPSt taskNr tst
	
	initPSt taskNr tst
		# pst = { PSt
				| state = initState
				, tasks = [(task, False) \\ task <- initTasks]
				}
		# tst = setTaskStoreFor taskNr "pst" pst tst
		= (pst,tst)

	markProcessed pst idx
		# (t,b) 	= pst.tasks !! idx
		# tasks 	= updateAt idx (t,True) pst.tasks
		= {PSt | pst & tasks = tasks}
		
	evaluateConditions actions state = [(getAction a,evaluateCondition (getCond a)) \\ a <-  actions]
	where
		evaluateCondition GroupAlways				= Left	True
		evaluateCondition (StatePredicate p)		= Left	(p state)
		evaluateCondition (SharedPredicate id p)	= Right	(checkSharedPred id p)
		
		checkSharedPred id p tst=:{TSt|iworld=iworld=:{IWorld|store,world}}
			# (mbVal,store,world)	= loadValue id store world
			# tst					= {TSt|tst & iworld = {IWorld|iworld & store = store, world = world}}
			= case mbVal of
				Just val	= (p (SharedValue val), tst)
				Nothing		= (p SharedDeleted, tst)
				
	getAction	(GroupAction a _ _)			= a
	getAction	(GroupActionParam name _ _)	= ActionParam name "?"
	
	getCond		(GroupAction _ _ cond)		= cond
	getCond		(GroupActionParam _ _ cond)	= cond
	
	getResult	(ActionParam _ param)	(GroupActionParam _ f _)	= f param
	getResult	_						(GroupAction _ res _)		= res

parallel :: !TaskParallelType !String !String !((a,Int) b -> (b,PAction (Task a) tag)) (b -> c) !b ![Task a] -> Task c | iTask a & iTask b & iTask c
parallel parType label description procFun parseFun initState initTasks
	= mkParallelTask label {TaskParallelInfo|type = parType, description = description} parallel`
where
	parallel` tst=:{taskNr,properties}
		// When the initial list of tasks is empty just return the transformed initial state
		| isEmpty initTasks
			= (TaskFinished (parseFun initState), tst)
		// Load the internal state
		# (pst,tst)			= loadPSt taskNr tst
		// Evaluate the subtasks for all currently active tasks
 		# (res,pst,tst)		= processAllTasks 0 pst tst
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
		# (mbPSt,tst) = getTaskStoreFor taskNr "pst" tst
		= case mbPSt of
			Just pst	= (pst,tst)
			Nothing 	= ({PSt | state = initState, tasks = [(task,False) \\ task <- initTasks]},tst)
	
	storePSt taskNr pst tst
		= setTaskStoreFor taskNr "pst" pst tst
	
	processAllTasks idx pst=:{PSt|state,tasks} tst=:{TSt|taskNr,properties}
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
						# (result,pst,tst) = processAllTasks (inc idx) {PSt|state = state, tasks = ts} {tst & taskNr = taskNr}
						= (result, {PSt| pst & tasks = [(task,False):pst.tasks]}, {tst & taskNr = taskNr})  
					TaskFinished a
						//When we have applied the process function already, don't do it a second time
						| done
							//Process the other tasks
							# (result,pst,tst) = processAllTasks (inc idx) {PSt| state = state, tasks = ts} {tst & taskNr = taskNr}
							= (result, {PSt | pst & tasks = [(task,True):pst.tasks]}, {tst & taskNr = taskNr}) 
						//Apply the process function
						# (state,action)	= procFun (a,idx) state
						= case action of
							Stop
								//Don't process the other tasks, return the state as result
								= (TaskFinished state, {PSt|pst & state = state, tasks = [(task,True):ts]}, {tst & taskNr = taskNr})
							Continue
								//Process the other tasks
								# (result,pst,tst) = processAllTasks (inc idx) {PSt| state = state, tasks = ts} {tst & taskNr = taskNr}
								= (result, {PSt | pst & tasks = [(task,True):pst.tasks]}, {tst & taskNr = taskNr})
								//Process the other tasks extended with the new tasks
							Extend tasks
								# (result,pst,tst) = processAllTasks (inc idx) {PSt| state = state, tasks = ts ++ [(t,False) \\ t <- tasks]} {tst & taskNr = taskNr}
								= (result, {PSt | pst & tasks = [(task,True):pst.tasks]}, {tst & taskNr = taskNr})
					TaskException e
						//Don't process the other tasks, just let the exception through
						= (TaskException e, {PSt| pst & tasks = [(task,True):ts]}, {tst & taskNr = taskNr})
/*
* When a task is assigned to a user a synchronous task instance process is created.
* It is created once and loaded and evaluated on later runs.
*/
assign :: !User !(Task a) -> Task a | iTask a	
assign user task = mkMainTask "assign" assign`
where
	assign` tst
		# (result,node,tst) = createOrEvaluateTaskInstance Nothing (task <<@ user) tst
		= (result,{TSt|tst & tree = node})
		
createOrEvaluateTaskInstance :: !(Maybe TaskParallelType) !(Task a) !*TSt -> (!TaskResult a, !TaskTree, !*TSt) | iTask a
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
			# user				= proc.Process.properties.managerProperties.ManagerProperties.worker
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

spawnProcess :: !User !Bool !Bool !(Task a) -> Task (ProcessRef a) | iTask a
spawnProcess user activate gcWhenDone task = mkInstantTask "spawnProcess" spawnProcess`
where
	spawnProcess` tst
		# properties	=	{ initManagerProperties
							& worker = user
							, subject = taskLabel task
							}
		# (pid,_,_,tst)	= createTaskInstance (createThread (task <<@ properties)) True Nothing activate gcWhenDone tst
		= (TaskFinished (ProcessRef pid), tst)

killProcess :: !(ProcessRef a) -> Task Void | iTask a
killProcess (ProcessRef pid) = mkInstantTask "killProcess" killProcess`
where
	killProcess` tst 
		# tst = deleteTaskInstance pid tst
		= (TaskFinished Void, tst)

waitForProcess :: (ProcessRef a) -> Task (Maybe a) | iTask a
waitForProcess (ProcessRef pid) = mkMonitorTask "waitForProcess" waitForProcess`
where
	waitForProcess` tst 
		# (mbProcess,tst) = getProcess pid tst
		= case mbProcess of
			Just {Process | taskId, properties}
				= case properties.systemProperties.SystemProperties.status of
					Finished
						# (mbResult,tst)					= loadProcessResult (taskNrFromString pid) tst	
						= case mbResult of
							Just (TaskFinished (a :: a^))	= (TaskFinished (Just a), tst)	
							_								= (TaskFinished Nothing, tst) //Ignore all other cases
					_	
						# tst = setStatus [Text "Waiting for result of task ",StrongTag [] [Text "\"",Text properties.managerProperties.subject,Text "\""]] tst
						= (TaskBusy, tst)		// We are not done yet...
			_	
				= (TaskFinished Nothing, tst)	//We could not find the process in our database, we are done
