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
	
//TODO : CLEAN UP!
group :: !String !String !((a,Int) b -> (b,PAction (Task a) tag)) (b -> c) !b ![Task a] ![GroupAction a b s] -> Task c | iTask a & iTask b & iTask c & iTask s
group label description procFun parseFun initState initTasks groupActions = execInGroup Nothing label description procFun parseFun initState initTasks (Just groupActions)

execInGroup :: !(Maybe TaskParallelType) !String !String !((a,Int) b -> (b,PAction (Task a) tag)) (b->c) !b ![Task a] !(Maybe [GroupAction a b s]) -> Task c | iTask a & iTask b & iTask c & iTask s
execInGroup mbParType label description procFun parseFun initState initTasks mbGroupActions =
	case mbParType of
		(Nothing) = makeTaskNode label Nothing execInGroup`
		(Just pt) = makeTaskNode label (Just (mkTpi pt)) execInGroup`
where
	execInGroup` tst=:{taskNr,request}
		# taskNr			= drop 1 taskNr // get taskNr of group-task
		# (updates,tst)		= getChildrenUpdatesFor taskNr tst
		# (pst,tst)   		= loadPSt taskNr tst
		| otherwise		
		// check for group actions
		# (gActionStop,mbFocus,pst) = case mbGroupActions of
			Just gActions
				# gAction = case parseString (http_getValue "_group" updates "") of
					Nothing	= parseString (http_getValue "menuAndGroup" updates "")
					res		= res
				= case gAction of
					Just action	= case filter (\act -> (getAction act) == action) gActions of
						[gAction:_]
							# (nSt,act) = procFun (getResult action gAction,-1) pst.state
							# pst = {pst & state = nSt}
							= case act of
								Stop			= (True,Nothing,pst)
								Continue		= (False,Nothing,pst)
								Extend tlist	= (False,Nothing,{PSt | pst & tasks = pst.tasks ++ [(assignTask task,False) \\ task <- tlist]})
								Focus tag		= (False,Just tag,pst)
						_ = (False,Nothing,pst)
					Nothing = (False,Nothing,pst)
			Nothing = (False,Nothing,pst)		
		# (result,pst,tst,mbFocus) 	= processAllTasks pst 0 tst mbFocus
		# tst						= setTaskStoreFor taskNr "pst" pst tst
		= case result of
			TaskException e 
				//remove all subtask workers from this process at once, as it is in Exception state, so their temporary rights to see this parallel should be revoked.
				//# tst = clearSubTaskWorkers (taskNrToString taskNr) mbParType tst
				= (TaskException e,tst)
			TaskFinished  r 
				//remove all subtask workers from this process at once, as it is finished.
				//# tst = clearSubTaskWorkers (taskNrToString taskNr) mbParType tst
				= (TaskFinished (parseFun r),tst)
			TaskBusy
				| gActionStop	= (TaskFinished (parseFun pst.state),tst)
				| otherwise
					# tst = case mbGroupActions of
						Just gActions	= setGroupActions (evaluateConditions gActions pst.state) tst
						Nothing			= tst
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
						# pst = {PSt | pst & tasks = pst.tasks ++ [(assignTask task,False) \\ task <- tlist]}
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
				, tasks = [(assignTask task, False) \\ task <- initTasks]
				}
		# tst = setTaskStoreFor taskNr "pst" pst tst
		= (pst,tst)

	assignTask task
		= case (taskUser task) of
			AnyUser = task 			//Just let the current user do it
			user	= createOrEvaluateTaskInstance user mbParType task
			
	mkTpi parType =
		{ TaskParallelInfo
		| type = parType
		, description = description
		}

	makeTaskNode label Nothing 	f = mkGroupedTask label f
	makeTaskNode label (Just p)	f = mkParallelTask label p f
	
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
	| isEmpty initTasks	= return (parseFun initState)
	| otherwise = mkParallelTask label {TaskParallelInfo | type = parType, description = description} execInParallel
where
	execInParallel tst=:{taskNr}
		# parTaskNr			= tl taskNr		
		# (pst,tst)			= loadPSt parTaskNr tst
		# (result,pst,tst)	= processAllTasks pst 0 tst
		# tst				= setTaskStoreFor parTaskNr "pst" pst tst	
		= case result of
			TaskException e
				= (TaskException e,tst)
			TaskFinished r
				# tst = clearSubTaskWorkers (taskNrToString parTaskNr) (Just parType) tst
				= (TaskFinished (parseFun r),tst)
			TaskBusy
				= (TaskBusy,tst)
	
	processAllTasks pst idx tst=:{TSt | properties}
		| (length pst.tasks) == idx = (TaskBusy,pst,tst)
		# (task,done)				= pst.tasks !! idx
		# (res,tst)					= applyTask task tst
		= case res of
			TaskException e = (TaskException e,pst,tst)
			TaskBusy		= processAllTasks pst (inc idx) tst
			TaskFinished a	
				| done			= processAllTasks pst (inc idx) tst
				# (nSt,act)		= procFun (a,idx) pst.state
				# pst			= markProcessed {PSt | pst & state = nSt} idx
				= case act of
					Stop 		= (TaskFinished pst.state,pst,tst)
					Continue	= processAllTasks pst (inc idx) tst
					Extend tlist
						# pst = {PSt | pst & tasks = pst.tasks ++ [(assignTask task properties.managerProperties.ManagerProperties.worker,False) \\ task <- tlist]}
						= processAllTasks pst (inc idx) tst
		
	loadPSt taskNr tst
		# (mbPSt,tst) = getTaskStoreFor taskNr "pst" tst
		= case mbPSt of	(Just p) = (p,tst); Nothing  = initPSt taskNr tst
	
	initPSt taskNr tst=:{TSt | properties}
		# pst = { PSt
				| state = initState
				, tasks = [(assignTask task properties.managerProperties.ManagerProperties.worker, False) \\ task <- initTasks]
				}
		# tst = setTaskStoreFor taskNr "pst" pst tst
		= (pst,tst)

	assignTask task worker
		= case (taskUser task) of
			AnyUser = createOrEvaluateTaskInstance worker (Just parType) task //Just let the current user (worker) do it
			user	= createOrEvaluateTaskInstance user (Just parType) task	
		
	markProcessed pst idx
	# (t,b) 	= pst.tasks !! idx
	# tasks 	= updateAt idx (t,True) pst.tasks
	= {PSt | pst & tasks = tasks}			


/*
* When a task is assigned to a user a synchronous task instance process is created.
* It is created once and loaded and evaluated on later runs.
*/

assign :: !User !(Task a) -> Task a | iTask a	
assign user task = createOrEvaluateTaskInstance user Nothing task

createOrEvaluateTaskInstance :: !User !(Maybe TaskParallelType) !(Task a) -> Task a | iTask a
createOrEvaluateTaskInstance user mbpartype task = mkMainTask "assign" createOrEvaluateTaskInstance`
where
	createOrEvaluateTaskInstance` tst=:{TSt|taskNr,updates}
		//Try to load the stored process for this subtask
		# taskId		 = taskNrToString taskNr
		# (mbProc,tst)	 = getProcess taskId tst	
		= case mbProc of
			//Nothing found, create a task instance
			Nothing	
				# tst				  = addSubTaskWorker taskId user mbpartype tst
				# (resDyn,procId,tst) = createTaskInstance (createThread (task <<@ user)) False mbpartype True False tst
				= case resDyn of
					(result :: TaskResult a^)	= (result, tst)
					_							= abort "createOrEvaluateTaskIntance: task result of invalid type!"
			//When found, evaluate
			Just proc
				//add temp users before(!) the new proc is evaluated, because then the tst still contains the parent info
				# tst				= addSubTaskWorker taskId user mbpartype tst
				// -> TSt in subprocess
				# (result,_,tst)	= evaluateTaskInstance proc updates Nothing False False tst
				// <- TSt back to current process				
				//Add parallel type after the new proc is evaluated
				= case result of
					TaskBusy				
						= (TaskBusy,tst)
					TaskFinished (a :: a^) 
						//Comment out(?): Subtask workers should not be removed here, because when their access is removed, they cannot see the results of the other users
						//# tst = removeSubTaskWorker proc.Process.taskId user mbpartype tst	 
						= (TaskFinished a,tst)
					TaskFinished _			
						# tst = removeSubTaskWorker proc.Process.taskId user mbpartype tst
						= (TaskException (dynamic "assign: result of wrong type returned"),tst)
					TaskException e			
						# tst = removeSubTaskWorker proc.Process.taskId user mbpartype tst
						= (TaskException e, tst)

addSubTaskWorker :: !ProcessId !User !(Maybe TaskParallelType) !*TSt -> *TSt
addSubTaskWorker procId user mbpartype tst
		= case mbpartype of
			Nothing 		= tst
			(Just Closed) 	= tst
			(Just Open)		= {TSt | tst & properties = {tst.TSt.properties & systemProperties = {tst.TSt.properties.systemProperties & subTaskWorkers = removeDup [(procId,user):tst.TSt.properties.systemProperties.subTaskWorkers]}}} 

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
		# (result,pid,tst)	= createTaskInstance (createThread (task <<@ properties)) True Nothing activate gcWhenDone tst
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
			Just {Process | taskId, status, properties}
				= case status of
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

	