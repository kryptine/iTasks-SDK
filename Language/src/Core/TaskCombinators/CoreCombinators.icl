implementation module CoreCombinators

import	StdList, StdArray, StdTuple, StdMisc, StdBool
from	StdFunc import id, const
from	TaskTree import :: TaskParallelType

import	TSt
import	Util, Http
import	GenUpdate
import	UserDB, ProcessDB
import  Store

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

class PActionClass t where
	getName :: (t a) -> Maybe UserName
	getTask :: (t a) -> Task a
	
instance PActionClass AssignedTask where
	getName :: (AssignedTask a) -> Maybe UserName
	getName at = (Just at.AssignedTask.user)
	
	getTask :: (AssignedTask a) -> Task a
	getTask at = at.AssignedTask.task
	
instance PActionClass Task where
	getName :: (Task a) -> Maybe UserName
	getName ta = Nothing
	
	getTask :: (Task a) -> Task a
	getTask ta = ta

:: PSt a b =
	{ state :: b
	, tasks :: [(Task a,Bool)]
	}

import GenPrint
//:: PAction t a = Stop | Continue | Extend .[t a]
derive gPrint PAction

parallel :: !TaskParallelType !String !String !((a,Int) b -> (b,PAction (AssignedTask a))) (b -> c) !b ![AssignedTask a] -> Task c | iTask a & iTask b & iTask c
parallel type label description procFun parseFun initState initTask = execInParallel (Just type) label description procFun parseFun initState initTask nothing
where
	nothing :: Maybe [GroupAction a b Void]
	nothing = Nothing

group :: !String !String !((a,Int) b -> (b,PAction (Task a))) (b -> c) !b ![Task a] ![GroupAction a b s] -> Task c | iTask a & iTask b & iTask c & iTask s
group label description procFun parseFun initState initTasks groupActions = execInParallel Nothing label description procFun parseFun initState initTasks (Just groupActions)

execInParallel :: !(Maybe TaskParallelType) !String !String !((a,Int) b -> (b,PAction (t a))) (b->c) !b ![t a] !(Maybe [GroupAction a b s]) -> Task c | iTask a & iTask b & iTask c & PActionClass t & iTask s
execInParallel mbParType label description procFun parseFun initState initTasks mbGroupActions =
	case mbParType of
		(Nothing) = makeTaskNode label Nothing execInParallel`
		(Just pt) = makeTaskNode label (Just (mkTpi pt)) execInParallel`
where
	execInParallel` tst=:{taskNr,request}
		# taskNr			= drop 1 taskNr // get taskNr of group-task
		# (updates,tst)		= getChildrenUpdatesFor taskNr tst
		# (pst,tst)   		= loadPSt taskNr tst
		// check for group actions
		# (gActionStop,pst) = case mbGroupActions of
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
								Stop			= (True,pst)
								Continue		= (False,pst)
								Extend tlist	= (False,{PSt | pst & tasks = pst.tasks ++ [(assignTask task,False) \\ task <- tlist]})
						_ = (False,pst)
					Nothing = (False,pst)
			Nothing = (False,pst)
		# (result,pst,tst) 	= processAllTasks pst 0 tst
		# tst				= setTaskStoreFor taskNr "pst" pst tst
		= case result of
			TaskException e = (TaskException e,tst)
			TaskFinished  r = (TaskFinished (parseFun r),tst)
			TaskBusy
				| gActionStop	= (TaskFinished (parseFun pst.state),tst)
				| otherwise
					# tst = case mbGroupActions of
						Just gActions	= setGroupActions (evaluateConditions gActions pst.state) tst
						Nothing			= tst
					= (TaskBusy,tst)

	processAllTasks pst idx tst
		| (length pst.tasks) == idx = (TaskBusy,pst,tst)
		# (task,done)				= pst.tasks !! idx
		# (res,tst)					= applyTask task tst
		= case res of
			TaskException e = (TaskException e,pst,tst)
			TaskBusy		= processAllTasks pst (inc idx) tst
			TaskFinished a	
				| done			= processAllTasks pst (inc idx) tst
				# (nSt,act)	= procFun (a,idx) pst.state
				# pst		= markProcessed {PSt | pst & state = nSt} idx
				= case act of
					Stop 		= (TaskFinished pst.state,pst,tst)
					Continue	= processAllTasks pst (inc idx) tst
					Extend tlist
						# pst = {PSt | pst & tasks = pst.tasks ++ [(assignTask task,False) \\ task <- tlist]}
						= processAllTasks pst (inc idx) tst

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

	assignTask atask
		# mbUser = getName atask
		# task	 = getTask atask
		= case mbUser of
			(Nothing)  = task
			(Just usr) = createOrEvaluateTaskInstance usr NormalPriority Nothing mbParType task
	
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
		
		checkSharedPred id p tst=:{TSt|dataStore,world}
			# (mbVal,dstore,world)	= loadValue id dataStore world
			# tst					= {TSt|tst & dataStore = dstore, world = world}
			= case mbVal of
				Just val	= (p (SharedValue val), tst)
				Nothing		= (p SharedDeleted, tst)
				
	getAction	(GroupAction a _ _)			= a
	getAction	(GroupActionParam name _ _)	= ActionParam name "?"
	
	getCond		(GroupAction _ _ cond)		= cond
	getCond		(GroupActionParam _ _ cond)	= cond
	
	getResult	(ActionParam _ param)	(GroupActionParam _ f _)	= f param
	getResult	_						(GroupAction _ res _)		= res
	
				
/*
* When a task is assigned to a user a synchronous task instance process is created.
* It is created once and loaded and evaluated on later runs.
*/
class assign u :: u !TaskPriority !(Maybe Timestamp) !(Task a) -> Task a	| iTask a

instance assign UserName
where
	assign :: !UserName !TaskPriority !(Maybe Timestamp) !(Task a) -> Task a | iTask a	
	assign userName initPriority initDeadline task = createOrEvaluateTaskInstance userName initPriority initDeadline Nothing task

instance assign User
where
	assign :: !User !TaskPriority !(Maybe Timestamp) !(Task a) -> Task a | iTask a	
	assign user initPriority initDeadline task = createOrEvaluateTaskInstance (toUserName user) initPriority initDeadline Nothing task

createOrEvaluateTaskInstance :: !UserName !TaskPriority !(Maybe Timestamp) !(Maybe TaskParallelType) !(Task a) -> Task a | iTask a
createOrEvaluateTaskInstance userName initPriority initDeadline mbpartype task = mkMainTask "assign" createOrEvaluateTaskInstance`
where
	createOrEvaluateTaskInstance` tst=:{TSt|taskNr}
		//Try to load the stored process for this subtask
		# taskId		 = taskNrToString taskNr
		# (mbProc,tst)	 = getProcess taskId tst	
		= case mbProc of
			//Nothing found, create a task instance
			Nothing	
				# (userName,tst)= tidyUserName userName tst
				# props 		= {TaskManagerProperties
					  				| worker		 = userName
					  				, subject		 = taskLabel task
					  				, priority		 = initPriority
					  				, deadline		 = initDeadline
									}
				# tst				  = addSubTaskWorker taskId userName mbpartype tst
				# (resDyn,procId,tst) = createTaskInstance (createThread task (Just props)) False mbpartype True False tst
				= case resDyn of
					(result :: TaskResult a^)	= (result, tst)
					_							= abort "createOrEvaluateTaskIntance: task result of invalid type!"
			//When found, evaluate
			Just proc
				//add temp users before(!) the new proc is evaluated, because then the tst still contains the parent info
				# tst				= addSubTaskWorker taskId userName mbpartype tst
				// -> TSt in subprocess
				# (result,_,tst)	= evaluateTaskInstance proc Nothing False False tst
				// <- TSt back to current process				
				//Add parallel type after the new proc is evaluated
				= case result of
					TaskBusy				
						= (TaskBusy,tst)
					TaskFinished (a :: a^) 
						# tst = removeSubTaskWorker proc.Process.processId userName mbpartype tst	 
						= (TaskFinished a,tst)
					TaskFinished _			
						# tst = removeSubTaskWorker proc.Process.processId userName mbpartype tst
						= (TaskException (dynamic "assign: result of wrong type returned"),tst)
					TaskException e			
						# tst = removeSubTaskWorker proc.Process.processId userName mbpartype tst
						= (TaskException e, tst)

addSubTaskWorker :: !ProcessId !UserName !(Maybe TaskParallelType) !*TSt -> *TSt
addSubTaskWorker procId uname mbpartype tst
		= case mbpartype of
			Nothing 		= tst
			(Just Closed) 	= tst
			(Just Open)		
				# stwlist = tst.TSt.properties.systemProps.subTaskWorkers
				# nstwlist = [(procId,uname):[(p,u) \\ (p,u) <- stwlist | not (p == procId && u == uname)]]				
				= {TSt | tst & properties = {tst.TSt.properties & systemProps = {tst.TSt.properties.systemProps & subTaskWorkers = nstwlist}}} 

removeSubTaskWorker :: !ProcessId !UserName !(Maybe TaskParallelType) !*TSt -> *TSt			
removeSubTaskWorker procId uname mbpartype tst
		= case mbpartype of
			Nothing 		= tst
			(Just Closed) 	= tst
			(Just Open)		
				# stwlist = tst.TSt.properties.systemProps.subTaskWorkers
				# nstwlist = [(p,u) \\ (p,u) <- stwlist | not (p == procId && u == uname)]				
				= {TSt | tst & properties = {tst.TSt.properties & systemProps = {tst.TSt.properties.systemProps & subTaskWorkers = nstwlist}}} 
						