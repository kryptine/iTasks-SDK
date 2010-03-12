implementation module CoreCombinators

import	StdList, StdArray, StdTuple, StdMisc, StdBool
from	StdFunc import id, const
from	TaskTree import :: TaskParallelType

import	TSt
import	Util
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
			
// Parallel composition
derive gPrint 		PSt,OPResult
derive gParse 		PSt,OPResult
derive gVisualize 	PSt,OPResult
derive gUpdate 		PSt,OPResult

derive bimap Maybe, (,)

:: PSt a b =
	{ state :: b
	, tasks :: [(Task a,Bool)]
	}
	
parallel  :: !String !String !((a,Int) b -> (b,ParallelAction a)) (b -> c) !b ![Task a] -> Task c | iTask a & iTask b & iTask c
parallel label description procFun finFun initState initTasks 
	= parallelWrap label description Closed False procFun finFun initState [(Nothing,t) \\ t <- initTasks]

parallelU :: !String !String !TaskParallelType !((a,Int) b -> (b,ParallelAction a)) (b -> c) !b ![(UserId,Task a)] -> Task c | iTask a & iTask b & iTask c
parallelU label description partype procFun finFun initState initTasks 
	= parallelWrap label description partype True procFun finFun initState [(Just u,t) \\ (u,t) <- initTasks]
	
parallelWrap :: !String !String !TaskParallelType Bool !((a,Int) b -> (b,ParallelAction a)) (b -> c) !b ![(Maybe UserId,Task a)] -> Task c | iTask a & iTask b & iTask c
parallelWrap label description partype addusers procFun finFun initState initTasks = mkParallelTask label mkTpi (parallel`)
where
	parallel` tst
		# (pst,tst)   		= loadPSt tst
		# (result,pst,tst) 	= processAllTasks pst 0 tst
		# tst				= setTaskStore "pst" pst tst
		= case result of
			TaskException e = (TaskException e, tst)
			TaskFinished  b = (TaskFinished (finFun b), tst)
			TaskBusy		= (TaskBusy, tst)
				
	processAllTasks pst idx tst
		| (length pst.tasks) == idx = (TaskBusy,pst,tst)
		# task	   = pst.tasks !! idx
		# (r,tst=:{staticInfo}) = applyTask (fst task) tst			
		= case r of
			TaskException e = (TaskException e,pst,{TSt | tst & staticInfo = staticInfo})
			TaskBusy		= processAllTasks pst (inc idx) {TSt | tst & staticInfo = staticInfo}
			TaskFinished a	
				| snd task == True   = processAllTasks pst (inc idx) {TSt | tst & staticInfo = staticInfo} //This task had already been accumulated in the state
				| otherwise
					# (st,act) 		  = procFun (a,idx) pst.state //apply the task result and its index to the state
					# pst			  = {pst & state = st}
					# pst			  = markProcessed pst idx //mark the task as applied in the PState
					= case act of
						Stop		  
							= (TaskFinished pst.state,pst,tst)  //stop the execution of the parallel and return the state
						Continue	  = processAllTasks pst (inc idx) {TSt | tst & staticInfo = staticInfo} //continue
						Extend etasks
							# tasks = case addusers of 
								False 
									= pst.tasks ++ [(t,False) \\ t <- etasks]
								True
									= pst.tasks ++ [(assignTask (Just (toUserId staticInfo.currentSession.user)) t,False) \\ t <- etasks]
							# pst	= {pst & tasks = tasks}
							= processAllTasks pst (inc idx) {TSt | tst & staticInfo = staticInfo}
						ExtendU etasks
							# tasks = case addusers of
								False = pst.tasks ++ [(t,False) \\ (u,t) <- etasks]
								True  = pst.tasks ++ [(assignTask (Just u) t,False) \\ (u,t) <- etasks] //extend the parallel with additional tasks
							# pst	= {pst & tasks = tasks}
							= processAllTasks pst (inc idx) {TSt | tst & staticInfo = staticInfo}
																			
	loadPSt tst
		# (mbPSt,tst) = getTaskStore "pst" tst
		= case mbPSt of
			(Just p) = (p,tst)
			Nothing  = initPSt initState initTasks tst
						
	initPSt initState initTasks tst
		# pst ={ PSt 
	  	   	   | state = initState
	  	   	   , tasks = [(assignTask u t, False) \\ (u,t) <- initTasks]
	  	   	   }
		# tst = setTaskStore "pst" pst tst
		= (pst,tst)
	
	assignTask mbUserName task
		= case mbUserName of
			Nothing			= task
			(Just userName)	= createOrEvaluateTaskInstance userName NormalPriority Nothing (Just partype) task //deadline??
	  	   
	markProcessed pst idx
		# (t,b) 	= pst.tasks !! idx
		# tasks 	= updateAt idx (t,True) pst.tasks
		# pst	    = {pst & tasks = tasks}
		= pst
	
	mkTpi =
		{ TaskParallelInfo
		| type = partype
		, description = description
		}
			
/**
* The behaviour of the 'old' parallel combinator expressed in terms of the 'new' parallel combinator*
**/
:: OPResult a = AllDone [(Int,a)] | PredDone [(Int,a)] | NotDone [(Int,a)]

oldParallel :: !String !([a] -> Bool) ([a] -> b) ([a] -> b) ![Task a] -> Task b | iTask a & iTask b 
oldParallel label pred predDone allDone tasks =
	parallel label "The old parallel combinator" (pfunc pred) (ffunc allDone predDone) (NotDone []) tasks
where
	pfunc :: ([a]->Bool) (a,Int) (OPResult a) -> ((OPResult a),ParallelAction a) 
	pfunc pred (val,i) (NotDone st)
		# st = st++[(i,val)]
		| length st == length tasks = (AllDone st,Stop)
		| pred (stToList st) 		= (PredDone st,Stop)
		| otherwise	   		 		= (NotDone st,Continue)
	
	stToList :: [(Int,a)] -> [a]
	stToList st = [v \\ (i,v) <- st]
		
	ffunc :: ([a]->b) ([a]->b) (OPResult a) -> b
	ffunc adone pdone (AllDone v)  = adone (sortList v)
	ffunc adone pdone (PredDone v) = pdone (sortList v)
	ffunc adone pdone _			   = abort "(Old Parallel) Finish function, while not done"	

	sortList :: [(Int,a)] -> [a]
	sortList [] = []
	sortList [(i,v):ps] = sortList [(is,vs) \\ (is,vs) <- ps | is < i] ++ [v] ++ sortList [(is,vs) \\ (is,vs) <- ps | is > i]
		
/*
* When a task is assigned to a user a synchronous task instance process is created.
* It is created once and loaded and evaluated on later runs.
*/
assign :: !UserId !TaskPriority !(Maybe Timestamp) !(Task a) -> Task a | iTask a	
assign userName initPriority initDeadline task = createOrEvaluateTaskInstance userName initPriority initDeadline Nothing task
						 
createOrEvaluateTaskInstance :: !UserId !TaskPriority !(Maybe Timestamp) !(Maybe TaskParallelType) !(Task a) -> Task a | iTask a
createOrEvaluateTaskInstance userName initPriority initDeadline mbpartype task = mkMainTask "assign" createOrEvaluateTaskInstance`
where
	createOrEvaluateTaskInstance` tst=:{TSt|taskNr}
		//Try to load the stored process for this subtask
		# taskId		= taskNrToString taskNr
		# (mbProc,tst)	= getProcess taskId tst	
		= case mbProc of
			//Nothing found, create a task instance
			Nothing	
				# props 		= {TaskManagerProperties
					  				| worker		 = userName
					  				, subject		 = taskLabel task
					  				, priority		 = initPriority
					  				, deadline		 = initDeadline
					  				, tempWorkers	 = []
									}
				# tst				  = addTemporaryUser taskId userName mbpartype tst
				# (result,procId,tst) = createTaskInstance task props False tst
				# (ok,tst) = updateProcess procId (\x -> {Process | x & inParallelType = mbpartype}) tst
				= (result,tst)
			//When found, evaluate
			Just proc
				//add temp users before(!) the new proc is evaluated, because then the tst still contains the parent info
				# tst				= addTemporaryUser taskId userName mbpartype tst
				// -> TSt in subprocess
				# (result,_,tst)	= evaluateTaskInstance proc Nothing False False tst
				// <- TSt back to current process				
				//Add parallel type after the new proc is evaluated
				# (ok,tst) 			= updateProcess proc.Process.processId (\x -> {Process | x & inParallelType = mbpartype}) tst 
				= case result of
					TaskBusy				
						= (TaskBusy,tst)
					TaskFinished (a :: a^) 
						# tst = removeTemporaryUser proc.Process.processId userName mbpartype tst	 
						= (TaskFinished a,tst)
					TaskFinished _			
						# tst = removeTemporaryUser proc.Process.processId userName mbpartype tst
						= (TaskException (dynamic "assign: result of wrong type returned"),tst)
					TaskException e			
						# tst = removeTemporaryUser proc.Process.processId userName mbpartype tst
						= (TaskException e, tst)

addTemporaryUser :: !ProcessId !UserId !(Maybe TaskParallelType) !*TSt -> *TSt
addTemporaryUser procId uname mbpartype tst
		= case mbpartype of
			Nothing 		= tst
			(Just Closed) 	= tst
			(Just Open)		
				# twlist = tst.TSt.properties.managerProps.tempWorkers
				# ntwlist = [(procId,uname):[(p,u) \\ (p,u) <- twlist | not (p == procId && u == uname)]]				
				= {TSt | tst & properties = {tst.TSt.properties & managerProps = {tst.TSt.properties.managerProps & tempWorkers = ntwlist}}} 

removeTemporaryUser :: !ProcessId !UserId !(Maybe TaskParallelType) !*TSt -> *TSt			
removeTemporaryUser procId uname mbpartype tst
		= case mbpartype of
			Nothing 		= tst
			(Just Closed) 	= tst
			(Just Open)		
				# twlist = tst.TSt.properties.managerProps.tempWorkers
				# ntwlist = [(p,u) \\ (p,u) <- twlist | not (p == procId && u == uname)]				
				= {TSt | tst & properties = {tst.TSt.properties & managerProps = {tst.TSt.properties.managerProps & tempWorkers = ntwlist}}} 
						