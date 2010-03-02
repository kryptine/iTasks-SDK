implementation module CoreCombinators

import	StdList, StdArray, StdTuple, StdMisc, StdBool
from	StdFunc import id, const

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
:: OPResult a = AllDone [(Int,a)] | PredDone [(Int,a)] | NotDone [(Int,a)]
:: PSt a b =
	{ state :: b
	, tasks :: [(Maybe User,Task a,Bool)]
	}

derive gPrint 		PSt,OPResult
derive gParse 		PSt,OPResult
derive gVisualize 	PSt,OPResult
derive gUpdate 		PSt,OPResult

derive bimap Maybe, (,)

oldParallel :: !String !([a] -> Bool) ([a] -> b) ([a] -> b) ![Task a] -> Task b | iTask a & iTask b 
oldParallel label pred predDone allDone tasks =
	parallel label (pfunc pred) (ffunc allDone predDone) (NotDone []) [(Nothing,t) \\ t <- tasks]
where
	pfunc :: ([a]->Bool) (a,Int) (OPResult a) -> ((OPResult a),PAction a) 
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

parallel :: !String !((a,Int) b -> (b,PAction a)) (b -> c) !b ![(Maybe User,Task a)] -> Task c | iTask a & iTask b & iTask c
parallel label procFun finFun initState initTasks = mkParallelTask label (parallel`)
where
	parallel` tst
		# (pst,tst) 	   	= loadPSt tst
		# (result,pst,tst) 	= processAllTasks pst 0 tst
		# tst				= setTaskStore "pst" pst tst
		= case result of
			TaskException e = (TaskException e, tst)
			TaskFinished  b = (TaskFinished (finFun b), tst)
			TaskBusy		= (TaskBusy, tst)
				
	processAllTasks pst idx tst
		| (length pst.tasks) == idx = (TaskBusy,pst,tst)
		# task	  = pst.tasks !! idx
		# (r,tst) = applyTask (snd3 task) tst
		= case r of
			TaskException e = (TaskException e,pst,tst)
			TaskBusy		= processAllTasks pst (inc idx) tst
			TaskFinished a	
				| thd3 task == True   = processAllTasks pst (inc idx) tst //This task had already been accumelated in the state
				| otherwise
					# (st,act) 		  = procFun (a,idx) pst.state //apply the task result and its index to the state
					# pst			  = {pst & state = st}
					# pst			  = markProcessed pst idx //mark the task as applied in the PState
					= case act of
						Stop		  = (TaskFinished pst.state,pst,tst) //stop the execution of the parallel and return the state
						Continue	  = processAllTasks pst (inc idx) tst //continue
						Extend etasks
							# tasks = pst.tasks ++ [(u,t,False) \\ (u,t) <- etasks] //extend the parallel with additional tasks
							# pst	= {pst & tasks = tasks}
							= processAllTasks pst (inc idx) tst
												
	loadPSt tst
		# (mbPSt,tst) = getTaskStore "pst" tst
		= case mbPSt of
			(Just p) = (p,tst)
			Nothing  = initPSt initState initTasks tst
						
	initPSt initState initTasks tst
		# pst ={ PSt 
	  	   	   | state = initState
	  	   	   , tasks = [(u,t,False) \\ (u,t) <- initTasks]
	  	   	   }
		# tst = setTaskStore "pst" pst tst
		= (pst,tst)
	  	   
	markProcessed pst idx
		# (u,t,b) 	= pst.tasks !! idx
		# tasks 	= updateAt idx (u,t,True) pst.tasks
		# pst	    = {pst & tasks = tasks}
		= pst

/****
parallel :: !String !([a] -> Bool) ([a] -> b) ([a] -> b) ![Task a] -> Task b | iTask a & iTask b
parallel label pred combinePred combineAll tasks 
	= mkParallelTask label (parallel` tasks)
where
	parallel` [] tst	=  (TaskFinished (combineAll []), tst)
	parallel` tasks tst
		# (result,tst)	= checkAllTasks tasks 0 [] tst
		= case result of
			TaskException e
				= (TaskException e,tst)
			TaskFinished list
				| pred list
					= (TaskFinished (combinePred list), tst) 		// stop, all work done so far satisfies predicate
				| length list == length tasks						
					= (TaskFinished (combineAll list), tst)			// all tasks are done
				| otherwise
					= (TaskBusy, tst)								// still busy
	where
		checkAllTasks tasks index accu tst
			| index == length tasks
				= (TaskFinished (reverse accu),tst)														// all tasks tested
			# task					= tasks !! index
			# (result,tst)			= applyTask (mkSequenceTask (taskLabel task) (applyTask task)) tst	// check tasks	
			= case result of
				TaskBusy		= checkAllTasks tasks (inc index) accu tst 
				TaskFinished a	= checkAllTasks tasks (inc index) [a:accu] tst
				TaskException e	= (TaskException e,tst)
****/

/*
* When a task is assigned to a user a synchronous task instance process is created.
* It is created once and loaded and evaluated on later runs.
*/
assign :: !UserName !TaskPriority !(Maybe Timestamp) !(Task a) -> Task a | iTask a	
assign userName initPriority initDeadline task = mkMainTask "assign" assign`
where
	assign` tst=:{TSt|taskNr}
		//Try to load the stored process for this subtask
		# taskId		= taskNrToString taskNr
		# (mbProc,tst)	= getProcess taskId tst	
		= case mbProc of
			//Nothing found, create a task instance
			Nothing	
				# (user,tst)	= getUser userName tst
				# props 		=	{TaskManagerProperties
					  				| worker	= (user.User.userName, user.User.displayName)
					  				, subject	= taskLabel task
					  				, priority	= initPriority
					  				, deadline	= initDeadline
									}
				# (result,_,tst)= createTaskInstance task props False tst
				= (result,tst)
			//When found, evaluate
			Just proc
				# (result,_,tst)= evaluateTaskInstance proc Nothing False tst
				= case result of
					TaskBusy				= (TaskBusy,tst)
					TaskFinished (a :: a^) 	= (TaskFinished a,tst)
					TaskFinished _			= (TaskException (dynamic "assign: result of wrong type returned"),tst)
					TaskException e			= (TaskException e, tst)
						 
