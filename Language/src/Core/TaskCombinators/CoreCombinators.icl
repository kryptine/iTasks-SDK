implementation module CoreCombinators

import	StdList, StdArray, StdTuple, StdMisc, StdBool
from	StdFunc import id, const

import	TSt
import	Util
import	GenUpdate
import	UserDB, ProcessDB, DynamicDB
import	StdDynamic
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
				
assign :: !UserName !TaskPriority !(Maybe Timestamp) !(Task a) -> Task a | iTask a	
assign userName initPriority initDeadline task = mkMainTask "assign" (assign` userName initPriority initDeadline task) 

assign` :: !UserName !TaskPriority !(Maybe Timestamp) !(Task a) !*TSt -> (!TaskResult a, !*TSt) | iTask a
assign` toUserName initPriority initDeadline task tst =: { TSt| taskNr, taskInfo, firstRun, mainTask = currentMainTask, staticInfo = {currentProcessId}
													   , userId, delegatorId = currentDelegatorId, doChange, changes, dataStore, world}
	# taskId  			   = taskNrToString taskNr
	# (mbProc,tst) 		   = getProcess taskId tst
	# (taskStatus, taskProperties, curTask, changeNr, tst)
		= case mbProc of
			(Just {Process | status, properties, changeNr})
				# (curTask,tst) = loadTaskFunctionStatic taskNr tst
				| isNothing curTask
					= abort ("(assign) No task function stored for process " +++ taskNrToString taskNr)
				| otherwise
					= (status, properties, fromJust curTask, changeNr, tst)		
			Nothing
				# (user,tst)		= getUser toUserName tst
				# initProperties
					= {TaskManagerProperties
					  | worker		= (user.User.userName, user.User.displayName)
					  , subject		= taskLabel task
					  , priority	= initPriority
					  , deadline	= initDeadline
					}
				# (processId,tst)	= createTaskInstance task initProperties False tst
				# (mbProc,tst)		= getProcess processId tst
				= case mbProc of
					(Just {Process | status, properties, changeNr})
						= (status, properties, task, changeNr, tst)
					_
						= abort "(assign) Could not load newly created process"
				
	//Process has finished, unpack the dynamic result
	| taskStatus == Finished
		# (mbRes,tst) = loadProcessResult taskNr tst
		= case mbRes of
			Just a	= (a, tst)
			Nothing	= abort "(assign) Could not unpack process result"
	//Apply all active changes (oldest change first, hence the 'reverse changes')
	| firstRun
		= all_changes taskNr taskInfo taskProperties changeNr task curTask (reverse changes) {TSt|tst & changes = []}
	//Apply the current change change
	| doChange
		= case changes of
			[Just (clt,cid,cdyn):rest]
				= one_change taskNr taskInfo taskProperties changeNr task curTask (clt,cid,cdyn) rest tst
			other
				= do_task taskNr taskInfo taskProperties changeNr curTask tst
	| otherwise
		= do_task taskNr taskInfo taskProperties changeNr curTask tst

//Just execute the task
all_changes :: !TaskNr !TaskInfo !TaskProperties !Int !(Task a) !(Task a) ![Maybe (!ChangeLifeTime,!DynamicId,!Dynamic)] !*TSt -> (!TaskResult a,!*TSt) | iTask a
all_changes taskNr taskInfo taskProperties changeNr origTask curTask [] tst
	= do_task taskNr taskInfo taskProperties changeNr curTask tst
	
all_changes taskNr taskInfo taskProperties changeNr origTask curTask [Just (clt,cid,cdyn):cs] tst=:{TSt|changes}
	# processId = taskNrToString taskNr
	# (mbProperties,mbTask,mbChange) = appChange cdyn taskProperties curTask origTask
	# changes = case mbChange of
		(Just change)	= [Just (clt,cid,change):changes]
		Nothing			= [Nothing:changes]
	//Update task (and properties when changed) 	
	| isJust mbTask
		# changeNr			= inc changeNr 
		# taskProperties	= if (isJust mbProperties) (fromJust mbProperties) taskProperties
		# curTask			= fromJust mbTask				
		# tst				= storeTaskThread taskNr (createTaskThread curTask) tst
		# tst				= storeTaskFunctionStatic taskNr curTask tst
		# (_,tst) 			= updateProcess processId (\p -> {p & properties = taskProperties, changeNr = changeNr}) tst
		# (a,tst)			= do_task taskNr taskInfo taskProperties changeNr curTask {TSt|tst & changes = changes}
		= case cs of
			[]	-> (a,tst)
			_	-> all_changes taskNr taskInfo taskProperties changeNr origTask curTask cs {TSt|tst & changes = changes}
	//Only add properties
	| isJust mbProperties
		# taskProperties	= fromJust mbProperties
		# (_,tst) 			= updateProcess processId (\p -> {p & properties = taskProperties}) tst
		# (a,tst) 			= do_task taskNr taskInfo taskProperties changeNr curTask {TSt|tst & changes = changes}
		= case cs of
			[]	= (a,tst)
			_	= all_changes taskNr taskInfo taskProperties changeNr origTask curTask cs {TSt|tst & changes = changes}
	// Task and properties unchanged
	| otherwise
		= all_changes taskNr taskInfo taskProperties changeNr origTask curTask cs {TSt|tst & changes = changes}	

all_changes taskNr taskInfo taskProperties changeNr origTask curTask [c:cs] tst=:{TSt|changes}
	= all_changes taskNr taskInfo taskProperties changeNr origTask curTask cs {TSt|tst & changes = [c:changes]}

one_change :: !TaskNr !TaskInfo !TaskProperties !Int !(Task a) !(Task a) !(!ChangeLifeTime, !DynamicId, !Dynamic) ![Maybe (!ChangeLifeTime,!DynamicId,!Dynamic)] !*TSt -> (!TaskResult a,!*TSt) | iTask a
one_change taskNr taskInfo taskProperties changeNr origTask curTask (changeLifeTime, changeId, changeDyn) rest tst
 	# processId = taskNrToString taskNr
 	# (mbProperties, mbTask, mbChange) = appChange changeDyn taskProperties (setTaskContext [changeNr:taskNr] curTask) origTask
	//Determine new change list
	# changes = case mbChange of
			(Just change)	= [Just (changeLifeTime,changeId,change):rest]
			Nothing			= [Nothing:rest]
	//Update task (and properties when changed) 	
	| isJust mbTask
		# changeNr			= inc changeNr 
		# taskProperties	= if (isJust mbProperties) (fromJust mbProperties) taskProperties
		# curTask			= fromJust mbTask					
		# (_,tst) 			= updateProcess processId (\p -> {p & properties = taskProperties, changeNr = changeNr}) tst
		# tst				= storeTaskFunctionStatic taskNr curTask tst				//Store the changed task with context
		# tst				= storeTaskThread taskNr (createTaskThread curTask) tst
		= do_task taskNr taskInfo taskProperties changeNr curTask {TSt|tst & changes = changes} //Execute
	//Only add properties
	| isJust mbProperties
		# taskProperties	= fromJust mbProperties
		# (_,tst) 			= updateProcess processId (\p -> {p & properties = taskProperties}) tst
		= do_task taskNr taskInfo taskProperties changeNr curTask {TSt|tst & changes = changes}
	// Task and properties unchanged
	| otherwise
		= do_task taskNr taskInfo taskProperties changeNr curTask {TSt|tst & changes = changes}

do_task :: !TaskNr !TaskInfo !TaskProperties !Int !(Task a) !*TSt -> (!TaskResult a,!*TSt) | iTask a
do_task taskNr taskInfo taskProperties changeNr curTask tst=:{userId,delegatorId, mainTask}
	# tst		= {tst & tree = TTMainTask taskInfo taskProperties []
					, taskNr		= [changeNr:taskNr]
					, mainTask		= taskNrToString taskNr
					, userId		= fst taskProperties.managerProps.worker
					, delegatorId	= fst taskProperties.systemProps.manager
					}
	# (result, tst)	= applyTask curTask tst
	= (result, {TSt | tst & userId = userId, delegatorId = delegatorId, mainTask = mainTask})

//The tricky dynamic part of applying changes
appChange :: !Dynamic !TaskProperties !(Task a) !(Task a) -> (Maybe TaskProperties,Maybe (Task a), Maybe Dynamic) | iTask a
appChange (fun :: A.c: Change c | iTask c) properties curTask origTask
	= fun properties curTask origTask
appChange (fun :: Change a^) properties curTask origTask
	= fun properties curTask origTask
appChange dyn properties curTask origTask
	= (Nothing, Nothing, Just dyn)
	
setTaskContext :: TaskNr (Task a) -> (Task a)
setTaskContext cxt (Task name _ tf) = Task name (Just cxt) tf

