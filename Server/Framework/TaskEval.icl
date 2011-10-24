implementation module TaskEval

import StdList, StdBool
import Error
import SystemTypes, IWorld, Task, TaskContext, WorkflowDB

from CoreCombinators	import :: TaskContainer(..), :: ParallelTaskType(..), :: ParallelTask(..), :: ParallelControl
from SystemTypes		import :: ProcessId(..)

from ProcessDB	import qualified newSessionId, loadTaskInstance, storeTaskInstance
from Map		import qualified fromList, get, put
import iTaskClass

ITERATION_THRESHOLD :== 10

setRunning :: !ProgressMeta -> ProgressMeta
setRunning meta = {meta & status = Running}

setFinished :: !ProgressMeta -> ProgressMeta
setFinished meta = {meta & status = Finished}

setExcepted :: !ProgressMeta  -> ProgressMeta
setExcepted meta = {meta & status = Excepted}

createThread :: (Task a) -> Dynamic | iTask a
createThread task = (dynamic container :: Container (TaskThread a^) a^)
where
 	container = Container {TaskThread|originalTask = task, currentTask = task}

processNo :: !ProcessId -> Int
processNo (SessionProcess _)		= 0
processNo (WorkflowProcess no)		= no
processNo (EmbeddedProcess no _)	= no 

createContext :: !ProcessId !Dynamic !ManagementMeta !User !*IWorld -> (!TaskContext, !*IWorld)
createContext processId thread=:(Container {TaskThread|originalTask} :: Container (TaskThread a) a) mmeta user iworld=:{IWorld|localDateTime}
	# originalTaskFuncs = taskFuncs originalTask
	# (tcontext,iworld) = originalTaskFuncs.initFun (taskNo processId) iworld		
	# tmeta = taskMeta originalTask
	# pmeta = {issuedAt = localDateTime, issuedBy = user, status = Running, firstEvent = Nothing, latestEvent = Nothing}
	= (TaskContext processId tmeta pmeta mmeta 0 (TTCRunning thread tcontext),iworld)
where
	taskNo (WorkflowProcess pid)= [0,pid]
	taskNo (SessionProcess _)	= [0,0]


editInstance :: !(Maybe EditEvent) !TaskContext !*IWorld -> (!MaybeErrorString TaskContext, !*IWorld)
editInstance editEvent context=:(TaskContext processId tmeta pmeta mmeta changeNo tcontext) iworld
	= case tcontext of
		TTCRunning thread=:(Container {TaskThread|currentTask} :: Container (TaskThread a) a) scontext
			# editFun			= (taskFuncs currentTask).editFun
			# procNo			= processNo processId
			# taskNr			= [changeNo,procNo]
			# (scontext,iworld) = case editEvent of
				Just (ProcessEvent [p,c:steps] event)
					| p == procNo && c == changeNo
						= editFun taskNr (TaskEvent steps event) scontext iworld
					| otherwise	
						= editFun taskNr (ProcessEvent [p,c:steps] event) scontext iworld
				Just (ProcessEvent steps event)
					= editFun taskNr (ProcessEvent steps event) scontext iworld
				_
					= (scontext, iworld)
			= (Ok (TaskContext processId tmeta pmeta mmeta changeNo (TTCRunning thread scontext)), iworld)
		_
			= (Ok context, iworld)


//Evaluate the given context and yield the result of the main task indicated by target
evalInstance :: !TaskNr !(Maybe CommitEvent) !TaskContext  !*IWorld	-> (!MaybeErrorString (TaskResult Dynamic), !TaskContext, !*IWorld)
evalInstance target commitEvent context=:(TaskContext processId tmeta pmeta mmeta changeNo tcontext) iworld=:{evalStack}
	= case tcontext of
		//Eval instance
		TTCRunning thread=:(Container {TaskThread|currentTask} :: Container (TaskThread a) a) scontext
			# evalFun			= (taskFuncs currentTask).evalFun
			# (ilayout,playout)	= taskLayouters currentTask
			# procNo			= processNo processId
			# taskNo			= [changeNo,procNo]
			//Update current process id & eval stack in iworld
			# iworld			= {iworld & evalStack = [processId:evalStack]} 
			//Strip the process id and change number from the commit event and switch from ProcessEvent to TaskEvent if it matches
			# commitEvent = case commitEvent of
					Just (ProcessEvent [p,c:steps] action)
						| p == procNo && c == changeNo		= Just (TaskEvent steps action)
						| otherwise							= Just (ProcessEvent [p,c:steps] action)
					Just (ProcessEvent steps action)		= Just (ProcessEvent steps action)
					_										= Nothing
			//Match processId & changeNo in target path
			//# target			= foldr stepTarget [changeNo,pid] target
			# target			= tl (tl target) //TODO: FIGURE OUT WHY IT DOESN'T WORK WHEN FOLDING STEPTARGET
			//Apply task's eval function	
			# (result,iworld)	= evalFun taskNo tmeta commitEvent target ilayout playout scontext iworld 
			//Restore current process id in iworld
			# iworld			= {iworld & evalStack = evalStack}
			= case result of
				TaskBusy tui actions scontext
				//	# properties	= setRunning properties
					# context		= TaskContext processId tmeta (setRunning pmeta) mmeta changeNo (TTCRunning thread scontext)
					= (Ok (TaskBusy tui actions scontext), context, iworld)
				TaskFinished val
					# context		= TaskContext processId tmeta (setFinished pmeta) mmeta changeNo (TTCFinished (dynamic val))
					= (Ok (TaskFinished (dynamic val)), context, iworld)
				TaskException e str
					# context		= TaskContext processId tmeta (setExcepted pmeta) mmeta changeNo (TTCExcepted str)
					= (Ok (TaskException e str), context, iworld)
		TTCFinished r
			= (Ok (TaskFinished r), context, iworld)
		TTCExcepted e
			= (Ok (taskException e), context, iworld)

createSessionInstance :: !(Task a) !*IWorld -> (!MaybeErrorString (!TaskResult Dynamic, !ProcessId), !*IWorld) |  iTask a
createSessionInstance task iworld
	# (sessionId,iworld)	= 'ProcessDB'.newSessionId iworld
	# (context, iworld)		= createContext sessionId (createThread task) noMeta AnyUser iworld
	# (mbRes,iworld)		= iterateEval [0,0] Nothing context iworld
	= case mbRes of
		Ok result	= (Ok (result, sessionId), iworld)
		Error e		= (Error e, iworld)

evalSessionInstance :: !ProcessId !(Maybe EditEvent) !(Maybe CommitEvent) !*IWorld -> (!MaybeErrorString (TaskResult Dynamic), !*IWorld)
evalSessionInstance sessionId editEvent commitEvent iworld
	# (mbContext,iworld)	= 'ProcessDB'.loadTaskInstance sessionId iworld
	= case mbContext of
		Error e				= (Error e, iworld)
		Ok context
			//Apply edit event (only once)
			# (mbContext, iworld) = editInstance editEvent context iworld
			= case mbContext of
				Error e				= (Error e, iworld)
				Ok context			= iterateEval [0,0] commitEvent context iworld
	
iterateEval :: !TaskNr !(Maybe CommitEvent) !TaskContext !*IWorld -> (!MaybeErrorString (TaskResult Dynamic), !*IWorld)
iterateEval target commitEvent context iworld = eval target commitEvent 1 context iworld
where
	eval :: !TaskNr !(Maybe CommitEvent) !Int !TaskContext !*IWorld -> (!MaybeErrorString (TaskResult Dynamic), !*IWorld)
	eval target commitEvent iteration context iworld
		//Initialize the toplevel task list
		# iworld 	= {iworld & parallelControls = 'Map'.fromList [(toString GlobalTaskList,(0,[]))]}
		//Reset read shares list
		# iworld			= {iworld & readShares = Just []} 
		//Evaluate top instance	
		# (mbResult, context, iworld) = evalInstance target commitEvent context iworld 
		= case mbResult of
			Error e
				= (Error e, iworld)
			Ok result
				//Process controls (eval & store additions/removals)
				# iworld				= processControls iworld
				//Check for next iteration counter: save or re-evaluate
				# iworld=:{readShares}	= iworld
				= case result of
					(TaskBusy _ _ _)
						| isNothing readShares && iteration < ITERATION_THRESHOLD
							= eval target Nothing (iteration + 1) context iworld
						| otherwise
							# iworld	= 'ProcessDB'.storeTaskInstance context iworld
							= (Ok result,iworld)
					_
						# iworld	= 'ProcessDB'.storeTaskInstance context iworld
						= (Ok result,iworld)
		
	processControls :: !*IWorld -> *IWorld
	processControls iworld = processControls` [] iworld
	where	
		processControls` queue iworld=:{currentUser}
			//Check for additions/removals in the toplevel task list
			# (controls, iworld)	= getControls iworld
			//Execute the additions/removals. Additions are appended to the queue to be evaluated
			# (queue, iworld)		= execControls controls queue iworld
			//Eval an element of the queue and recurse
			= case queue of 
				[]		= iworld
				[c:cs]
					//Evaluate and store the context only. We do not want any result	
					# iworld		= {iworld & currentUser = issueUser c}
					# (_,c,iworld)	= evalInstance (topTarget c) Nothing c iworld
					# iworld		= 'ProcessDB'.storeTaskInstance c iworld
					= processControls` cs {iworld & currentUser = currentUser}
	
	//Extracts controls and resets the toplevel task list
	getControls iworld=:{parallelControls}
		= case 'Map'.get (toString GlobalTaskList) parallelControls of
			Just (_,controls)	= (controls, {iworld & parallelControls = 'Map'.put (toString GlobalTaskList)(0,[]) parallelControls})
			_					= ([],iworld)
	
	execControls [] queue iworld = (queue,iworld)
	execControls [c:cs] queue iworld = case c of
		AppendTask pid user (container :: TaskContainer Void)
			//Make thread and properties
			# (thread,managerProperties) = case container of
				(Embedded,tfun)			= (createThread (tfun GlobalTaskList),{noMeta & worker = Just user})
				(Detached props,tfun)	= (createThread (tfun GlobalTaskList), props)	
			//Make context
			# (context,iworld) = createContext (WorkflowProcess pid) thread managerProperties user iworld			
			= execControls cs (queue ++ [context]) iworld
		//TODO: RemoveTask on the global list
		_
			= execControls cs queue iworld		
	

	topTarget (TaskContext processId _ _ _ changeNo _)
		= case processId of 
			(SessionProcess _)			= [0,0]
			(WorkflowProcess procNo)	= [changeNo,procNo]
			(EmbeddedProcess _ taskId)	= taskNrFromString taskId
			
			
	issueUser (TaskContext _ _ {ProgressMeta|issuedBy} _ _ _)
		= issuedBy