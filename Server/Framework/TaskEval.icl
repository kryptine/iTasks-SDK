implementation module TaskEval

import StdList, StdBool
import Error
import SystemTypes, IWorld, Task, TaskContext

from CoreCombinators	import :: TaskContainer(..), :: ParallelTaskType(..), :: ParallelTask(..), :: ParallelResult
from SystemTypes		import :: ProcessId(..)
from TaskStore			import newSessionId, loadTaskInstance, storeTaskInstance

from Map		import qualified fromList, get, put
import iTaskClass

ITERATION_THRESHOLD :== 10

setRunning :: !ProgressMeta -> ProgressMeta
setRunning meta = {meta & status = Running}

setFinished :: !ProgressMeta -> ProgressMeta
setFinished meta = {meta & status = Finished}

setExcepted :: !ProgressMeta  -> ProgressMeta
setExcepted meta = {meta & status = Excepted}

createTaskContainer :: (Task a) -> Dynamic | iTask a
createTaskContainer task = (dynamic (Container task) :: Container (Task a^) a^)

createValueContainer :: a -> Dynamic | iTask a
createValueContainer val = (dynamic (Container val) :: Container a^ a^)
 	
processNo :: !ProcessId -> Int
processNo (SessionProcess _)		= 0
processNo (WorkflowProcess no)		= no
processNo (EmbeddedProcess no _)	= no 

createContext :: !ProcessId !Dynamic !ManagementMeta !User !*IWorld -> (!TaskContext, !*IWorld)
createContext processId container=:(Container task :: Container (Task a) a) mmeta user iworld=:{IWorld|localDateTime}
	# (tcontext,iworld) = (taskFuncs task).initFun (taskNo processId) iworld		
	# tmeta = taskMeta task
	# pmeta = {issuedAt = localDateTime, issuedBy = user, status = Running, firstEvent = Nothing, latestEvent = Nothing}
	= (TaskContext processId tmeta pmeta mmeta 0 (TTCRunning container tcontext),iworld)
where
	taskNo (WorkflowProcess pid)= [0,pid]
	taskNo (SessionProcess _)	= [0,0]


editInstance :: !(Maybe EditEvent) !TaskContext !*IWorld -> (!MaybeErrorString TaskContext, !*IWorld)
editInstance editEvent context=:(TaskContext processId tmeta pmeta mmeta changeNo tcontext) iworld
	= case tcontext of
		TTCRunning container=:(Container task :: Container (Task a) a) scontext
			# editFun			= (taskFuncs task).editFun
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
				Just (LuckyEvent event)
					= editFun taskNr (LuckyEvent event) scontext iworld
				_
					= (scontext, iworld)
			= (Ok (TaskContext processId tmeta pmeta mmeta changeNo (TTCRunning container scontext)), iworld)
		_
			= (Ok context, iworld)


//Evaluate the given context and yield the result of the main task indicated by target
evalInstance :: !TaskNr !(Maybe CommitEvent) !Bool !TaskContext  !*IWorld	-> (!MaybeErrorString (TaskResult Dynamic), !TaskContext, !*IWorld)
evalInstance target commitEvent genGUI context=:(TaskContext processId tmeta pmeta mmeta changeNo tcontext) iworld=:{evalStack}
	= case tcontext of
		//Eval instance
		TTCRunning container=:(Container task :: Container (Task a) a) scontext
			# evalFun			= (taskFuncs task).evalFun
			# repAs				= if genGUI (RepAsTUI task.layout) RepAsService
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
					Just (LuckyEvent e)						= Just (LuckyEvent e)
					_										= Nothing
			//Match processId & changeNo in target path
			//# target			= foldr stepTarget [changeNo,pid] target
			# target			= tl (tl target) //TODO: FIGURE OUT WHY IT DOESN'T WORK WHEN FOLDING STEPTARGET
			//Apply task's eval function	
			# (result,iworld)	= evalFun taskNo tmeta commitEvent target repAs scontext iworld 
			//Restore current process id in iworld
			# iworld			= {iworld & evalStack = evalStack}
			= case result of
				TaskInstable _ (rep,actions) scontext
					# context		= TaskContext processId tmeta (setRunning pmeta) mmeta changeNo (TTCRunning container scontext)
					= (Ok (TaskInstable Nothing (rep,actions) scontext), context, iworld)
				TaskStable val (rep,actions) scontext
					# context		= TaskContext processId tmeta (setFinished pmeta) mmeta changeNo (TTCFinished (createValueContainer val))
					= (Ok (TaskStable (createValueContainer val) (rep,actions) scontext), context, iworld)
				TaskException e str
					# context		= TaskContext processId tmeta (setExcepted pmeta) mmeta changeNo (TTCExcepted str)
					= (Ok (TaskException e str), context, iworld)
		TTCRunning container scontext
			= (Ok (taskException "Could not unpack task context"), context, iworld)
		TTCFinished r
			= (Ok (TaskStable r (NoRep,[]) TCEmpty), context, iworld)
		TTCExcepted e
			= (Ok (taskException e), context, iworld)

createSessionInstance :: !(Task a) !(Maybe EditEvent) !(Maybe CommitEvent) !Bool !*IWorld -> (!MaybeErrorString (!TaskResult Dynamic, !ProcessId), !*IWorld) |  iTask a
createSessionInstance task editEvent commitEvent genGUI iworld
	# (sessionId,iworld)	= newSessionId iworld
	# (context, iworld)		= createContext sessionId (createTaskContainer task) noMeta AnyUser iworld
	# (mbContext,iworld)	= editInstance editEvent context iworld
	= case mbContext of
		Error e				= (Error e, iworld)
		Ok context
			# (mbRes,iworld)		= iterateEval [0,0] commitEvent genGUI context iworld
			= case mbRes of
				Ok result	= (Ok (result, sessionId), iworld)
				Error e		= (Error e, iworld)

evalSessionInstance :: !ProcessId !(Maybe EditEvent) !(Maybe CommitEvent) !Bool !*IWorld -> (!MaybeErrorString (TaskResult Dynamic, !ProcessId), !*IWorld)
evalSessionInstance sessionId editEvent commitEvent genGUI iworld
	# (mbContext,iworld)	= loadTaskInstance sessionId iworld
	= case mbContext of
		Error e				= (Error e, iworld)
		Ok context
			//Apply edit event (only once)
			# (mbContext, iworld) = editInstance editEvent context iworld
			= case mbContext of
				Error e				= (Error e, iworld)
				Ok context			
					# (mbRes,iworld)	= iterateEval [0,0] commitEvent genGUI context iworld
					= case mbRes of
						Ok result	= (Ok (result, sessionId), iworld)
						Error e		= (Error e, iworld)
						
iterateEval :: !TaskNr !(Maybe CommitEvent) !Bool !TaskContext !*IWorld -> (!MaybeErrorString (TaskResult Dynamic), !*IWorld)
iterateEval target commitEvent genGUI context iworld = eval target commitEvent genGUI 1 context iworld
where
	eval :: !TaskNr !(Maybe CommitEvent) !Bool !Int !TaskContext !*IWorld -> (!MaybeErrorString (TaskResult Dynamic), !*IWorld)
	eval target commitEvent genGUI iteration context iworld
		//Initialize the toplevel task list
		# iworld 	= {iworld & parallelControls = 'Map'.fromList [(toString GlobalTaskList,(0,[]))]}
		//Reset read shares list
		# iworld			= {iworld & readShares = Just []} 
		//Evaluate top instance	
		# (mbResult, context, iworld) = evalInstance target commitEvent genGUI context iworld 
		= case mbResult of
			Error e
				= (Error e, iworld)
			Ok result
				//Process controls (eval & store additions/removals)
				# iworld				= processControls iworld
				//Check for next iteration counter: save or re-evaluate
				# iworld=:{readShares}	= iworld
				= case result of
					(TaskInstable _ _ _)
						| isNothing readShares && iteration < ITERATION_THRESHOLD
							= eval target Nothing genGUI (iteration + 1) context iworld
						| otherwise
							# iworld	= storeTaskInstance context iworld
							= (Ok result,iworld)
					_
						# iworld	= storeTaskInstance context iworld
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
					# (_,c,iworld)	= evalInstance (topTarget c) Nothing genGUI c iworld
					# iworld		= storeTaskInstance c iworld
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
				(Embedded,tfun)			= (createTaskContainer (tfun GlobalTaskList),{noMeta & worker = Just user})
				(Detached props,tfun)	= (createTaskContainer (tfun GlobalTaskList), props)	
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
