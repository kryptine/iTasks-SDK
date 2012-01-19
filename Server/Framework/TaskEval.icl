implementation module TaskEval

import StdList, StdBool
import Error
import SystemTypes, IWorld, Task, TaskContext
import LayoutCombinators

from CoreCombinators	import :: TaskContainer(..), :: ParallelTaskType(..), :: ParallelTask(..), :: ParallelResult
from TaskStore			import newSessionId, loadTaskInstance, storeTaskInstance

from Map				import qualified fromList, get, put
import iTaskClass

ITERATION_THRESHOLD :== 10

setRunning :: !ProgressMeta -> ProgressMeta
setRunning meta = {meta & status = Running}

setFinished :: !ProgressMeta -> ProgressMeta
setFinished meta = {meta & status = Finished}

setExcepted :: !ProgressMeta  -> ProgressMeta
setExcepted meta = {meta & status = Excepted}

getTaskMeta	:: !TaskRep -> [(!String,!String)]
getTaskMeta NoRep				= []
getTaskMeta (TUIRep (_,_,attr))	= attr
getTaskMeta (ServiceRep _)		= [] //TODO

createTaskContainer :: (Task a) -> Dynamic | iTask a
createTaskContainer task = (dynamic (Container task) :: Container (Task a^) a^)

createValueContainer :: a -> Dynamic | iTask a
createValueContainer val = (dynamic (Container val) :: Container a^ a^)

createContext :: !(Either SessionId TopNo) !Dynamic !ManagementMeta !User !*IWorld -> (!TaskContext, !*IWorld)
createContext topId container=:(Container task :: Container (Task a) a) mmeta user iworld =:{IWorld|localDateTime}
	# taskId				= case topId of
		Left _		= TaskId 0 0
		Right topNo	= TaskId topNo 0
	# (tcontext,iworld) 	= task.initFun taskId {iworld & nextTaskNo = 1, evalStack = [taskId]}		
	# (nextTaskNo,iworld)	= getNextTaskNo iworld 
	# pmeta	 = {issuedAt = localDateTime
			   ,issuedBy = user
			   , status  = Running
			   , firstEvent = Nothing
			   , latestEvent = Nothing
			   }
	= (TaskContext topId nextTaskNo pmeta mmeta [] (TTCRunning container tcontext),iworld)
where
	getNextTaskNo iworld=:{nextTaskNo} = (nextTaskNo,iworld)
	
editInstance :: !(Maybe EditEvent) !TaskContext !*IWorld -> (!MaybeErrorString TaskContext, !*IWorld)
editInstance editEvent context=:(TaskContext topId nextTaskNo pmeta mmeta tmeta tcontext) iworld=:{evalStack}
	= case tcontext of
		TTCRunning container=:(Container task :: Container (Task a) a) scontext
			# taskId					= case topId of
				(Left _)		= TaskId 0 0
				(Right topNo)	= TaskId topNo 0
				
			# (scontext,iworld)	= case editEvent of
				Just event	= task.editFun event scontext {iworld & evalStack = [taskId:evalStack]}
				_			= (scontext,iworld)
			# iworld		= {iworld & evalStack = evalStack}
			= (Ok (TaskContext topId nextTaskNo pmeta mmeta tmeta (TTCRunning container scontext)), iworld)
		_
			= (Ok context, iworld)

//Evaluate the given context and yield the result of the main task indicated by target
evalInstance :: !(Maybe EditEvent) !(Maybe CommitEvent) !(Maybe TaskId) !Bool !TaskContext  !*IWorld -> (!MaybeErrorString (TaskResult Dynamic), !TaskContext, !*IWorld)
evalInstance editEvent commitEvent repTarget genGUI context=:(TaskContext topId curNextTaskNo pmeta mmeta _ tcontext) iworld=:{nextTaskNo,evalStack}
	= case tcontext of
		//Eval instance
		TTCRunning container=:(Container task :: Container (Task a) a) scontext
			# evalFun					= task.evalFun
			# repAs						= if genGUI (RepAsTUI repTarget task.layout) (RepAsService repTarget)
			//Update current process id & eval stack in iworld
			# taskId					= case topId of
				(Left _)		= TaskId 0 0
				(Right topNo)	= TaskId topNo 0
			# iworld					= {iworld & evalStack = [taskId:evalStack], nextTaskNo = curNextTaskNo} 
			//Apply task's eval function and take updated nextTaskId from iworld
			# (result,iworld)			= evalFun editEvent commitEvent repAs scontext iworld
			# (updNextTaskNo,iworld)	= getNextTaskNo iworld
			//Restore current process id & nextTask id in iworld
			# iworld			= {iworld & nextTaskNo = nextTaskNo, evalStack = evalStack}
			= case result of
				TaskInstable _ rep scontext
					# context		= TaskContext topId updNextTaskNo (setRunning pmeta) mmeta (getTaskMeta rep) (TTCRunning container scontext)
					= (Ok (TaskInstable Nothing rep scontext), context, iworld)
				TaskStable val rep scontext
					# context		= TaskContext topId updNextTaskNo (setFinished pmeta) mmeta (getTaskMeta rep) (TTCFinished (createValueContainer val))
					= (Ok (TaskStable (createValueContainer val) rep scontext), context, iworld)
				TaskException e str
					# context		= TaskContext topId updNextTaskNo (setExcepted pmeta) mmeta [] (TTCExcepted str)
					= (Ok (TaskException e str), context, iworld)
		TTCRunning container scontext
			= (Ok (taskException "Could not unpack task context"), context, iworld)
		TTCFinished r
			= (Ok (TaskStable r NoRep (TCEmpty (TaskId 0 0))), context, iworld)
		TTCExcepted e
			= (Ok (taskException e), context, iworld)
where
	getNextTaskNo iworld=:{nextTaskNo} = (nextTaskNo,iworld)

createSessionInstance :: !(Task a) !(Maybe EditEvent) !(Maybe CommitEvent) !Bool !*IWorld -> (!MaybeErrorString (!TaskResult Dynamic, !SessionId), !*IWorld) |  iTask a
createSessionInstance task editEvent commitEvent genGUI iworld
	# (sessionId,iworld)	= newSessionId iworld
	# (context, iworld)		= createContext (Left sessionId) (createTaskContainer task) noMeta AnyUser iworld
	# (mbContext,iworld)	= editInstance editEvent context iworld
	= case mbContext of
		Error e				= (Error e, iworld)
		Ok context
			# (mbRes,iworld)		= iterateEval editEvent commitEvent genGUI context iworld
			= case mbRes of
				Ok result	= (Ok (result, sessionId), iworld)
				Error e		= (Error e, iworld)

evalSessionInstance :: !SessionId !(Maybe EditEvent) !(Maybe CommitEvent) !Bool !*IWorld -> (!MaybeErrorString (TaskResult Dynamic, !SessionId), !*IWorld)
evalSessionInstance sessionId editEvent commitEvent genGUI iworld
	# (mbContext,iworld)	= loadTaskInstance (Left sessionId) iworld
	= case mbContext of
		Error e				= (Error e, iworld)
		Ok context
			//Apply edit function only once
			# (mbContext, iworld) = editInstance editEvent context iworld
			= case mbContext of
				Error e	
					= (Error e, iworld)
				Ok context			
					# (mbRes,iworld)	= iterateEval editEvent commitEvent genGUI context iworld
					= case mbRes of
						Ok result		= (Ok (result, sessionId), iworld)
						Error e			= (Error e, iworld)
						
iterateEval :: !(Maybe EditEvent) !(Maybe CommitEvent) !Bool !TaskContext !*IWorld -> (!MaybeErrorString (TaskResult Dynamic), !*IWorld)
iterateEval editEvent commitEvent genGUI context iworld = eval editEvent commitEvent genGUI 1 context iworld
where
	eval :: !(Maybe EditEvent) !(Maybe CommitEvent) !Bool !Int !TaskContext !*IWorld -> (!MaybeErrorString (TaskResult Dynamic), !*IWorld)
	eval editEvent commitEvent genGUI iteration context iworld
		//Initialize the toplevel task list
		# iworld 	= {iworld & parallelControls = 'Map'.fromList [(TaskId 0 0,(0,[]))]}
		//Reset read shares list
		# iworld			= {iworld & readShares = Just []} 
		//Evaluate top instance	
		# (mbResult, context, iworld) = evalInstance editEvent commitEvent Nothing genGUI context iworld 
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
							//The edit event is passed to each eval iteration, but the commit event only once
							= eval editEvent Nothing genGUI (iteration + 1) context iworld
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
					# (_,c,iworld)	= evalInstance Nothing Nothing Nothing genGUI c iworld
					# iworld		= storeTaskInstance c iworld
					= processControls` cs {iworld & currentUser = currentUser}
	
	//Extracts controls and resets the toplevel task list
	getControls iworld=:{parallelControls}
		= case 'Map'.get (TaskId 0 0) parallelControls of
			Just (_,controls)	= (controls, {iworld & parallelControls = 'Map'.put (TaskId 0 0) (0,[]) parallelControls})
			_					= ([],iworld)
	
	execControls [] queue iworld = (queue,iworld)
	execControls [c:cs] queue iworld = case c of
		AppendTask tid user (container :: TaskContainer Void)
			//Make thread and properties
			# (thread,managerProperties) = case container of
				(Embedded,tfun)			= (createTaskContainer (tfun TopLevelTaskList),{noMeta & worker = Just user})
				(Detached props,tfun)	= (createTaskContainer (tfun TopLevelTaskList), props)	
			//Make context
			# (context,iworld) = createContext (Right tid) thread managerProperties user iworld			
			= execControls cs (queue ++ [context]) iworld
		//TODO: RemoveTask on the global list
		_
			= execControls cs queue iworld		
		
	issueUser (TaskContext _ _ {ProgressMeta|issuedBy} _ _ _) = issuedBy
		
