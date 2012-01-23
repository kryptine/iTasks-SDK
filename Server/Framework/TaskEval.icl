implementation module TaskEval

import StdList, StdBool
import Error
import SystemTypes, IWorld, Shared, Task, TaskContext
import LayoutCombinators

from CoreCombinators	import :: ParallelTaskType(..), :: ParallelTask(..), :: ParallelResult
from TaskStore			import newSessionId, loadTaskInstance, storeTaskInstance, deleteTaskInstance

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
getTaskMeta (ServiceRep _)		= fixme
where
	fixme = [] //TODO

createTaskContainer :: (ParallelTask a) -> Dynamic | iTask a
createTaskContainer parTask = (dynamic (Container parTask) :: Container (ParallelTask a^) a^)

createValueContainer :: a -> Dynamic | iTask a
createValueContainer val = fixme
where
	fixme = (dynamic (Container val) :: Container a^ a^)	//TOTALLY NOT USED

//Lifts any task to a task that can be added to the "sessions" task list
toSessionTask :: (Task a) -> ParallelTask Void
toSessionTask task=:{evalFun} = \_ -> {task & evalFun = evalFun`}
where
	evalFun` mbE mbC repAs state iworld = case evalFun mbE mbC repAs state iworld of
		(TaskInstable mba rep state,iworld)	= (TaskInstable (fmap (\_ -> Remove) mba) rep state, iworld)
		(TaskStable a rep state, iworld)	= (TaskStable Remove rep state, iworld)
		(TaskException e str, iworld)		= (TaskException e str, iworld)

createTaskState :: !(Either SessionId TopNo) !Dynamic !ManagementMeta !User !*IWorld -> (!TaskContext, !*IWorld)
createTaskState topId container=:(Container parTask :: Container (ParallelTask a) a) mmeta user iworld =:{IWorld|localDateTime}
	# taskId				= case topId of
		Left _		= TaskId 0 0
		Right topNo	= TaskId topNo 0
	# taskList				= taskListShare TopLevelTaskList
	# task					= parTask taskList
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
		TTCRunning container=:(Container parTask :: Container (ParallelTask a) a) scontext
			# taskId					= case topId of
				(Left _)		= TaskId 0 0
				(Right topNo)	= TaskId topNo 0
			# taskList			= taskListShare TopLevelTaskList
			# task				= parTask taskList	
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
		TTCRunning container=:(Container parTask :: Container (ParallelTask a) a) scontext
			# taskList					= taskListShare TopLevelTaskList
			# task						= parTask taskList
			# repAs						= if genGUI (RepAsTUI repTarget task.layout) (RepAsService repTarget)
			//Update current process id & eval stack in iworld
			# taskId					= case topId of
				(Left _)		= TaskId 0 0
				(Right topNo)	= TaskId topNo 0
			# iworld					= {iworld & evalStack = [taskId:evalStack], nextTaskNo = curNextTaskNo} 
			//Apply task's eval function and take updated nextTaskId from iworld
			# (result,iworld)			= task.evalFun editEvent commitEvent repAs scontext iworld
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
	# (state, iworld)		= createTaskState (Left sessionId) (createTaskContainer (toSessionTask task)) noMeta AnyUser iworld
	# (mbState,iworld)		= editInstance editEvent state iworld
	= case mbState of
		Error e				= (Error e, iworld)
		Ok state
			# (mbRes,iworld)	= iterateEval editEvent commitEvent genGUI state iworld
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
		# iworld 	= {iworld & parallelControls = 'Map'.fromList [(toString TopLevelTaskList,(0,[]))]}
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
		# listId = toString TopLevelTaskList
		= case 'Map'.get listId parallelControls of
			Just (_,controls)	= (controls, {iworld & parallelControls = 'Map'.put listId (0,[]) parallelControls})
			_					= ([],iworld)
	
	execControls [] queue iworld = (queue,iworld)
	execControls [c:cs] queue iworld = case c of
		AppendTask tid user parType (parTask :: ParallelTask Void)
			//Make thread and properties
			# (container,managerProperties) = case parType of
				Embedded			= (createTaskContainer parTask, {noMeta & worker = Just user})
				Detached props		= (createTaskContainer parTask, props)	
			//Make context
			# (state,iworld)		= createTaskState (Right tid) container managerProperties user iworld			
			= execControls cs (queue ++ [state]) iworld
		RemoveTask (TaskId tid 0)
			//Remove task instance
			# iworld				= deleteTaskInstance (Right tid) iworld
			= execControls cs queue iworld
		_
			= execControls cs queue iworld		
		
	issueUser (TaskContext _ _ {ProgressMeta|issuedBy} _ _ _) = issuedBy

taskListShare :: !(TaskListId s) -> (SharedTaskList s) | TC s
taskListShare listId = ReadWriteShared [listIdS] read write getVersion
where
	listIdS = toString listId
	
	read iworld=:{parallelStates,parallelLists}
		= case 'Map'.get listIdS parallelStates of
				Just (_,state :: s^)
					= case 'Map'.get listIdS parallelLists of
						Just (_,items)	= (Ok {TaskList|listId = listId, state = state, items = items}, iworld)
						_				= (Error ("Could not read parallel task list of " +++ toString listId), iworld)
				_						= (Error ("Could not read shared parallel state of task list " +++ toString listId),iworld)
	
	write state iworld=:{parallelStates}
		# (mbv,iworld)	= getVersion iworld
		# version		= case mbv of (Ok v) = v ; _ = 0
		= (Ok Void, {iworld & parallelStates = 'Map'.put listIdS (version + 1, (dynamic state :: s^)) parallelStates})
	
	getVersion iworld=:{parallelStates,parallelLists}
			= case ('Map'.get listIdS parallelStates,'Map'.get listIdS parallelLists) of
				(Just (vs,_),Just (vl,_))	= (Ok (vs + vl), iworld)
				_							= (Error ("Could not read timestamp for shared state of task list " +++ listIdS),iworld)
