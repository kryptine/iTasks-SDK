implementation module TaskEval

import StdList, StdBool
import Error
import SystemTypes, IWorld, Shared, Task, TaskContext
import LayoutCombinators

from CoreCombinators	import :: ParallelTaskType(..), :: ParallelTask(..)
from TaskStore			import newSessionId, loadTaskInstance, storeTaskInstance, deleteTaskInstance

from Map				import qualified fromList, get, put
import iTaskClass

ITERATION_THRESHOLD :== 5

setUnstable :: !ProgressMeta -> ProgressMeta
setUnstable meta = {meta & status = Unstable}

setStable :: !ProgressMeta -> ProgressMeta
setStable meta = {meta & status = Stable}

setExcepted :: !ProgressMeta  -> ProgressMeta
setExcepted meta = {meta & status = Excepted}

getTaskMeta	:: !TaskRep -> [(!String,!String)]
getTaskMeta NoRep					= []
getTaskMeta (TUIRep (_,_,_,attr))	= attr
getTaskMeta (ServiceRep (_,_,attr))	= attr

//Tasks are packed in container to have all required overloading resolved for
//completely independent evaluation
createTaskContainer :: (ParallelTask a) -> Dynamic | iTask a
createTaskContainer parTask = (dynamic (Container parTask) :: Container (ParallelTask a^) a^)

//Final task results are wrapped in a container to be able to serialize it in
//a webservice response
createValueContainer :: a -> Dynamic | iTask a
createValueContainer val = (dynamic (Container val) :: Container a^ a^)

//Lifts any task to a task that can be added to the "sessions" task list
toSessionTask :: (Task a) -> ParallelTask Void
toSessionTask task=:{evalFun} = \_ -> {task & evalFun = evalFun`}
where
	evalFun` mbE mbC repAs state iworld = case evalFun mbE mbC repAs state iworld of
		(TaskUnstable mba rep state,iworld)	= (TaskUnstable (fmap (\_ -> Void) mba) rep state, iworld)
		(TaskStable a rep state, iworld)	= (TaskStable Void rep state, iworld)
		(TaskException e str, iworld)		= (TaskException e str, iworld)

createTaskState :: !(Either SessionId TopNo) !Dynamic !ManagementMeta !User !*IWorld -> (!TopInstance, !*IWorld)
createTaskState topId container=:(Container parTask :: Container (ParallelTask a) a) mmeta user iworld =:{IWorld|localDateTime}
	# taskId				= case topId of
		Left _		= TaskId 0 0
		Right topNo	= TaskId topNo 0
	# taskList				= taskListShare TopLevelTaskList
	# task					= parTask taskList
	# (state,iworld) 		= task.initFun taskId {iworld & nextTaskNo = 1, evalStack = [taskId]}		
	# (nextTaskNo,iworld)	= getNextTaskNo iworld 
	# pmeta	 = {issuedAt = localDateTime
			   ,issuedBy = user
			   , status  = Unstable
			   , firstEvent = Nothing
			   , latestEvent = Nothing
			   }
	= ({TopInstance|instanceId=topId,nextTaskNo=nextTaskNo,progress=pmeta,management=mmeta,task=container,state=Left state,attributes=[]},iworld)
where
	getNextTaskNo iworld=:{IWorld|nextTaskNo} = (nextTaskNo,iworld)
	
editInstance :: !(Maybe EditEvent) !TopInstance !*IWorld -> (!MaybeErrorString TopInstance, !*IWorld)
editInstance editEvent topInstance=:{TopInstance|instanceId,task=(Container parTask :: Container (ParallelTask a) a),state=Left taskState} iworld=:{evalStack}
	# taskId				= case instanceId of
		(Left _)		= TaskId 0 0
		(Right topNo)	= TaskId topNo 0
	# taskList				= taskListShare TopLevelTaskList
	# task					= parTask taskList	
	# (taskState,iworld)	= case editEvent of
		Just event		= task.editFun event taskState {iworld & evalStack = [taskId:evalStack]}
		_				= (taskState,iworld)
	# iworld				= {iworld & evalStack = evalStack}
	= (Ok {TopInstance|topInstance & state = Left taskState}, iworld)
editInstance editEvent topInstance iworld
	= (Ok topInstance, iworld)

//Evaluate the given context and yield the result of the main task indicated by target
evalInstance :: !(Maybe EditEvent) !(Maybe CommitEvent) !(Maybe TaskId) !Bool !TopInstance  !*IWorld -> (!MaybeErrorString (TaskResult Dynamic), !TopInstance, !*IWorld)
evalInstance editEvent commitEvent repTarget genGUI topInstance=:{TopInstance|instanceId,nextTaskNo=curNextTaskNo,progress,task=(Container parTask :: Container (ParallelTask a) a),state=Left taskState} iworld=:{nextTaskNo,evalStack}
	//Eval instance
	# taskList					= taskListShare TopLevelTaskList
	# task						= parTask taskList
	# repAs						= if genGUI (RepAsTUI repTarget task.layout) (RepAsService repTarget)
	//Update current process id & eval stack in iworld
	# taskId					= case instanceId of
		(Left _)		= TaskId 0 0
		(Right topNo)	= TaskId topNo 0
	# iworld					= {iworld & evalStack = [taskId:evalStack], nextTaskNo = curNextTaskNo} 
	//Apply task's eval function and take updated nextTaskId from iworld
	# (result,iworld)			= task.evalFun editEvent commitEvent repAs taskState iworld
	# (updNextTaskNo,iworld)	= getNextTaskNo iworld
	//Restore current process id & nextTask id in iworld
	# iworld			= {iworld & nextTaskNo = nextTaskNo, evalStack = evalStack}
	= case result of
		TaskUnstable _ rep state
			# topInstance = {TopInstance|topInstance & nextTaskNo = updNextTaskNo, progress = setUnstable progress, state = Left state}
			= (Ok (TaskUnstable Nothing rep state), topInstance , iworld)
		TaskStable val rep state
			# topInstance = {TopInstance|topInstance & nextTaskNo = updNextTaskNo, progress = setStable progress, state = Left state}
			= (Ok (TaskStable (createValueContainer val) rep state), topInstance , iworld)
		TaskException e str
			# topInstance = {TopInstance|topInstance & nextTaskNo = updNextTaskNo, progress = setExcepted progress, state = Right str}
			= (Ok (TaskException e str), topInstance, iworld)
where
	getNextTaskNo iworld=:{IWorld|nextTaskNo} = (nextTaskNo,iworld)
evalInstance _ _ _ _ topInstance=:{TopInstance|state=Right e} iworld
	= (Ok (taskException e), topInstance, iworld)
evalInstance _ _ _ _ topInstance iworld	
	= (Ok (taskException "Could not unpack instance state"), topInstance, iworld)


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

evalSessionInstance :: !SessionId !(Maybe EditEvent) !(Maybe CommitEvent) !Bool !*IWorld -> (!MaybeErrorString (!TaskResult Dynamic, !SessionId), !*IWorld)
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
						
iterateEval :: !(Maybe EditEvent) !(Maybe CommitEvent) !Bool !TopInstance !*IWorld -> (!MaybeErrorString (TaskResult Dynamic), !*IWorld)
iterateEval editEvent commitEvent genGUI context iworld = eval editEvent commitEvent genGUI 1 context iworld
where
	eval :: !(Maybe EditEvent) !(Maybe CommitEvent) !Bool !Int !TopInstance !*IWorld -> (!MaybeErrorString (TaskResult Dynamic), !*IWorld)
	eval editEvent commitEvent genGUI iteration context iworld
		//Initialize the toplevel task list
		# iworld 	= {iworld & parallelControls = 'Map'.fromList [("taskList:" +++ toString TopLevelTaskList,(0,[]))]}
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
					(TaskUnstable _ _ _)
						| isNothing readShares && iteration < ITERATION_THRESHOLD
							//Always store the instance betweeen iterations, else the index is not updated and you
							//get to see an incorrect view on the work list
							# iworld	= storeTaskInstance context iworld
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
		# listId = "taskList:" +++ toString TopLevelTaskList
		= case 'Map'.get listId parallelControls of
			Just (_,controls)	= (controls, {iworld & parallelControls = 'Map'.put listId (0,[]) parallelControls})
			_					= ([],iworld)
	
	execControls [] queue iworld = (queue,iworld)
	execControls [c:cs] queue iworld = case c of
		AppendTask {ParallelItem|taskId=TaskId tid 0,task=(parTask :: ParallelTask Void), management}
			# container			= createTaskContainer parTask
			# management		= fromMaybe noMeta management
			# user				= fromMaybe AnyUser management.worker
			# (state,iworld)	= createTaskState (Right tid) container management AnyUser iworld	
			= execControls cs (queue ++ [state]) iworld
		RemoveTask (TaskId tid 0)
			//Remove task instance
			# iworld				= deleteTaskInstance (Right tid) iworld
			= execControls cs queue iworld
		_
			= fixme where fixme = execControls cs queue iworld		
		
	issueUser {TopInstance|progress={ProgressMeta|issuedBy}} = issuedBy

taskListShare :: !(TaskListId s) -> (SharedTaskList s) | iTask s
taskListShare listId = (makeReadOnlySharedError "taskList" listKey read getVersion)
where
	listKey = toString listId
	
	read iworld=:{parallelLists}
		= case 'Map'.get ("taskList:" +++ listKey) parallelLists of
			Just (_,items)	= (Ok (mkTaskList items), iworld)
			_				= (Error ("Could not read parallel task list of " +++ toString listId), iworld)
		
	
	getVersion iworld=:{parallelLists}
			= case ('Map'.get ("taskList:" +++ listKey) parallelLists) of
				(Just (v,_))	= (Ok v, iworld)
				_				= (Error ("Could not read timestamp for shared state of task list " +++ listKey),iworld)

	mkTaskList items = {TaskList|listId = listId, state = state, items = litems}
	where
		state	= [fromJSON lastValue \\ {ParallelItem|lastValue} <- items]
		litems	= map toMeta items
		
		toMeta {ParallelItem|taskId,progress,management,state,lastAttributes}
			= {TaskListItem|taskId = taskId, taskMeta = lastAttributes, progressMeta = progress, managementMeta = management
			  ,subItems = stateToTaskListItems state}
	
		
		