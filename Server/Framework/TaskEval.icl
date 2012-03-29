implementation module TaskEval

import StdList, StdBool
import Error
import SystemTypes, IWorld, Shared, Task, TaskState
import LayoutCombinators

from CoreCombinators	import :: ParallelTaskType(..), :: ParallelTask(..)
from TaskStore			import newSessionId, newInstanceId, loadTaskInstance, loadSessionInstance, storeTaskInstance, deleteTaskInstance

from Map				import qualified fromList, toList, get, put
import iTaskClass

setStatus :: !Stability !ProgressMeta -> ProgressMeta
setStatus status meta = {meta & status = status}

getTaskMeta	:: !TaskRep -> [(!String,!String)]
getTaskMeta (TaskRep (_,_,_,attr) _)	= attr

//Tasks are packed in container to have all required overloading resolved for
//completely independent evaluation
createTaskContainer :: (ParallelTask a) -> Dynamic | iTask a
createTaskContainer parTask = (dynamic (Container parTask) :: Container (ParallelTask a^) a^)

//Final task results are wrapped in a container to be able to serialize it in
//a webservice response
createValueContainer :: a -> Dynamic | iTask a
createValueContainer val = (dynamic (Container val) :: Container a^ a^)

createTaskInstance :: !InstanceNo !(Maybe SessionId) !Dynamic !ManagementMeta !ProgressMeta !*IWorld -> (!TaskInstance, !*IWorld)
createTaskInstance instanceNo sessionId container=:(Container parTask :: Container (ParallelTask a) a) mmeta pmeta iworld=:{taskTime} 
	= ({TaskInstance|instanceNo=instanceNo,sessionId=sessionId,nextTaskNo=1,nextTaskTime=2,progress=pmeta,management=mmeta,task=container
	   ,result = ValueResult NoValue taskTime (TaskRep (SingleTask,Just (stringDisplay "This task has not been evaluated yet." ),[],[]) []) (TCInit (TaskId instanceNo 0) 1)
	   ,shares = [], lists = []},iworld)

//Evaluate the given context and yield the result of the main task indicated by target
evalInstance :: !(Maybe EditEvent) !(Maybe CommitEvent) !RefreshFlag !(Maybe TaskId) !TaskInstance  !*IWorld -> (!MaybeErrorString (TaskResult Dynamic), !TaskInstance, !*IWorld)
evalInstance editEvent commitEvent refresh repTarget topInstance=:{TaskInstance|instanceNo,nextTaskNo=curNextTaskNo,nextTaskTime,progress,task=(Container parTask :: Container (ParallelTask a) a),result=ValueResult _ _ _ tree,shares} iworld=:{nextTaskNo,taskTime,evalStack,localShares}
	//Eval instance
	# taskList				= taskListShare TopLevelTaskList
	# (Task eval)			= parTask taskList
	# repAs					= TaskRepTarget repTarget Nothing Nothing
	//Update current process id & eval stack in iworld
	# taskId					= TaskId instanceNo 0
	# curLocalShares			= 'Map'.fromList [(TaskId instanceNo taskNo,val) \\ (taskNo,val) <- shares] 
	# iworld					= {iworld & evalStack = [taskId:evalStack], nextTaskNo = curNextTaskNo, taskTime = nextTaskTime, localShares = curLocalShares} 
	//Apply task's eval function and take updated nextTaskId from iworld
	# (result,iworld)			= eval editEvent commitEvent refresh repAs tree iworld

	# (updNextTaskNo,iworld)	= getNextTaskNo iworld
	# (shares,iworld)			= getLocalShares iworld
	//Restore current process id, nextTask id and local shares in iworld
	# iworld					= {iworld & nextTaskNo = nextTaskNo, taskTime = taskTime, evalStack = evalStack, localShares = localShares}
	= case result of
		ValueResult NoValue lastEvent rep tree
			# topInstance = {TaskInstance|topInstance & nextTaskNo = updNextTaskNo, nextTaskTime = nextTaskTime + 1, progress = setStatus Unstable progress, result = ValueResult NoValue lastEvent rep tree, shares = shares}
			= (Ok (ValueResult NoValue lastEvent rep tree), topInstance, iworld)
		ValueResult (Value val stable) lastEvent rep tree
			# topInstance = {TaskInstance|topInstance & nextTaskNo = updNextTaskNo, nextTaskTime = nextTaskTime + 1, progress = setStatus stable progress, result = ValueResult (Value (toJSON val) stable) lastEvent rep tree, shares = shares}
			= (Ok (ValueResult (Value (createValueContainer val) stable) lastEvent rep tree), topInstance, iworld)
		ExceptionResult e str
			# topInstance = {TaskInstance|topInstance & nextTaskNo = updNextTaskNo, nextTaskTime = nextTaskTime + 1, progress = setStatus Unstable progress, result = ExceptionResult e str, shares = shares}
			= (Ok (ExceptionResult e str), topInstance, iworld)
where
	getNextTaskNo iworld=:{IWorld|nextTaskNo} = (nextTaskNo,iworld)
	getLocalShares iworld=:{IWorld|localShares} = ([(taskNo,val) \\ (TaskId _ taskNo,val) <- 'Map'.toList localShares],iworld)
	
evalInstance _ _ _ _ topInstance=:{TaskInstance|result=ExceptionResult e msg} iworld
	= (Ok (ExceptionResult e msg), topInstance, iworld)
evalInstance _ _ _ _ topInstance iworld	
	= (Ok (exception "Could not unpack instance state"), topInstance, iworld)

createSessionInstance :: !(Task a) !(Maybe EditEvent) !(Maybe CommitEvent) !*IWorld -> (!MaybeErrorString (!TaskResult Dynamic, !SessionId), !*IWorld) |  iTask a
createSessionInstance task editEvent commitEvent iworld=:{currentDateTime}
	# (sessionId,iworld)	= newSessionId iworld
	# (instanceId,iworld)	= newInstanceId iworld
	# (state, iworld)		= createTaskInstance instanceId (Just sessionId) (createTaskContainer (\_ -> task)) noMeta {issuedAt=currentDateTime,issuedBy=AnonymousUser sessionId,status=Unstable,firstEvent=Nothing,latestEvent=Nothing} iworld
	# (mbRes,iworld)		= evalTopInstance editEvent commitEvent state iworld
	= case mbRes of
		Ok result	= (Ok (result, sessionId), iworld)
		Error e		= (Error e, iworld)

evalSessionInstance :: !SessionId !(Maybe EditEvent) !(Maybe CommitEvent) !*IWorld -> (!MaybeErrorString (!TaskResult Dynamic, !SessionId), !*IWorld)
evalSessionInstance sessionId editEvent commitEvent iworld
	# (mbInstance,iworld)	= loadSessionInstance sessionId iworld
	= case mbInstance of
		Error e				= (Error e, iworld)
		Ok inst
			# (mbRes,iworld)	= evalTopInstance editEvent commitEvent inst {iworld & currentUser = AnonymousUser sessionId}
			= case mbRes of
				Ok result		= (Ok (result, sessionId), iworld)
				Error e			= (Error e, iworld)

evalTopInstance :: !(Maybe EditEvent) !(Maybe CommitEvent) !TaskInstance !*IWorld -> (!MaybeErrorString (TaskResult Dynamic), !*IWorld)
evalTopInstance editEvent commitEvent state iworld
	//Initialize the toplevel task list
	# iworld 	= {iworld & parallelControls = 'Map'.fromList [("taskList:" +++ toString TopLevelTaskList,(0,[]))]}
	
	//If an event was present evaluate an extra time without events for a consistent picture
	# (mbResult, state, iworld) = case (editEvent,commitEvent) of
		(Nothing,Nothing)	= evalInstance editEvent commitEvent False Nothing state iworld 
		_					
			//Evaluate twice: The second time set the refresh flag
			# (_,state,iworld)	= evalInstance editEvent commitEvent False Nothing state iworld 
			//Save the intermediate state and process controls (or you won't see completed/added/removed processes)
			# iworld			= storeTaskInstance state iworld		
			# iworld			= processControls iworld
			= evalInstance editEvent commitEvent True Nothing state iworld
	//Store context & process controls
	# iworld	= storeTaskInstance state iworld		
	# iworld	= processControls iworld 
	= (mbResult, iworld)
		
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
				# (_,c,iworld)	= evalInstance Nothing Nothing False Nothing c iworld
				# iworld		= storeTaskInstance c iworld
				= processControls` cs {iworld & currentUser = currentUser}

	//Extracts controls and resets the toplevel task list
	getControls iworld=:{parallelControls}
		# listId = "taskList:" +++ toString TopLevelTaskList
		= case 'Map'.get listId parallelControls of
			Just (_,controls)	= (controls, {iworld & parallelControls = 'Map'.put listId (0,[]) parallelControls})
			_					= ([],iworld)

	execControls [] queue iworld = (queue,iworld)
	execControls [c:cs] queue iworld=:{currentDateTime,currentUser} = case c of
		AppendTask {ParallelItem|taskId=TaskId tid 0,task=(parTask :: ParallelTask Void), management, progress}
			# container			= createTaskContainer parTask
			# management		= fromMaybe noMeta management
			# progress			= fromMaybe {issuedAt=currentDateTime,issuedBy=currentUser,status=Unstable,firstEvent=Nothing,latestEvent=Nothing} progress
			# (state,iworld)	= createTaskInstance tid Nothing container management progress iworld	
			= execControls cs (queue ++ [state]) iworld
		RemoveTask (TaskId tid 0)
			//Remove task instance
			# iworld				= deleteTaskInstance tid iworld
			= execControls cs queue iworld
		_
			= execControls cs queue iworld		
		
	issueUser {TaskInstance|progress={ProgressMeta|issuedBy}} = issuedBy

taskListShare :: !(TaskListId s) -> (SharedTaskList s) | iTask s
taskListShare listId = (makeReadOnlySharedError "taskList" listKey read)
where
	listKey = toString listId
	
	read iworld=:{parallelLists}
		= case 'Map'.get ("taskList:" +++ listKey) parallelLists of
			Just items	= (Ok (mkTaskList items), iworld)
	
	mkTaskList items = {TaskList|listId = listId, state = state, items = litems}
	where
		state	= [fromJust (fromJSON lastValue) \\ {ParallelItem|lastValue} <- items]
		litems	= map toMeta items
		
		toMeta {ParallelItem|taskId,progress,management,state,lastAttributes}
			= {TaskListItem|taskId = taskId, taskMeta = lastAttributes, progressMeta = progress, managementMeta = management
			  ,subItems = stateToTaskListItems state}
		