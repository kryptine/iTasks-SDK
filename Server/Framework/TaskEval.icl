implementation module TaskEval

import StdList, StdBool
import Error
import SystemTypes, IWorld, Shared, Task, TaskState
import LayoutCombinators

from CoreCombinators	import :: ParallelTaskType(..), :: ParallelTask(..)
from TaskStore			import newSessionId, loadTaskInstance, storeTaskInstance, deleteTaskInstance

from Map				import qualified fromList, get, put
import iTaskClass

setStatus :: !Stability !ProgressMeta -> ProgressMeta
setStatus status meta = {meta & status = status}

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

createTaskState :: !(Either SessionId TopNo) !Dynamic !ManagementMeta !User !*IWorld -> (!TopInstance, !*IWorld)
createTaskState topId container=:(Container parTask :: Container (ParallelTask a) a) mmeta user iworld =:{IWorld|currentDateTime}
	# taskId	= case topId of
		Left _		= TaskId 0 0
		Right topNo	= TaskId topNo 0
	# pmeta		= { issuedAt = currentDateTime
				  , issuedBy = user
				  , status  = Unstable
				  , firstEvent = Nothing
				  , latestEvent = Nothing
				  }
	= ({TopInstance|instanceId=topId,nextTaskNo=1,nextTaskTime=2,progress=pmeta,management=mmeta,task=container,state=Left (TCInit taskId 1),attributes=[]},iworld)
where
	getNextTaskNo iworld=:{IWorld|nextTaskNo} = (nextTaskNo,iworld)

//Evaluate the given context and yield the result of the main task indicated by target
evalInstance :: !(Maybe EditEvent) !(Maybe CommitEvent) !(Maybe TaskId) !Bool !TopInstance  !*IWorld -> (!MaybeErrorString (TaskResult Dynamic), !TopInstance, !*IWorld)
evalInstance editEvent commitEvent repTarget genGUI topInstance=:{TopInstance|instanceId,nextTaskNo=curNextTaskNo,nextTaskTime,progress,task=(Container parTask :: Container (ParallelTask a) a),state=Left taskState} iworld=:{nextTaskNo,taskTime,evalStack}
	//Eval instance
	# taskList					= taskListShare TopLevelTaskList
	# task						= parTask taskList
	# repAs						= if genGUI (RepAsTUI repTarget task.layout) (RepAsService repTarget)
	//Update current process id & eval stack in iworld
	# taskId					= case instanceId of
		(Left _)		= TaskId 0 0
		(Right topNo)	= TaskId topNo 0
	# iworld					= {iworld & evalStack = [taskId:evalStack], nextTaskNo = curNextTaskNo, taskTime = nextTaskTime} 
	//Apply task's eval function and take updated nextTaskId from iworld
	# (result,iworld)			= task.eval editEvent commitEvent repAs taskState iworld

	# (updNextTaskNo,iworld)	= getNextTaskNo iworld
	//Restore current process id & nextTask id in iworld
	# iworld					= {iworld & nextTaskNo = nextTaskNo, taskTime = taskTime, evalStack = evalStack}
	= case result of
		ValueResult NoValue lastEvent rep state
			# topInstance = {TopInstance|topInstance & nextTaskNo = updNextTaskNo, nextTaskTime = nextTaskTime + 1, progress = setStatus Unstable progress, state = Left state}
			= (Ok (ValueResult NoValue lastEvent rep state), topInstance, iworld)
		ValueResult (Value val stable) lastEvent rep state
			# topInstance = {TopInstance|topInstance & nextTaskNo = updNextTaskNo, nextTaskTime = nextTaskTime + 1, progress = setStatus stable progress, state = Left state}
			= (Ok (ValueResult (Value (createValueContainer val) stable) lastEvent rep state), topInstance, iworld)
		ExceptionResult e str
			# topInstance = {TopInstance|topInstance & nextTaskNo = updNextTaskNo, nextTaskTime = nextTaskTime + 1, progress = setStatus Unstable progress, state = Right str}
			= (Ok (ExceptionResult e str), topInstance, iworld)
where
	getNextTaskNo iworld=:{IWorld|nextTaskNo} = (nextTaskNo,iworld)
evalInstance _ _ _ _ topInstance=:{TopInstance|state=Right e} iworld
	= (Ok (exception e), topInstance, iworld)
evalInstance _ _ _ _ topInstance iworld	
	= (Ok (exception "Could not unpack instance state"), topInstance, iworld)

createSessionInstance :: !(Task a) !(Maybe EditEvent) !(Maybe CommitEvent) !Bool !*IWorld -> (!MaybeErrorString (!TaskResult Dynamic, !SessionId), !*IWorld) |  iTask a
createSessionInstance task editEvent commitEvent genGUI iworld
	# (sessionId,iworld)	= newSessionId iworld
	# (state, iworld)		= createTaskState (Left sessionId) (createTaskContainer (\_ -> task)) noMeta AnyUser iworld
	# (mbRes,iworld)		= evalTopInstance editEvent commitEvent genGUI state iworld
	= case mbRes of
		Ok result	= (Ok (result, sessionId), iworld)
		Error e		= (Error e, iworld)

evalSessionInstance :: !SessionId !(Maybe EditEvent) !(Maybe CommitEvent) !Bool !*IWorld -> (!MaybeErrorString (!TaskResult Dynamic, !SessionId), !*IWorld)
evalSessionInstance sessionId editEvent commitEvent genGUI iworld
	# (mbContext,iworld)	= loadTaskInstance (Left sessionId) iworld
	= case mbContext of
		Error e				= (Error e, iworld)
		Ok context
			# (mbRes,iworld)	= evalTopInstance editEvent commitEvent genGUI context iworld
			= case mbRes of
				Ok result		= (Ok (result, sessionId), iworld)
				Error e			= (Error e, iworld)

evalTopInstance :: !(Maybe EditEvent) !(Maybe CommitEvent) !Bool !TopInstance !*IWorld -> (!MaybeErrorString (TaskResult Dynamic), !*IWorld)
evalTopInstance editEvent commitEvent genGUI state iworld
	//Initialize the toplevel task list
	# iworld 	= {iworld & parallelControls = 'Map'.fromList [("taskList:" +++ toString TopLevelTaskList,(0,[]))]}
	
	//If an event was present evaluate an extra time without events for a consistent picture
	# (mbResult, state, iworld) = case (editEvent,commitEvent) of
		(Nothing,Nothing)	= evalInstance editEvent commitEvent Nothing genGUI state iworld 
		_					
			//Evaluate twice: First with events, but without generating a GUI, then once without events
			# (_,state,iworld)	= evalInstance editEvent commitEvent Nothing False state iworld 
			//Save the intermediate state and process controls (or you won't see completed/added/removed processes)
			# iworld			= storeTaskInstance state iworld		
			# iworld			= processControls iworld
			= evalInstance Nothing Nothing Nothing genGUI state iworld
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
				# (_,c,iworld)	= evalInstance Nothing Nothing Nothing False c iworld
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
			= execControls cs queue iworld		
		
	issueUser {TopInstance|progress={ProgressMeta|issuedBy}} = issuedBy

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
		