implementation module TaskEval

import StdList, StdBool
import Error
import SystemTypes, IWorld, Shared, Task, TaskState, TaskStore
import LayoutCombinators

from CoreCombinators	import :: ParallelTaskType(..), :: ParallelTask(..)

from Map				import qualified fromList, toList, get, put
import iTaskClass

setStatus :: !Stability !ProgressMeta -> ProgressMeta
setStatus status meta = {meta & status = status}

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
	= ({TaskInstance|instanceNo=instanceNo,sessionId=sessionId,parent=0,nextTaskNo=1,nextTaskTime=2,progress=pmeta,management=mmeta,task=container
	   ,result = ValueResult NoValue taskTime (TaskRep (SingleTask,Just (stringDisplay "This task has not been evaluated yet." ),[],[]) []) (TCInit (TaskId instanceNo 0) 1)
	   ,shares = [], lists = [], observers = []}, iworld)

createSessionInstance :: !(Task a) !(Maybe EditEvent) !(Maybe CommitEvent) !*IWorld -> (!MaybeErrorString (!TaskResult Dynamic, !SessionId), !*IWorld) |  iTask a
createSessionInstance task editEvent commitEvent iworld=:{currentDateTime}
	# (sessionId,iworld)	= newSessionId iworld
	# (instanceId,iworld)	= newInstanceId iworld
	# (inst, iworld)		= createTaskInstance instanceId (Just sessionId) (createTaskContainer (\_ -> task)) noMeta {issuedAt=currentDateTime,issuedBy=AnonymousUser sessionId,status=Unstable,firstEvent=Nothing,latestEvent=Nothing} iworld
	# (mbRes,inst,iworld)	= evalInstance editEvent commitEvent False inst iworld
	# iworld				= storeTaskInstance inst iworld
	= case mbRes of
		Ok result	= (Ok (result, sessionId), iworld)
		Error e		= (Error e, iworld)

evalSessionInstance :: !SessionId !(Maybe EditEvent) !(Maybe CommitEvent) !*IWorld -> (!MaybeErrorString (!TaskResult Dynamic, !SessionId), !*IWorld)
evalSessionInstance sessionId eEvent cEvent iworld
	//Set session user
	# iworld				= {iworld & currentUser = AnonymousUser sessionId}
	//If there is an event, evaluate the instance at which the event is targeted first
	# iworld				= processEvent eEvent cEvent iworld
	//Evaluate session instance
	# (mbInstance,iworld)	= loadSessionInstance sessionId iworld
	= case mbInstance of
		Error e				= (Error e, iworld)
		Ok inst
			# (mbRes,inst,iworld)	= evalInstance eEvent cEvent True inst iworld
			# iworld				= storeTaskInstance inst iworld
			# iworld				= remOutdatedInstance inst.TaskInstance.instanceNo iworld
			= case mbRes of
				Ok result		= (Ok (result, sessionId), iworld)
				Error e			= (Error e, iworld)

processEvent :: !(Maybe EditEvent) !(Maybe CommitEvent) !*IWorld -> *IWorld
processEvent Nothing Nothing iworld
	= iworld
processEvent eEvent cEvent iworld
	= case loadTaskInstance (instanceNo eEvent cEvent) iworld of
		(Error _,iworld)	= iworld
		(Ok inst,iworld)
			//Eval the targeted instance first
			# (_,inst,iworld)	= evalInstance eEvent cEvent False inst iworld
			# iworld			= storeTaskInstance inst iworld
			# iworld			= remOutdatedInstance inst.TaskInstance.instanceNo iworld
			//Eval another time with refresh flag to make sure shares are all up-to-date (THIS SHOULD BE OPTIMIZED)
			# (_,inst,iworld)	= evalInstance eEvent cEvent True inst iworld
			# iworld			= storeTaskInstance inst iworld
			# iworld			= remOutdatedInstance inst.TaskInstance.instanceNo iworld
			//Refresh affected tasks
			# iworld			= refreshOutdatedInstances iworld 
			= iworld
where
	instanceNo (Just (TaskEvent (TaskId no _) _)) _ = no
	instanceNo _ (Just (TaskEvent (TaskId no _) _)) = no
	
createPersistentInstance :: !(Task a) !ManagementMeta !User !*IWorld -> (!TaskId, !*IWorld) | iTask a
createPersistentInstance task meta issuer iworld=:{currentDateTime}
	# (instanceId,iworld)	= newInstanceId iworld
	# (state, iworld)		= createTaskInstance instanceId Nothing (createTaskContainer (\_ -> task)) meta {issuedAt=currentDateTime,issuedBy=issuer,status=Unstable,firstEvent=Nothing,latestEvent=Nothing} iworld
	# iworld				= storeTaskInstance state iworld		
	= (TaskId instanceId 0, iworld)

//Evaluate a single task instance
evalInstance :: !(Maybe EditEvent) !(Maybe CommitEvent) !RefreshFlag !TaskInstance  !*IWorld -> (!MaybeErrorString (TaskResult Dynamic), !TaskInstance, !*IWorld)
evalInstance editEvent commitEvent refresh inst=:{TaskInstance|instanceNo,nextTaskNo=curNextTaskNo,nextTaskTime,progress,task=(Container parTask :: Container (ParallelTask a) a),result=ValueResult _ _ _ tree,shares,lists} iworld=:{currentInstance,nextTaskNo,taskTime,localShares,localLists}
	//Eval instance
	# (Task eval)				= parTask topListShare
	# repAs						= TaskRepOpts Nothing Nothing
	//Update current process id & eval stack in iworld
	# taskId					= TaskId instanceNo 0
	# curLocalShares			= 'Map'.fromList [(TaskId instanceNo taskNo,val) \\ (taskNo,val) <- shares] 
	# curLocalLists				= 'Map'.fromList lists
	# iworld					= {iworld & currentInstance = instanceNo, nextTaskNo = curNextTaskNo, taskTime = nextTaskTime, localShares = curLocalShares, localLists = curLocalLists} 
	//Apply task's eval function and take updated nextTaskId from iworld
	# (result,iworld)			= eval editEvent commitEvent refresh repAs tree iworld
	# (updNextTaskNo,iworld)	= getNextTaskNo iworld
	# (shares,iworld)			= getLocalShares iworld
	# (lists,iworld)			= getLocalLists iworld
	//Restore current process id, nextTask id and local shares in iworld
	# iworld					= {iworld & currentInstance = currentInstance, nextTaskNo = nextTaskNo, taskTime = taskTime, localShares = localShares, localLists = localLists}
	= case result of
		ValueResult NoValue lastEvent rep tree
			# inst = {TaskInstance|inst & nextTaskNo = updNextTaskNo, nextTaskTime = nextTaskTime + 1, progress = setStatus Unstable progress, result = ValueResult NoValue lastEvent rep tree, shares = shares, lists = lists}
			= (Ok (ValueResult NoValue lastEvent rep tree), inst, iworld)
		ValueResult (Value val stable) lastEvent rep tree
			# inst = {TaskInstance|inst & nextTaskNo = updNextTaskNo, nextTaskTime = nextTaskTime + 1, progress = setStatus stable progress, result = ValueResult (Value (toJSON val) stable) lastEvent rep tree, shares = shares, lists = lists}
			= (Ok (ValueResult (Value (createValueContainer val) stable) lastEvent rep tree), inst, iworld)
		ExceptionResult e str
			# inst = {TaskInstance|inst & nextTaskNo = updNextTaskNo, nextTaskTime = nextTaskTime + 1, progress = setStatus Unstable progress, result = ExceptionResult e str, shares = shares, lists = lists}
			= (Ok (ExceptionResult e str), inst, iworld)
where
	getNextTaskNo iworld=:{IWorld|nextTaskNo} = (nextTaskNo,iworld)
	getLocalShares iworld=:{IWorld|localShares} = ([(taskNo,val) \\ (TaskId _ taskNo,val) <- 'Map'.toList localShares],iworld)
	getLocalLists iworld=:{IWorld|localLists} = ('Map'.toList localLists,iworld)

evalInstance _ _ _ inst=:{TaskInstance|result=ExceptionResult e msg} iworld
	= (Ok (ExceptionResult e msg), inst, iworld)
evalInstance _ _ _ inst iworld	
	= (Ok (exception "Could not unpack instance state"), inst, iworld)

//Evaluate tasks marked as outdated in the task pool
refreshOutdatedInstances :: !*IWorld -> *IWorld
refreshOutdatedInstances iworld = refresh [] iworld
where
	refresh done iworld = case nextOutdatedInstance iworld of
		(Nothing,iworld)	= iworld
		(Just next,iworld)
			| isMember next done	= iworld
									= refresh [next:done] (refreshInstance next iworld)
	
//Evaluate a task instance without any events
refreshInstance :: !InstanceNo !*IWorld -> *IWorld
refreshInstance instanceNo iworld
	= case loadTaskInstance instanceNo iworld of
		(Error _,iworld)	= iworld
		(Ok inst,iworld)
			# (_,inst,iworld)	= evalInstance Nothing Nothing False inst iworld
			# iworld			= storeTaskInstance inst iworld
			# iworld			= remOutdatedInstance inst.TaskInstance.instanceNo iworld
			= iworld

localShare :: !TaskId -> Shared a | iTask a
localShare taskId = makeUnsafeShare "localShare" shareKey read write
where
	shareKey = toString taskId
	read iworld=:{localShares}
		= case 'Map'.get taskId localShares of
			Just encs	
				= case fromJSON encs of
					Just s	= (Ok s, iworld)
					_		= (Error ("Could not decode local shared state " +++ shareKey), iworld)
			_			= (Error ("Could not read local shared state " +++ shareKey), iworld)
	
	write value iworld=:{localShares}
		= (Ok Void, {iworld & localShares = 'Map'.put taskId (toJSON value) localShares})

//Top list share has no items, and is therefore completely polymorphic
topListShare :: SharedTaskList a
topListShare = makeReadOnlySharedError "globalList" "top" read
where
	read iworld
		= (Ok {listId = TopLevelTaskList, items = []}, iworld)
		
parListShare :: !TaskId -> SharedTaskList a | iTask a
parListShare taskId=:(TaskId instanceNo taskNo) = makeReadOnlySharedError "localList" shareKey read
where
	shareKey = toString taskId
	read iworld=:{localLists}
		= case 'Map'.get taskId localLists of
			Just entries
				= (Ok {listId = ParallelTaskList taskId, items = map toItem entries},iworld)
			_			= (Error ("Could not read local shared state " +++ shareKey), iworld)

	toItem {TaskListEntry|entryId,state,result=ValueResult val ts (TaskRep (_,_,_,attr) _) _}
		= 	{taskId			= entryId
			,value			= deserialize val
			,taskMeta		= attr
			,managementMeta = management
			,progressMeta	= progress
			}
	where
		(progress,management) = case state of
			DetachedState _ p m = (Just p,Just m)
			_					= (Nothing,Nothing)
	
	deserialize NoValue	= NoValue
	deserialize (Value json stable) = maybe NoValue (\v -> Value v stable) (fromJSON json)

