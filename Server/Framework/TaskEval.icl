implementation module TaskEval

import StdList, StdBool, StdTuple
import Error
import SystemTypes, IWorld, Shared, Task, TaskState, TaskStore, Util, Func
import LayoutCombinators

from CoreCombinators	import :: ParallelTaskType(..), :: ParallelTask(..)
from Map				import qualified newMap, fromList, toList, get, put
from SharedDataSource	import qualified read, write, writeFilterMsg
from IWorld				import dequeueWorkFilter
import iTaskClass

createSessionTaskInstance :: !(Task a) !Event !*IWorld -> (!MaybeErrorString (!TaskResult JSONNode, !InstanceNo, !SessionId), !*IWorld) |  iTask a
createSessionTaskInstance task event iworld=:{currentDateTime,taskTime}
	# (sessionId,iworld)	= newSessionId iworld
	# (instanceNo,iworld)	= newInstanceNo iworld
	# worker				= AnonymousUser sessionId
	//Create the initial instance data in the store
	# mmeta					= defaultValue
	# pmeta					= {issuedAt=currentDateTime,issuedBy=worker,stable=False,firstEvent=Nothing,latestEvent=Nothing,latestAttributes='Map'.newMap}
	# meta					= createMeta instanceNo (Just sessionId) 0 (Just worker) mmeta pmeta
	# (_,iworld)			= 'SharedDataSource'.write meta (taskInstanceMeta instanceNo) iworld
	# (_,iworld)			= 'SharedDataSource'.write (createReduct instanceNo task taskTime) (taskInstanceReduct instanceNo) iworld
	# (_,iworld)			= 'SharedDataSource'.write (createResult taskTime) (taskInstanceResult instanceNo) iworld
	# (_,iworld)			= 'SharedDataSource'.write (createRep) (taskInstanceRep instanceNo) iworld
	//Register the sessionId -> instanceNo relation
	# iworld				= registerSession sessionId instanceNo iworld
	//Evaluate once
	# (mbResult,iworld)		= evalTaskInstance RefreshEvent instanceNo iworld
	= case mbResult of
		Ok result	= (Ok (result,instanceNo,sessionId),iworld)
		Error e		= (Error e, iworld)
where
	registerSession sessionId instanceNo iworld=:{IWorld|sessions}
		= {IWorld|iworld & sessions = 'Map'.put sessionId instanceNo sessions}

createTopTaskInstance  :: !(Task a) !ManagementMeta !User !InstanceNo !*IWorld -> (!TaskId, !*IWorld) | iTask a
createTopTaskInstance  task mmeta issuer parent iworld=:{currentDateTime,taskTime}
	# (instanceNo,iworld)	= newInstanceNo iworld
	# pmeta					= {issuedAt=currentDateTime,issuedBy=issuer,stable=False,firstEvent=Nothing,latestEvent=Nothing,latestAttributes='Map'.newMap}
	# meta					= createMeta instanceNo Nothing parent Nothing mmeta pmeta
	# (_,iworld)			= 'SharedDataSource'.write meta (taskInstanceMeta instanceNo) iworld
	# (_,iworld)			= 'SharedDataSource'.write (createReduct instanceNo task taskTime) (taskInstanceReduct instanceNo) iworld
	# (_,iworld)			= 'SharedDataSource'.write (createResult taskTime) (taskInstanceResult instanceNo) iworld
	# (_,iworld)			= 'SharedDataSource'.write (createRep) (taskInstanceRep instanceNo) iworld
	= (TaskId instanceNo 0, iworld)

createMeta :: !InstanceNo (Maybe SessionId) InstanceNo !(Maybe User) !ManagementMeta !ProgressMeta  -> TIMeta
createMeta instanceNo sessionId parent worker mmeta pmeta
	= {TIMeta|instanceNo=instanceNo,sessionId=sessionId,parent=parent,worker=worker,observes=[],observedBy=[],management=mmeta,progress=pmeta}

createReduct :: !InstanceNo !(Task a) !TaskTime -> TIReduct | iTask a
createReduct instanceNo task taskTime
	= {TIReduct|task=toJSONTask task,nextTaskNo=2,nextTaskTime=1,tree=(TCInit (TaskId instanceNo 0) 1),shares = 'Map'.newMap, lists = 'Map'.newMap}
where
	toJSONTask (Task eval) = Task eval`
	where
		eval` event repOpts tree iworld = case eval event repOpts tree iworld of
			(ValueResult val ts rep tree,iworld)	= (ValueResult (fmap toJSON val) ts rep tree, iworld)
			(ExceptionResult e str,iworld)			= (ExceptionResult e str,iworld)

createResult :: TaskTime -> TIResult
createResult taskTime = TIValue NoValue taskTime

createRep :: TIRep
createRep = TaskRep (UIControlGroup {UIControlGroup|attributes='Map'.newMap, controls=[],direction = Vertical,actions = []}) []

//Evaluate a session task instance when a new event is received from a client
evalSessionTaskInstance :: !SessionId !Event !*IWorld -> (!MaybeErrorString (!TaskResult JSONNode, !InstanceNo, !SessionId), !*IWorld)
evalSessionTaskInstance sessionId event iworld 
	//Set session user
	# iworld				= {iworld & currentUser = AnonymousUser sessionId}
	//Update current datetime in iworld
	# iworld				= updateCurrentDateTime iworld
	//Determine which task instance to evaluate
	# (sessionNo, iworld)	= determineSessionInstanceNo sessionId iworld
	| sessionNo == 0		= (Error ("Could not load session " +++ sessionId), iworld)
	//Evaluate the task instance at which the event is targeted
	# (mbResult,iworld)		= evalTaskInstance event (eventTarget event sessionNo) iworld
	//Evaluate urgent task instances (just started workOn's for example)
	# iworld				= refreshUrgentTaskInstances iworld
	//If the session task is outdated compute it a second time
	# (outdated,iworld)		= isSessionOutdated sessionNo iworld
	| outdated
		# (mbResult,iworld)		= evalTaskInstance RefreshEvent sessionNo iworld
		= case mbResult of
			Ok result	= (Ok (result,sessionNo,sessionId),iworld)
			Error e		= (Error e, iworld)
	| otherwise
		= case mbResult of
			Ok result	= (Ok (result,sessionNo,sessionId),iworld)
			Error e		= (Error e, iworld)
where
	determineSessionInstanceNo sessionId iworld=:{IWorld|sessions}
		= case 'Map'.get sessionId sessions of
			Just no	= (no,iworld)
			_		= (0, iworld)

	isSessionOutdated sessionNo iworld //TODO: This function should not really be here
		# (work,iworld) = dequeueWorkFilter (\w -> case w of (Evaluate no) = (no == sessionNo); _ = False) iworld
		= (not (isEmpty work),iworld)

	eventTarget (EditEvent (TaskId no _) _ _)	_	= no
	eventTarget (ActionEvent (TaskId no _) _) _ 	= no
	eventTarget (FocusEvent (TaskId no _)) _		= no
	eventTarget RefreshEvent no						= no

//Evaluate a task instance, just to refresh its state
refreshTaskInstance :: !InstanceNo !*IWorld -> *IWorld
refreshTaskInstance instanceNo iworld
	= snd (evalTaskInstance RefreshEvent instanceNo iworld)

refreshUrgentTaskInstances :: !*IWorld -> *IWorld
refreshUrgentTaskInstances iworld
	# (work,iworld) = dequeueWorkFilter isUrgent iworld
	= seqSt refreshTaskInstance [instanceNo \\EvaluateUrgent instanceNo <- work] iworld
where
	isUrgent (EvaluateUrgent _)	= True
	isUrgent _					= False

//Evaluate a single task instance
evalTaskInstance :: !Event !InstanceNo !*IWorld -> (!MaybeErrorString (TaskResult JSONNode),!*IWorld)
evalTaskInstance event instanceNo iworld=:{currentDateTime,currentUser,currentInstance,nextTaskNo,taskTime,localShares,localLists}
	//Read the task instance data
	# (meta, iworld)			= 'SharedDataSource'.read (taskInstanceMeta instanceNo) iworld
	| isError meta				= (liftError meta, iworld)
	# meta=:{TIMeta|sessionId,parent,worker=Just worker,progress} = fromOk meta
	# (reduct, iworld)			= 'SharedDataSource'.read (taskInstanceReduct instanceNo) iworld
	| isError reduct			= (liftError reduct, iworld)
	# reduct=:{TIReduct|task=Task eval,nextTaskNo=curNextTaskNo,nextTaskTime,tree,shares,lists} = fromOk reduct
	# (result, iworld)			= 'SharedDataSource'.read (taskInstanceResult instanceNo) iworld
	| isError result			= (liftError result, iworld)
	= case fromOk result of
		(TIException e msg)		= (Ok (ExceptionResult e msg), iworld)
		(TIValue val _)
			//Eval instance
			# repAs						= {TaskRepOpts|useLayout=Nothing,afterLayout=Nothing,modLayout=Nothing,appFinalLayout=isJust sessionId}
			//Update current process id & eval stack in iworld
			# taskId					= TaskId instanceNo 0
			# iworld					= {iworld & currentInstance = instanceNo
												  , currentUser = worker
												  , nextTaskNo = reduct.TIReduct.nextTaskNo
												  , taskTime = reduct.TIReduct.nextTaskTime
												  , localShares = shares
												  , localLists = lists} 
			//Clear the instance's registrations for share changes
			# iworld					= clearShareRegistrations instanceNo iworld
			//Apply task's eval function and take updated nextTaskId from iworld
			# (result,iworld)			= eval event repAs tree iworld
			//Update meta data
			# (meta, iworld) = case 'SharedDataSource'.read (taskInstanceMeta instanceNo) iworld of
				(Ok meta, iworld)		= (meta, iworld)
				(_, iworld)				= (meta, iworld)
			# meta						= {TIMeta|meta & progress = updateProgress currentDateTime result progress}
			# (_,iworld)				= 'SharedDataSource'.writeFilterMsg meta ((<>) instanceNo) (taskInstanceMeta instanceNo) iworld //TODO Check error
			//Store updated reduct
			# (nextTaskNo,iworld)		= getNextTaskNo iworld
			# (shares,iworld)			= getLocalShares iworld
			# (lists,iworld)			= getLocalLists iworld
			# reduct					= {TIReduct|reduct & nextTaskNo = nextTaskNo, nextTaskTime = nextTaskTime + 1, tree = tasktree result, shares = shares, lists = lists}
			# (_,iworld)				= 'SharedDataSource'.writeFilterMsg reduct ((<>) instanceNo) (taskInstanceReduct instanceNo) iworld //TODO Check error
			//Store the result
			# (_,iworld)				= 'SharedDataSource'.writeFilterMsg (taskres result) ((<>) instanceNo) (taskInstanceResult instanceNo) iworld //TODO Check error
			//Store the representation
			# (_,iworld)				= 'SharedDataSource'.writeFilterMsg (taskrep result) ((<>) instanceNo) (taskInstanceRep instanceNo) iworld //TODO Check error
			//Return the result
			= (Ok result, iworld)
where
	getNextTaskNo iworld=:{IWorld|nextTaskNo}	= (nextTaskNo,iworld)
	getLocalShares iworld=:{IWorld|localShares}	= (localShares,iworld)
	getLocalLists iworld=:{IWorld|localLists}	= (localLists,iworld)

	updateProgress now result progress
		# progress = {progress & firstEvent = Just (fromMaybe now progress.firstEvent), latestEvent = Just now}
		= case result of
			(ExceptionResult _ _)				= {progress & stable = True}
			(ValueResult (Value _ True) _ _ _)	= {progress & stable = True}
			(ValueResult _ _ (TaskRep ui _) _)	= {progress & stable = False, latestAttributes = uiDefAttributes ui}
			_									= {progress & stable = False}

	tasktree (ValueResult _ _ _ tree)	= tree
	tasktree (ExceptionResult _ _)		= TCNop
	
	taskres (ValueResult val {TaskInfo|lastEvent} _ _)	= TIValue val lastEvent
	taskres (ExceptionResult e str)						= TIException e str
	
	taskrep	(ValueResult _ _ rep _)		= rep
	taskrep (ExceptionResult _ str)		= TaskRep (UIControlSequence {UIControlSequence|attributes = 'Map'.newMap, controls = [(stringDisplay str, 'Map'.newMap)], direction = Vertical}) []
								
localShare :: !TaskId -> Shared a | iTask a
localShare taskId=:(TaskId instanceNo taskNo) = createChangeOnWriteSDS "localShare" shareKey read write
where
	shareKey = toString taskId

	read iworld=:{currentInstance,localShares}
		//Local share
		| instanceNo == currentInstance
			= case 'Map'.get taskId localShares of
				Just encs	
					= case fromJSON encs of
						Just s	= (Ok s, iworld)
						_		= (Error ("Could not decode local shared state " +++ shareKey), iworld)
				_			= (Error ("Could not read local shared state " +++ shareKey), iworld)
		//Share of ancestor
		| otherwise
			= case 'SharedDataSource'.read (taskInstanceReduct instanceNo) iworld of
				(Ok reduct,iworld)
					=  case 'Map'.get taskId reduct.TIReduct.shares of
						Just encs
							= case fromJSON encs of	
								Just s	= (Ok s, iworld)
								_		= (Error ("Could not decode remote shared state " +++ shareKey), iworld)
						_
							= (Error ("Could not read remote shared state " +++ shareKey), iworld)
				(Error _,iworld)
					= (Error ("Could not read remote shared state " +++ shareKey), iworld)
				
	write value iworld=:{currentInstance,localShares}
		| instanceNo == currentInstance
			= (Ok Void, {iworld & localShares = 'Map'.put taskId (toJSON value) localShares})
		| otherwise
			= case 'SharedDataSource'.read (taskInstanceReduct instanceNo) iworld of
				(Ok reduct,iworld)
					# reduct		= {TIReduct|reduct & shares = 'Map'.put taskId (toJSON value) reduct.TIReduct.shares}
					# (_,iworld)	= 'SharedDataSource'.write reduct (taskInstanceReduct instanceNo) iworld
					= (Ok Void, iworld)
				(Error _,iworld)
					= (Error ("Could not write to remote shared state " +++ shareKey), iworld)
		
//Top list share has no items, and is therefore completely polymorphic
topListShare :: SharedTaskList a
topListShare = createReadOnlySDS read
where
	read iworld
		= ({TaskList|listId = TopLevelTaskList, items = []}, iworld)
		
parListShare :: !TaskId -> SharedTaskList a | iTask a
parListShare taskId=:(TaskId instanceNo taskNo) = createReadOnlySDSError read
where
	shareKey = toString taskId
	read iworld=:{currentInstance,localLists}
		| instanceNo == currentInstance		
			= case 'Map'.get taskId localLists of
				Just entries
					= (Ok {TaskList|listId = ParallelTaskList taskId, items = [toItem e\\ e <- entries | not e.TaskListEntry.removed]},iworld)
				_	= (Error ("Could not read local task list " +++ shareKey), iworld)
		| otherwise
			= case 'SharedDataSource'.read (taskInstanceReduct instanceNo) iworld of
				(Ok reduct, iworld)
					= case 'Map'.get taskId reduct.TIReduct.lists of					
						Just entries
							= (Ok {TaskList|listId = ParallelTaskList taskId, items = [toItem e\\ e <- entries | not e.TaskListEntry.removed]},iworld)
						_	= (Error ("Could not read remote task list " +++ shareKey), iworld)
				(Error _,iworld)
					= (Error ("Could not load remote task list " +++ shareKey), iworld)
					
	toItem {TaskListEntry|entryId,state,result=TIValue val ts,attributes}
		= 	{taskId			= entryId
			,value			= deserialize val
			,managementMeta = management
			,progressMeta	= progress
			}
	where
		(progress,management) = case state of
			DetachedState _ p m = (Just p,Just m)
			_					= (Nothing,Nothing)
	
	deserialize NoValue	= NoValue
	deserialize (Value json stable) = maybe NoValue (\v -> Value v stable) (fromJSON json)
