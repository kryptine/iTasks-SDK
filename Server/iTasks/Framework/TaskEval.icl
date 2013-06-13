implementation module iTasks.Framework.TaskEval

import StdList, StdBool, StdTuple
import Data.Error, Data.Func, Data.Either, Text.JSON
import iTasks.Framework.IWorld, iTasks.Framework.Shared, iTasks.Framework.Task, iTasks.Framework.TaskState 
import iTasks.Framework.TaskStore, iTasks.Framework.Util, iTasks.Framework.iTaskClass
import iTasks.API.Core.SystemTypes, iTasks.API.Core.LayoutCombinators
import iTasks.Framework.UIDiff

from iTasks.Framework.IWorld			import dequeueWorkFilter
from iTasks.API.Core.CoreCombinators	import :: ParallelTaskType(..), :: ParallelTask(..)
from Data.Map				import qualified newMap, fromList, toList, get, put
from Data.SharedDataSource	import qualified read, write, writeFilterMsg

createSessionTaskInstance :: !(Task a) !Event !*IWorld -> (!MaybeErrorString (!TaskResult JSONNode, !InstanceNo, !SessionId, ![UIUpdate]), !*IWorld) |  iTask a
createSessionTaskInstance task event iworld=:{currentDateTime,taskTime}
	# (sessionId,iworld)	= newSessionId iworld
	# (instanceNo,iworld)	= newInstanceNo iworld
	# worker				= AnonymousUser sessionId
	//Create the initial instance data in the store
	# mmeta					= defaultValue
	# pmeta					= {issuedAt=currentDateTime,issuedBy=worker,stable=False,firstEvent=Nothing,latestEvent=Nothing,latestAttributes='Data.Map'.newMap}
	# meta					= createMeta instanceNo (Just sessionId) 0 (Just worker) mmeta pmeta
	# (_,iworld)			= 'Data.SharedDataSource'.write meta (taskInstanceMeta instanceNo) iworld
	# (_,iworld)			= 'Data.SharedDataSource'.write (createReduct instanceNo task taskTime) (taskInstanceReduct instanceNo) iworld
	# (_,iworld)			= 'Data.SharedDataSource'.write (createResult instanceNo taskTime) (taskInstanceResult instanceNo) iworld
	//Register the sessionId -> instanceNo relation
	# iworld				= registerSession sessionId instanceNo iworld
	//Evaluate once
	# (mbResult,iworld)		= evalTaskInstance RefreshEvent instanceNo iworld
	= case mbResult of
		Ok (result,mbUpdates)	= (Ok (result,instanceNo,sessionId,maybe [] snd mbUpdates),iworld)
		Error e					= (Error e, iworld)
where
	registerSession sessionId instanceNo iworld=:{IWorld|sessions}
		= {IWorld|iworld & sessions = 'Data.Map'.put sessionId instanceNo sessions}

createTopTaskInstance  :: !(Task a) !ManagementMeta !User !InstanceNo !*IWorld -> (!TaskId, !*IWorld) | iTask a
createTopTaskInstance  task mmeta issuer parent iworld=:{currentDateTime,taskTime}
	# (instanceNo,iworld)	= newInstanceNo iworld
	# pmeta					= {issuedAt=currentDateTime,issuedBy=issuer,stable=False,firstEvent=Nothing,latestEvent=Nothing,latestAttributes='Data.Map'.newMap}
	# meta					= createMeta instanceNo Nothing parent Nothing mmeta pmeta
	# (_,iworld)			= 'Data.SharedDataSource'.write meta (taskInstanceMeta instanceNo) iworld
	# (_,iworld)			= 'Data.SharedDataSource'.write (createReduct instanceNo task taskTime) (taskInstanceReduct instanceNo) iworld
	# (_,iworld)			= 'Data.SharedDataSource'.write (createResult instanceNo taskTime) (taskInstanceResult instanceNo) iworld
	= (TaskId instanceNo 0, iworld)

createMeta :: !InstanceNo (Maybe SessionId) InstanceNo !(Maybe User) !ManagementMeta !ProgressMeta  -> TIMeta
createMeta instanceNo sessionId parent worker mmeta pmeta
	= {TIMeta|instanceNo=instanceNo,sessionId=sessionId,parent=parent,worker=worker,observes=[],observedBy=[],management=mmeta,progress=pmeta}

createReduct :: !InstanceNo !(Task a) !TaskTime -> TIReduct | iTask a
createReduct instanceNo task taskTime
	= {TIReduct|task=toJSONTask task,nextTaskNo=2,nextTaskTime=1,shares = 'Data.Map'.newMap, lists = 'Data.Map'.newMap, tasks= 'Data.Map'.newMap}
where
	toJSONTask (Task eval) = Task eval`
	where
		eval` event repOpts tree iworld = case eval event repOpts tree iworld of
			(ValueResult val ts rep tree,iworld)	= (ValueResult (fmap toJSON val) ts rep tree, iworld)
			(ExceptionResult e str,iworld)			= (ExceptionResult e str,iworld)

createResult :: !InstanceNo !TaskTime -> TaskResult JSONNode
createResult instanceNo taskTime = ValueResult NoValue {TaskInfo|lastEvent=taskTime,refreshSensitive=True} (TaskRep (UIControlGroup {UIControlGroup|attributes='Data.Map'.newMap, controls=[],direction = Vertical,actions = []}) []) (TCInit (TaskId instanceNo 0) 1)

//Evaluate a session task instance when a new event is received from a client
evalSessionTaskInstance :: !SessionId !Event !*IWorld -> (!MaybeErrorString (!TaskResult JSONNode, !InstanceNo, !SessionId, ![UIUpdate]), !*IWorld)
evalSessionTaskInstance sessionId event iworld 
	//Set session user
	# iworld					= {iworld & currentUser = AnonymousUser sessionId}
	//Update current datetime in iworld
	# iworld					= updateCurrentDateTime iworld
	//Determine which task instance to evaluate
	# (sessionNo, iworld)		= determineSessionInstanceNo sessionId iworld
	| sessionNo == 0			= (Error ("Could not load session " +++ sessionId), iworld)
	//Evaluate the task instance at which the event is targeted
	# (mbResultPass1,iworld)	= evalTaskInstance event (eventTarget event sessionNo) iworld
	//Evaluate urgent task instances (just started workOn's for example)
	# iworld					= refreshUrgentTaskInstances iworld
	//If the session task is outdated compute it a second time
	# (outdated,iworld)			= isSessionOutdated sessionNo iworld
	//Get the additional ui updates that were generated by the refreshing
	# (extraUpdates,iworld)		= getUIUpdates sessionId iworld
	| outdated
		= case mbResultPass1 of
			Ok (_,mbUpdates1)	
				# (mbResultPass2,iworld)		= evalTaskInstance RefreshEvent sessionNo iworld
				= case mbResultPass2 of
					Ok (result,mbUpdates2)	= (Ok (result,sessionNo,sessionId,maybe [] snd mbUpdates1 ++ extraUpdates ++ maybe [] snd mbUpdates2),iworld)
					Error e					= (Error e, iworld)
			Error e				= (Error e, iworld)
	| otherwise
		= case mbResultPass1 of
			Ok (result,mbUpdates1)	= (Ok (result,sessionNo,sessionId,maybe [] snd mbUpdates1 ++ extraUpdates),iworld)
			Error e					= (Error e, iworld)
where
	determineSessionInstanceNo sessionId iworld=:{IWorld|sessions}
		= case 'Data.Map'.get sessionId sessions of
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
	# (mbResult,iworld)	= evalTaskInstance RefreshEvent instanceNo iworld
	= case mbResult of
		(Ok (_,Just (sessionId,updates)))	= addUIUpdates sessionId updates iworld
		_									= iworld

refreshUrgentTaskInstances :: !*IWorld -> *IWorld
refreshUrgentTaskInstances iworld
	# (work,iworld) = dequeueWorkFilter isUrgent iworld
	= seqSt refreshTaskInstance [instanceNo \\EvaluateUrgent instanceNo <- work] iworld
where
	isUrgent (EvaluateUrgent _)	= True
	isUrgent _					= False

//Evaluate a single task instance
evalTaskInstance :: !Event !InstanceNo !*IWorld -> (!MaybeErrorString (TaskResult JSONNode,Maybe (SessionId,[UIUpdate])),!*IWorld)
evalTaskInstance event instanceNo iworld=:{currentDateTime,currentUser,currentInstance,nextTaskNo,taskTime,localShares,localLists}
	//Read the task instance data
	# (oldMeta, iworld)			= 'Data.SharedDataSource'.read (taskInstanceMeta instanceNo) iworld
	| isError oldMeta			= (liftError oldMeta, iworld)
	# oldMeta=:{TIMeta|sessionId,parent,worker=Just worker,progress} = fromOk oldMeta
	# (oldReduct, iworld)		= 'Data.SharedDataSource'.read (taskInstanceReduct instanceNo) iworld
	| isError oldReduct			= (liftError oldReduct, iworld)
	# oldReduct=:{TIReduct|task=Task eval,nextTaskNo=curNextTaskNo,nextTaskTime,shares,lists,tasks} = fromOk oldReduct
	# (oldResult, iworld)		= 'Data.SharedDataSource'.read (taskInstanceResult instanceNo) iworld
	| isError oldResult			= (liftError oldResult, iworld)
	# oldResult					= fromOk oldResult
	= case oldResult of
		(ExceptionResult e msg)		= (Ok (ExceptionResult e msg,Nothing), iworld)
		(ValueResult val _ _ tree)
			//Eval instance
			# repAs						= {TaskRepOpts|useLayout=Nothing,afterLayout=Nothing,modLayout=Nothing,appFinalLayout=isJust sessionId}
			//Update current process id & eval stack in iworld
			# taskId					= TaskId instanceNo 0
			# eventRoute				= determineEventRoute event lists
			# iworld					= {iworld & currentInstance = instanceNo
												  , currentUser = worker
												  , nextTaskNo = oldReduct.TIReduct.nextTaskNo
												  , taskTime = oldReduct.TIReduct.nextTaskTime
												  , localShares = shares
												  , localLists = lists
												  , localTasks = tasks
												  , eventRoute = eventRoute
												  , uiDiffers = 'Data.Map'.newMap
												  } 
			//Clear the instance's registrations for share changes
			# iworld					= clearShareRegistrations instanceNo iworld
			//Apply task's eval function and take updated nextTaskId from iworld
			# (newResult,iworld)		= eval event repAs tree iworld
			//Update meta data
			# (oldMeta, iworld) = case 'Data.SharedDataSource'.read (taskInstanceMeta instanceNo) iworld of
				(Ok meta, iworld)		= (meta, iworld)
				(_, iworld)				= (oldMeta, iworld)
			# newMeta					= {TIMeta|oldMeta & progress = updateProgress currentDateTime newResult progress}
			# (_,iworld)				= 'Data.SharedDataSource'.writeFilterMsg newMeta ((<>) instanceNo) (taskInstanceMeta instanceNo) iworld //TODO Check error
			//Store updated reduct
			# (nextTaskNo,iworld)		= getNextTaskNo iworld
			# (shares,iworld)			= getLocalShares iworld
			# (lists,iworld)			= getLocalLists iworld
			# (tasks,iworld)			= getLocalTasks iworld
			# (differs,iworld)			= getUIDiffers iworld
			# newReduct					= {TIReduct|oldReduct & nextTaskNo = nextTaskNo, nextTaskTime = nextTaskTime + 1, shares = shares, lists = lists, tasks = tasks}
			# (_,iworld)				= 'Data.SharedDataSource'.writeFilterMsg newReduct ((<>) instanceNo) (taskInstanceReduct instanceNo) iworld //TODO Check error
			//Store the result
			# (_,iworld)				= 'Data.SharedDataSource'.writeFilterMsg newResult ((<>) instanceNo) (taskInstanceResult instanceNo) iworld //TODO Check error
			//Determine user interface updates by comparing the previous UI to the newly calculated one
			# updates					= case newMeta.TIMeta.sessionId of
				Just sessionId	= case (oldResult,newResult) of
					(ValueResult _ _ (TaskRep oldUI _) _,ValueResult _ _ (TaskRep newUI _) _)	= Just (sessionId, diffUIDefinitions oldUI newUI event differs)
					(_,_)	= Nothing
				_				= Nothing
			//Return the result
			= (Ok (newResult,updates), iworld)
where
	getNextTaskNo iworld=:{IWorld|nextTaskNo}	= (nextTaskNo,iworld)
	getLocalShares iworld=:{IWorld|localShares}	= (localShares,iworld)
	getLocalLists iworld=:{IWorld|localLists}	= (localLists,iworld)
	getLocalTasks iworld=:{IWorld|localTasks}	= (localTasks,iworld)
	getUIDiffers iworld=:{IWorld|uiDiffers}		= (uiDiffers,iworld)

	updateProgress now result progress
		# progress = {progress & firstEvent = Just (fromMaybe now progress.firstEvent), latestEvent = Just now}
		= case result of
			(ExceptionResult _ _)				= {progress & stable = True}
			(ValueResult (Value _ True) _ _ _)	= {progress & stable = True}
			(ValueResult _ _ (TaskRep ui _) _)	= {progress & stable = False, latestAttributes = uiDefAttributes ui}
			_									= {progress & stable = False}

determineEventRoute :: Event (Map TaskId [TaskListEntry]) -> Map TaskId Int
determineEventRoute RefreshEvent _ = 'Data.Map'.newMap 
determineEventRoute (EditEvent id _ _) lists	= determineEventRoute` id ('Data.Map'.toList lists)
determineEventRoute (ActionEvent id _) lists	= determineEventRoute` id ('Data.Map'.toList lists)
determineEventRoute (FocusEvent id) lists		= determineEventRoute` id ('Data.Map'.toList lists)

//TODO: Optimize this search function
determineEventRoute` :: TaskId [(TaskId,[TaskListEntry])] -> Map TaskId Int 
determineEventRoute` eventId lists = 'Data.Map'.fromList (search eventId)
where
	search searchId = case searchInLists searchId lists of	
		Just (parId, index)	= [(parId,index):search parId]
		Nothing				= []

	searchInLists searchId [] = Nothing
	searchInLists searchId [(parId,entries):xs] = case [i \\ e <- entries & i <- [0..] | inEntry searchId e] of
		[index] = Just (parId,index)
		_		= searchInLists searchId xs

	inEntry searchId {TaskListEntry|lastEval=ValueResult _ _ _ tree} = inTree searchId tree
	inEntry _ _ = False

	inTree searchId (TCInit taskId _) = searchId == taskId
	inTree searchId (TCBasic taskId _ _ _) = searchId == taskId
	inTree searchId (TCInteract taskId _ _ _ _ _) = searchId == taskId
	inTree searchId (TCInteract1 taskId _ _ _) = searchId == taskId
	inTree searchId (TCInteract2 taskId _ _ _ _) = searchId == taskId
	inTree searchId (TCProject taskId _ tree) = searchId == taskId || inTree searchId tree
	inTree searchId (TCStep taskId _ (Left tree)) = searchId == taskId || inTree searchId tree
	inTree searchId (TCStep taskId _ (Right (_,_,tree))) = searchId == taskId || inTree searchId tree
	inTree searchId (TCParallel taskId _) = searchId == taskId
	inTree searchId (TCShared taskId _ tree) = searchId == taskId || inTree searchId tree
	inTree searchId (TCStable taskId _ _) = searchId == taskId
	inTree searchId _ = False

localShare :: !TaskId -> Shared a | iTask a
localShare taskId=:(TaskId instanceNo taskNo) = createChangeOnWriteSDS "localShare" shareKey read write
where
	shareKey = toString taskId

	read iworld=:{currentInstance,localShares}
		//Local share
		| instanceNo == currentInstance
			= case 'Data.Map'.get taskId localShares of
				Just encs	
					= case fromJSON encs of
						Just s	= (Ok s, iworld)
						_		= (Error ("Could not decode local shared state " +++ shareKey), iworld)
				_			= (Error ("Could not read local shared state " +++ shareKey), iworld)
		//Share of ancestor
		| otherwise
			= case 'Data.SharedDataSource'.read (taskInstanceReduct instanceNo) iworld of
				(Ok reduct,iworld)
					=  case 'Data.Map'.get taskId reduct.TIReduct.shares of
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
			= (Ok Void, {iworld & localShares = 'Data.Map'.put taskId (toJSON value) localShares})
		| otherwise
			= case 'Data.SharedDataSource'.read (taskInstanceReduct instanceNo) iworld of
				(Ok reduct,iworld)
					# reduct		= {TIReduct|reduct & shares = 'Data.Map'.put taskId (toJSON value) reduct.TIReduct.shares}
					# (_,iworld)	= 'Data.SharedDataSource'.write reduct (taskInstanceReduct instanceNo) iworld
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
			= case 'Data.Map'.get taskId localLists of
				Just entries
					= (Ok {TaskList|listId = ParallelTaskList taskId, items = [toItem e\\ e <- entries | not e.TaskListEntry.removed]},iworld)
				_	= (Error ("Could not read local task list " +++ shareKey), iworld)
		| otherwise
			= case 'Data.SharedDataSource'.read (taskInstanceReduct instanceNo) iworld of
				(Ok reduct, iworld)
					= case 'Data.Map'.get taskId reduct.TIReduct.lists of					
						Just entries
							= (Ok {TaskList|listId = ParallelTaskList taskId, items = [toItem e\\ e <- entries | not e.TaskListEntry.removed]},iworld)
						_	= (Error ("Could not read remote task list " +++ shareKey), iworld)
				(Error _,iworld)
					= (Error ("Could not load remote task list " +++ shareKey), iworld)
					
	toItem {TaskListEntry|entryId,state,lastEval=ValueResult val _ _ _,attributes}
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
