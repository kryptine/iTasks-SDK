implementation module iTasks.Framework.TaskEval

import StdList, StdBool, StdTuple
import Data.Error, Data.Func, Data.Either, Text.JSON
import iTasks.Framework.IWorld, iTasks.Framework.Shared, iTasks.Framework.Task, iTasks.Framework.TaskState 
import iTasks.Framework.TaskStore, iTasks.Framework.Util, iTasks.Framework.Generic
import iTasks.API.Core.SystemTypes, iTasks.API.Core.LayoutCombinators
import iTasks.Framework.UIDiff

from iTasks.Framework.IWorld			import dequeueWorkFilter
from iTasks.API.Core.CoreCombinators	import :: ParallelTaskType(..), :: ParallelTask(..)
from Data.Map				import qualified newMap, fromList, toList, get, put
from Data.SharedDataSource	import qualified read, write, writeFilterMsg

derive gEq TIMeta, SessionInfo

createSessionTaskInstance :: !(Task a) !Event !*IWorld -> (!MaybeErrorString (!TaskResult JSONNode, !InstanceNo, !SessionInfo, ![UIUpdate]), !*IWorld) |  iTask a
createSessionTaskInstance task event iworld=:{currentDateTime,taskTime}
	# (sessionId,iworld)	= newSessionId iworld
	# (instanceNo,iworld)	= newInstanceNo iworld
	# worker				= AnonymousUser sessionId
	//Create the initial instance data in the store
	# mmeta					= defaultValue
	# pmeta					= {issuedAt=currentDateTime,issuedBy=worker,stable=False,firstEvent=Nothing,latestEvent=Nothing}
	# meta					= createMeta instanceNo (Just {SessionInfo|sessionId=sessionId,lastEvent=0}) (TaskId 0 0) (Just worker) mmeta pmeta
	# (_,iworld)			= 'Data.SharedDataSource'.write meta (sessionInstanceMeta instanceNo) iworld
	# (_,iworld)			= 'Data.SharedDataSource'.write (createReduct instanceNo task taskTime) (taskInstanceReduct instanceNo) iworld
	# (_,iworld)			= 'Data.SharedDataSource'.write (createResult instanceNo taskTime) (taskInstanceResult instanceNo) iworld
	//Register the sessionId -> instanceNo relation
	# iworld				= registerSession sessionId instanceNo iworld
	//Evaluate once
	# (mbResult,iworld)		= evalTaskInstance event instanceNo iworld 
	= case mbResult of
		Ok (result,Just (sessionInfo,updates))	= (Ok (result,instanceNo,sessionInfo,updates),iworld)
		Error e				= (Error e, iworld)
		_					= (Error "Unknown error in createSessionTaskInstance", iworld) 
where
	registerSession sessionId instanceNo iworld=:{IWorld|sessions}
		= {IWorld|iworld & sessions = 'Data.Map'.put sessionId instanceNo sessions}

createTopTaskInstance :: !(Task a) !(Maybe InstanceNo)  !ManagementMeta !User !TaskId !Bool !*IWorld -> (!TaskId, !*IWorld) | iTask a
createTopTaskInstance task mbInstanceNo mmeta issuer listId evalDirect iworld=:{currentDateTime,taskTime}
	# (instanceNo,iworld)	= case mbInstanceNo of
        Nothing         = newInstanceNo iworld
        Just instanceNo = (instanceNo,iworld)
	# pmeta					= {issuedAt=currentDateTime,issuedBy=issuer,stable=False,firstEvent=Nothing,latestEvent=Nothing}
	# meta					= createMeta instanceNo Nothing listId (if evalDirect (Just issuer) Nothing) mmeta pmeta
	# (_,iworld)			= 'Data.SharedDataSource'.write meta (detachedInstanceMeta instanceNo) iworld
	# (_,iworld)			= 'Data.SharedDataSource'.write (createReduct instanceNo task taskTime) (taskInstanceReduct instanceNo) iworld
	# (_,iworld)			= 'Data.SharedDataSource'.write (createResult instanceNo taskTime) (taskInstanceResult instanceNo) iworld
    # iworld                = if evalDirect (queueUrgentEvaluate instanceNo iworld) iworld
	= (TaskId instanceNo 0, iworld)

createMeta :: !InstanceNo (Maybe SessionInfo) TaskId !(Maybe User) !ManagementMeta !ProgressMeta  -> TIMeta
createMeta instanceNo session listId worker mmeta pmeta
	= {TIMeta|instanceNo=instanceNo,session=session,listId=listId,worker=worker,management=mmeta,progress=pmeta}

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
createResult instanceNo taskTime = ValueResult NoValue {TaskInfo|lastEvent=taskTime,refreshSensitive=True} (TaskRep (UIControlStack {UIControlStack|controls=[],attributes='Data.Map'.newMap}) []) (TCInit (TaskId instanceNo 0) 1)

evalSessionTaskInstance :: !SessionId !Event !*IWorld -> (!MaybeErrorString (!TaskResult JSONNode, !InstanceNo, !SessionInfo, ![UIUpdate]), !*IWorld)
evalSessionTaskInstance sessionId event iworld
	//Set session user
	# iworld					= {iworld & currentUser = AnonymousUser sessionId}
	//Update current datetime in iworld
	# iworld					= updateCurrentDateTime iworld
	//Determine which task instance to evaluate
	# (sessionNo, iworld)		= determineSessionInstanceNo sessionId iworld
	| sessionNo == 0			= (Error ("Could not load session " +++ sessionId), iworld)
	//Evaluate the task instance at which the event is targeted
    # targetNo                  = eventTarget event sessionNo
	# (mbResultPass1,iworld)	= evalTaskInstance event targetNo iworld
    # iworld = case mbResultPass1 of
		(Ok (_,Just ({SessionInfo|sessionId},updates)))	= addUIMessage sessionId (UIUpdates updates) iworld
        _                                               = iworld
	//Evaluate urgent task instances (just started workOn's for example)
	# iworld					= refreshUrgentTaskInstances iworld
	//If the session task is outdated compute it a second time
	# (outdated,iworld)			= isSessionOutdated sessionNo iworld
	| outdated || (sessionNo <> targetNo)
		# (mbResultPass2,iworld)		= evalTaskInstance (toRefresh event) sessionNo iworld
        # iworld = case mbResultPass2 of
		    (Ok (_,Just ({SessionInfo|sessionId},updates)))	= addUIMessage sessionId (UIUpdates updates) iworld
            _                                               = iworld
		= case mbResultPass2 of
			Ok (result,Just (sessionInfo,_))	= (Ok (result,sessionNo,sessionInfo,[]),iworld)
			Error e					            = (Error e, iworld)
			_									= (Error "Unknown error (1) in evalSessionTaskInstance", iworld)
	| otherwise
		= case mbResultPass1 of
			Ok (result,Just (sessionInfo,_))	= (Ok (result,sessionNo,sessionInfo,[]),iworld)
			Error e								= (Error e, iworld)
			_									= (Error "Unknown error (2) in evalSessionTaskInstance", iworld)
where
	determineSessionInstanceNo sessionId iworld=:{IWorld|sessions}
		= case 'Data.Map'.get sessionId sessions of
			Just no	= (no,iworld)
			_		= (0, iworld)

	isSessionOutdated sessionNo iworld //TODO: This function should not really be here
		# (work,iworld) = dequeueWorkFilter (\w -> case w of (Evaluate no) = (no == sessionNo); _ = False) iworld
		= (not (isEmpty work),iworld)

	eventTarget (EditEvent _ (TaskId no _) _ _)	_	= no
	eventTarget (ActionEvent _ (TaskId no _) _) _ 	= no
	eventTarget (FocusEvent _ (TaskId no _)) _		= no
	eventTarget (RefreshEvent _) no					= no

//Evaluate a task instance, just to refresh its state
refreshTaskInstance :: !InstanceNo !*IWorld -> *IWorld
refreshTaskInstance instanceNo iworld
	# (mbResult,iworld)	= evalTaskInstance (RefreshEvent Nothing) instanceNo iworld
	= case mbResult of
		(Ok (_,Just ({SessionInfo|sessionId},updates)))	= addUIMessage sessionId (UIUpdates updates) iworld
		(Error e)						
            //Check if the instance happened to be a session instance
            = case getSessionId instanceNo iworld of
                (Just sessionId,iworld) = addUIMessage sessionId (UIReset e) iworld
                (_,iworld)              = iworld
		_	                            = iworld
where
    getSessionId instanceNo iworld=:{IWorld|sessions}
        = case [sessionId \\ (sessionId,no) <- 'Data.Map'.toList sessions | no == instanceNo] of
            [sessionId:_]   = (Just sessionId,iworld)
            _               = (Nothing,iworld)

refreshUrgentTaskInstances :: !*IWorld -> *IWorld
refreshUrgentTaskInstances iworld
	# (work,iworld) = dequeueWorkFilter isUrgent iworld
	= seqSt refreshTaskInstance [instanceNo \\EvaluateUrgent instanceNo <- work] iworld
where
	isUrgent (EvaluateUrgent _)	= True
	isUrgent _					= False

//Evaluate a single task instance
evalTaskInstance :: !Event !InstanceNo !*IWorld -> (!MaybeErrorString (TaskResult JSONNode,Maybe (SessionInfo,[UIUpdate])),!*IWorld)
evalTaskInstance event instanceNo iworld=:{currentDateTime,currentUser,currentInstance,nextTaskNo,taskTime,localShares,localLists}
	//Read the task instance data
    //TODO: make sure we know it is a session in advance
	# (oldMeta, isSession, iworld)	= case 'Data.SharedDataSource'.read (detachedInstanceMeta instanceNo) iworld of
        (Ok meta,iworld)    = (Ok meta, False, iworld)
        (Error _,iworld)    = case 'Data.SharedDataSource'.read (sessionInstanceMeta instanceNo) iworld of
            (Ok meta,iworld)    = (Ok meta, True, iworld)
            (Error e,iworld)    = (Error e,True,iworld)
	| isError oldMeta			= (liftError oldMeta, iworld)
	# oldMeta=:{TIMeta|session,listId,worker=Just worker,progress} = fromOk oldMeta
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
			# repAs						= {TaskRepOpts|useLayout=Nothing,modLayout=Nothing,appFinalLayout=isJust session,noUI=False}
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
			# (oldMeta, iworld) = case 'Data.SharedDataSource'.read ((if isSession sessionInstanceMeta detachedInstanceMeta) instanceNo) iworld of
				(Ok meta, iworld)		= (meta, iworld)
				(_, iworld)				= (oldMeta, iworld)
			# newMeta					= if isSession
                {TIMeta|oldMeta & progress = updateProgress currentDateTime newResult progress, session = updateSession event oldMeta.TIMeta.session}
                {TIMeta|oldMeta & progress = updateProgress currentDateTime newResult progress}
			# (_,iworld)				= if (newMeta === oldMeta) //Only write if data has actually changed
                (Ok Void,iworld)
                ('Data.SharedDataSource'.writeFilterMsg newMeta ((<>) instanceNo) ((if isSession sessionInstanceMeta detachedInstanceMeta) instanceNo) iworld) //TODO Check error
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
			# updates					= case newMeta.TIMeta.session of
				Just session=:{SessionInfo|sessionId} = case (oldResult,newResult) of
					(ValueResult _ _ (TaskRep oldUI _) _,ValueResult _ _ (TaskRep newUI _) _)	= Just (session, diffUIDefinitions oldUI newUI event differs)
					(_,_)		= Just (session,[])
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
		//# progress = {progress & firstEvent = Just (fromMaybe now progress.firstEvent), latestEvent = Just now}
		# progress = {progress & firstEvent = Just (fromMaybe now progress.firstEvent), latestEvent = Nothing} //EXPERIMENT
		= case result of
			(ExceptionResult _ _)				= {progress & stable = True}
			(ValueResult (Value _ True) _ _ _)	= {progress & stable = True}
			(ValueResult _ _ (TaskRep ui _) _)	= {progress & stable = False}
			_									= {progress & stable = False}

	updateSession (EditEvent eventNo _ _ _) (Just s=:{SessionInfo|lastEvent})		= Just {SessionInfo|s & lastEvent = eventNo}
	updateSession (ActionEvent eventNo _ _) (Just s=:{SessionInfo|lastEvent})		= Just {SessionInfo|s & lastEvent = eventNo}
	updateSession (FocusEvent eventNo _) (Just s=:{SessionInfo|lastEvent})			= Just {SessionInfo|s & lastEvent = eventNo}
	updateSession (RefreshEvent (Just eventNo)) (Just s=:{SessionInfo|lastEvent})	= Just {SessionInfo|s & lastEvent = eventNo}
	updateSession _ session															= session

determineEventRoute :: Event (Map TaskId [TaskListEntry]) -> Map TaskId Int
determineEventRoute (RefreshEvent _) _			= 'Data.Map'.newMap
determineEventRoute (EditEvent _ id _ _) lists	= determineEventRoute` id ('Data.Map'.toList lists)
determineEventRoute (ActionEvent _ id _) lists	= determineEventRoute` id ('Data.Map'.toList lists)
determineEventRoute (FocusEvent _ id) lists		= determineEventRoute` id ('Data.Map'.toList lists)

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
topListShare = createChangeOnWriteSDS NS_TASK_INSTANCES "instances" read write //FIXMESHARE
where
	read iworld=:{IWorld|currentInstance}
		= (Ok {TaskList|listId = TopLevelTaskList, items = [], selfId = TaskId currentInstance 0}, iworld)
		
    write v iworld
        = (Ok Void,iworld)

parListShare :: !TaskId !TaskId -> SharedTaskList a | iTask a
parListShare listId=:(TaskId instanceNo taskNo) entryId = createChangeOnWriteSDS NS_TASK_INSTANCES "detached" read write
where
	shareKey = toString listId
	read iworld=:{currentInstance,localLists}
		| instanceNo == currentInstance		
			= case 'Data.Map'.get listId localLists of
				Just entries
					= (Ok {TaskList|listId = ParallelTaskList listId, items = [toItem e\\ e <- entries | hasValueResult e && not e.TaskListEntry.removed],selfId=entryId},iworld)
				_	= (Error ("Could not read local task list " +++ shareKey), iworld)
		| otherwise
			= case 'Data.SharedDataSource'.read (taskInstanceReduct instanceNo) iworld of
				(Ok reduct, iworld)
					= case 'Data.Map'.get listId reduct.TIReduct.lists of					
						Just entries
							= (Ok {TaskList|listId = ParallelTaskList listId, items = [toItem e\\ e <- entries | hasValueResult e && not e.TaskListEntry.removed],selfId = entryId},iworld)
						_	= (Error ("Could not read remote task list " +++ shareKey), iworld)
				(Error _,iworld)
					= (Error ("Could not load remote task list " +++ shareKey), iworld)
					
    hasValueResult {TaskListEntry|lastEval=ValueResult _ _ _ _} = True
    hasValueResult _                                            = False //TODO: Figure out what to with exceptions on this level

	toItem {TaskListEntry|entryId,name,state,lastEval=ValueResult val _ _ _}
		= 	{taskId			= entryId
            ,listId         = listId
            ,name           = name
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

	write updates iworld
        = case updateInstanceMeta updates iworld of
            (Error e,iworld) = (Error e,iworld)
            (Ok Void,iworld) = updateListReduct updates iworld

    //Update the master index of task instance data
    updateInstanceMeta [] iworld = (Ok Void,iworld)
    updateInstanceMeta [(TaskId instanceNo 0,management):updates] iworld
        = case 'Data.SharedDataSource'.read (detachedInstanceMeta instanceNo) iworld of
	        (Error _,iworld) = (Error ("Could not read task meta data of task instance " <+++ instanceNo), iworld)
            (Ok meta,iworld) = case 'Data.SharedDataSource'.write {TIMeta|meta &management=management} (detachedInstanceMeta instanceNo) iworld of
                (Error _,iworld) = (Error ("Could not write task meta data of task instance " <+++ instanceNo), iworld)
                (Ok _,iworld)   = updateInstanceMeta updates iworld

    //Update the cached management meta in the task list reduct
    updateListReduct updates iworld=:{currentInstance,localLists}
       | instanceNo == currentInstance
			= case 'Data.Map'.get listId localLists of
				Just entries    = (Ok Void,{iworld & localLists = 'Data.Map'.put listId (applyUpdates updates entries) localLists})
                _               = (Error ("Could not load local task list " +++ shareKey), iworld)
        | otherwise
			= case 'Data.SharedDataSource'.read (taskInstanceReduct instanceNo) iworld of //TODO: Check if reading another shared during a write is ok??
				(Ok reduct, iworld)
					= case 'Data.Map'.get listId reduct.TIReduct.lists of					
                        Just entries
                            = 'Data.SharedDataSource'.write {TIReduct|reduct & lists = 'Data.Map'.put listId (applyUpdates updates entries) reduct.TIReduct.lists} (taskInstanceReduct instanceNo) iworld
						_	= (Error ("Could not read remote task list " +++ shareKey), iworld)
				(Error _,iworld)
					= (Error ("Could not load remote task list " +++ shareKey), iworld)
    where
        applyUpdates [] entries = entries
        applyUpdates [(taskId,management):updates] entries = applyUpdates updates (map (updateManagementMeta taskId management) entries)

        updateManagementMeta taskId management e=:{TaskListEntry|entryId,state=DetachedState s p _}
            | entryId == taskId     = {TaskListEntry|e & state = DetachedState s p management}
                                    = e
        updateManagementMeta taskId management e = e
