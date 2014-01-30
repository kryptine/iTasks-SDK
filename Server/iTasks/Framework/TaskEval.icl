implementation module iTasks.Framework.TaskEval

import StdList, StdBool, StdTuple, StdMisc
import Data.Error, Data.Func, Data.Either, Text.JSON
import iTasks.Framework.IWorld, iTasks.Framework.Shared, iTasks.Framework.Task, iTasks.Framework.TaskState
import iTasks.Framework.TaskStore, iTasks.Framework.Util, iTasks.Framework.Generic
import iTasks.API.Core.SystemTypes, iTasks.API.Core.LayoutCombinators
import iTasks.Framework.UIDiff
import iTasks.Framework.SDSService

from iTasks.Framework.IWorld			import dequeueWorkFilter
from iTasks.API.Core.CoreCombinators	import :: ParallelTaskType(..), :: ParallelTask(..)
from Data.Map				import qualified newMap, fromList, toList, get, put
from iTasks.Framework.SDS as SDS import qualified read, write, writeFilterMsg

derive gEq TIMeta, TIType

createClientTaskInstance :: !(Task a) !SessionId !InstanceNo !*IWorld -> *(!TaskId, !*IWorld) |  iTask a
createClientTaskInstance task sessionId instanceNo iworld=:{current={localDateTime,taskTime}}
	# worker				= AnonymousUser sessionId
	//Create the initial instance data in the store
	# mmeta					= defaultValue
	# pmeta					= {value=None,issuedAt=localDateTime,issuedBy=worker,stable=False,firstEvent=Nothing,latestEvent=Nothing}
	# meta					= createMeta instanceNo "client" SessionInstance (TaskId 0 0) Nothing mmeta pmeta
	# (_,iworld)			= 'SDS'.write meta (taskInstanceMeta instanceNo) iworld
	# (_,iworld)			= 'SDS'.write (createReduct instanceNo task taskTime) (taskInstanceReduct instanceNo) iworld
	# (_,iworld)			= 'SDS'.write createRep (taskInstanceRep instanceNo) iworld
	# (_,iworld)			= 'SDS'.write (TIValue NoValue) (taskInstanceValue instanceNo) iworld
	= (TaskId instanceNo 0, iworld)	
		
createSessionTaskInstance :: !(Task a) !Event !*IWorld -> (!MaybeErrorString (!TaskResult JSONNode, !InstanceNo, !InstanceKey, !EventNo, ![UIUpdate]), !*IWorld) |  iTask a
createSessionTaskInstance task event iworld=:{current={localDateTime,taskTime}}
	# (instanceNo,iworld)	= newInstanceNo iworld
    # (instanceKey,iworld)  = newInstanceKey iworld
	# worker				= AnonymousUser instanceKey
	//Create the initial instance data in the store
	# mmeta					= defaultValue
	# pmeta					= {value=None,issuedAt=localDateTime,issuedBy=worker,stable=False,firstEvent=Nothing,latestEvent=Nothing}
	# meta					= createMeta instanceNo instanceKey SessionInstance (TaskId 0 0) Nothing mmeta pmeta
	# (_,iworld)			= 'SDS'.write meta (taskInstanceMeta instanceNo) iworld
	# (_,iworld)			= 'SDS'.write (createReduct instanceNo task taskTime) (taskInstanceReduct instanceNo) iworld
	# (_,iworld)			= 'SDS'.write createRep (taskInstanceRep instanceNo) iworld
	# (_,iworld)			= 'SDS'.write (TIValue NoValue) (taskInstanceValue instanceNo) iworld
	//Evaluate once
	# (mbResult,iworld)		= evalTaskInstance event instanceNo iworld
	= case mbResult of
		Ok (result,Left (eventNo,updates))	= (Ok (result,instanceNo,instanceKey,eventNo,updates),iworld)
		Error e				= (Error e, iworld)
		_					= (Error "Unknown error in createSessionTaskInstance", iworld)

createUnevaluatedTaskInstance :: !(Task a) !*IWorld -> (!MaybeErrorString (!InstanceNo,InstanceKey),!*IWorld) | iTask a
createUnevaluatedTaskInstance task iworld=:{current={localDateTime,taskTime}}
	# (instanceNo,iworld)	= newInstanceNo iworld
    # (instanceKey,iworld)  = newInstanceKey iworld
	# worker				= AnonymousUser instanceKey
	# mmeta					= defaultValue
	# pmeta					= {value=None,issuedAt=localDateTime,issuedBy=worker,stable=False,firstEvent=Nothing,latestEvent=Nothing}
	# meta					= createMeta instanceNo instanceKey SessionInstance (TaskId 0 0) Nothing mmeta pmeta
	# (_,iworld)			= 'SDS'.write meta (taskInstanceMeta instanceNo) iworld
	# (_,iworld)			= 'SDS'.write (createReduct instanceNo task taskTime) (taskInstanceReduct instanceNo) iworld
	# (_,iworld)			= 'SDS'.write createRep (taskInstanceRep instanceNo) iworld
	# (_,iworld)			= 'SDS'.write (TIValue NoValue) (taskInstanceValue instanceNo) iworld
    = (Ok (instanceNo,instanceKey),iworld)

createDetachedTaskInstance :: !(Task a) !(Maybe InstanceNo) !(Maybe String) !ManagementMeta !User !TaskId !(Maybe [TaskId]) !*IWorld -> (!TaskId, !*IWorld) | iTask a
createDetachedTaskInstance task mbInstanceNo name mmeta issuer listId mbAttachment iworld=:{current={localDateTime,taskTime}}
	# (instanceNo,iworld)	= case mbInstanceNo of
        Nothing         = newInstanceNo iworld
        Just instanceNo = (instanceNo,iworld)
    # (instanceKey,iworld)  = newInstanceKey iworld
	# pmeta					= {value=None,issuedAt=localDateTime,issuedBy=issuer,stable=False,firstEvent=Nothing,latestEvent=Nothing}
	# meta					= createMeta instanceNo instanceKey (maybe DetachedInstance (\attachment -> TmpAttachedInstance [listId:attachment] issuer) mbAttachment) listId name mmeta pmeta
	# (_,iworld)			= 'SDS'.write meta (taskInstanceMeta instanceNo) iworld
	# (_,iworld)			= 'SDS'.write (createReduct instanceNo task taskTime) (taskInstanceReduct instanceNo) iworld
	# (_,iworld)			= 'SDS'.write createRep (taskInstanceRep instanceNo) iworld
	# (_,iworld)			= 'SDS'.write (TIValue NoValue) (taskInstanceValue instanceNo) iworld
    # iworld                = if (isJust mbAttachment) (queueUrgentEvaluate instanceNo iworld) iworld
	= (TaskId instanceNo 0, iworld)

createMeta :: !InstanceNo !InstanceKey !TIType !TaskId !(Maybe String) !ManagementMeta !ProgressMeta  -> TIMeta
createMeta instanceNo instanceKey instanceType listId name mmeta pmeta
	= {TIMeta|instanceNo=instanceNo,instanceKey=instanceKey,instanceType=instanceType,listId=listId,name=name,management=mmeta,progress=pmeta}

createReduct :: !InstanceNo !(Task a) !TaskTime -> TIReduct | iTask a
createReduct instanceNo task taskTime
	= {TIReduct|task=toJSONTask task,tree=TCInit (TaskId instanceNo 0) 1,nextTaskNo=2,nextTaskTime=1,lastEventNo=0,shares = 'Data.Map'.newMap, lists = 'Data.Map'.newMap, tasks= 'Data.Map'.newMap}
where
	toJSONTask (Task eval) = Task eval`
	where
		eval` event repOpts tree iworld = case eval event repOpts tree iworld of
			(ValueResult val ts rep tree,iworld)	= (ValueResult (fmap toJSON val) ts rep tree, iworld)
			(ExceptionResult e str,iworld)			= (ExceptionResult e str,iworld)

createRep :: TaskRep
createRep = TaskRep {UIDef|content=UIControlStack {UIControlStack|controls=[],attributes='Data.Map'.newMap},windows=[]} []

createResult :: !InstanceNo !TaskTime -> TaskResult JSONNode
createResult instanceNo taskTime = ValueResult NoValue {TaskInfo|lastEvent=taskTime,refreshSensitive=True} (TaskRep {UIDef|content=UIControlStack {UIControlStack|controls=[],attributes='Data.Map'.newMap},windows=[]} []) (TCInit (TaskId instanceNo 0) 1)

evalSessionTaskInstance :: !InstanceNo !Event !*IWorld -> (!MaybeErrorString (!TaskResult JSONNode, !InstanceNo, !EventNo, ![UIUpdate]), !*IWorld)
evalSessionTaskInstance instanceNo event iworld
	//Update current datetime in iworld
	# iworld					= updateCurrentDateTime iworld
    = evalUntilSession event instanceNo (eventTarget event instanceNo) iworld
where
	eventTarget (EditEvent _ (TaskId no _) _ _)	_	= no
	eventTarget (ActionEvent _ (TaskId no _) _) _ 	= no
	eventTarget (FocusEvent _ (TaskId no _)) _		= no
	eventTarget (RefreshEvent _) no					= no

    evalUntilSession event sessionNo instanceNo iworld
        = case evalTaskInstance event instanceNo iworld of
			(Ok (result,Left (eventNo,updates)),iworld) = (Ok (result,sessionNo,eventNo,updates),iworld)   //Done! we have evaluated the session instance
            (Ok (_,Right [next:_]),iworld)                  = evalUntilSession (toRefresh event) next sessionNo iworld //We have not yet reached a session instance
            (Ok (_,Right []),iworld)                        = (Error "Event did not reach an instance attached to a session",iworld)
            (Error e, iworld)                               = (Error e,iworld)

//Evaluate a task instance, just to refresh its state
refreshTaskInstance :: !InstanceNo !*IWorld -> *IWorld
refreshTaskInstance instanceNo iworld
	# (mbResult,iworld)	= evalTaskInstance (RefreshEvent Nothing) instanceNo iworld
	= case mbResult of
		(Ok (_,Left (_,[])))	    = iworld
		(Ok (_,Left (_,updates)))	= addUIMessage instanceNo (UIUpdates updates) iworld
		(Error e)					= addUIMessage instanceNo (UIReset e) iworld	
		_	                        = iworld

refreshUrgentTaskInstances :: !*IWorld -> *IWorld
refreshUrgentTaskInstances iworld
	# (work,iworld) = dequeueWorkFilter isUrgent iworld
	= seqSt refreshTaskInstance [instanceNo \\EvaluateUrgent instanceNo <- work] iworld
where
	isUrgent (EvaluateUrgent _)	= True
	isUrgent _					= False

import StdDebug
//Evaluate a single task instance
evalTaskInstance :: !Event !InstanceNo !*IWorld -> (!MaybeErrorString (TaskResult JSONNode, Either (EventNo,[UIUpdate]) [InstanceNo]),!*IWorld)
evalTaskInstance event instanceNo iworld=:{current=current=:{taskTime,localDateTime,user,taskInstance,nextTaskNo,localShares,localLists}}
    # (oldMeta, iworld)         = 'SDS'.read (taskInstanceMeta instanceNo) iworld
	| isError oldMeta			= (liftError oldMeta, iworld)
	# oldMeta=:{TIMeta|instanceType,instanceKey,listId,progress} = fromOk oldMeta
	# (oldReduct, iworld)		= 'SDS'.read (taskInstanceReduct instanceNo) iworld
	| isError oldReduct			= (liftError oldReduct, iworld)
	# oldReduct=:{TIReduct|task=Task eval,tree,nextTaskNo=curNextTaskNo,nextTaskTime,shares,lists,tasks} = fromOk oldReduct
    //Check execption
    | progress.ProgressMeta.value === Exception
	    # (oldValue, iworld)		= 'SDS'.read (taskInstanceValue instanceNo) iworld
        = case oldValue of
            (Error e)                   = (Error e, iworld)
		    (Ok (TIException e msg))    = (Ok (ExceptionResult e msg, Right []), iworld)
    //Eval instance
    # (currentUser,currentSession,currentAttachment) = case instanceType of
        SessionInstance                         = (AnonymousUser instanceKey,Just instanceNo,[])
        DetachedInstance                        = (SystemUser,Nothing,[])
        AttachedInstance (attachment=:[TaskId sessionNo _:_]) worker    = (worker,Just sessionNo,attachment)
        TmpAttachedInstance (attachment=:[TaskId sessionNo _:_]) worker = (worker,Just sessionNo,attachment)
    # repAs						= {TaskRepOpts|useLayout=Nothing,modLayout=Nothing,noUI=False}
	//Update current process id & eval stack in iworld
	# taskId					= TaskId instanceNo 0
	# eventRoute				= determineEventRoute event lists
	# iworld					= {iworld & current =
                                        { current
                                        & taskInstance = instanceNo
                                        , sessionInstance = currentSession
                                        , attachmentChain = currentAttachment
										, taskTime = oldReduct.TIReduct.nextTaskTime
                                        , nextTaskNo = oldReduct.TIReduct.nextTaskNo
										, user = currentUser
										, localShares = shares
										, localLists = lists
										, localTasks = tasks
										, eventRoute = eventRoute
										}}
    //Clear the instance's registrations for share changes
	# iworld					= clearShareRegistrations instanceNo iworld
	//Apply task's eval function and take updated nextTaskId from iworld
	# (newResult,iworld)		= eval event repAs tree iworld
    //Finalize task UI for session instances
    # newResult                 = if (instanceType === SessionInstance) (finalizeSessionUI newResult) newResult
    # tree                      = case newResult of
        (ValueResult _ _ _ newTree) = newTree
        _                           = tree
	//Re-read old meta data because it may have been changed during the task evaluation
	# (oldMeta,deleted,iworld) = case 'SDS'.read (taskInstanceMeta instanceNo) iworld of
        (Ok meta, iworld)		= (meta,False, iworld)
		(Error e, iworld)		= (oldMeta,True, iworld) //If old meta is no longer there, it must have been removed
    # newMeta					= case instanceType of
       (SessionInstance)           = {TIMeta|oldMeta & progress = updateProgress localDateTime newResult progress}
       (TmpAttachedInstance _ _)   = {TIMeta|oldMeta & progress = updateProgress localDateTime newResult progress, instanceType = DetachedInstance}
       _                           = {TIMeta|oldMeta & progress = updateProgress localDateTime newResult progress}
    //Store new meta data
    # iworld                    = if deleted iworld (snd ('SDS'.write newMeta (taskInstanceMeta instanceNo) iworld)) //TODO Check error
    //Store updated reduct
    # (nextTaskNo,iworld)		= getNextTaskNo iworld
	# (shares,iworld)			= getLocalShares iworld
	# (lists,iworld)			= getLocalLists iworld
	# (tasks,iworld)			= getLocalTasks iworld
	# newReduct					= {TIReduct|oldReduct & tree = tree, nextTaskNo = nextTaskNo, nextTaskTime = nextTaskTime + 1, lastEventNo = lastEventNo event, shares = shares, lists = lists, tasks = tasks}
	# iworld                    = if deleted iworld (snd ('SDS'.writeFilterMsg newReduct ((<>) instanceNo) (taskInstanceReduct instanceNo) iworld)) //TODO Check error
    //Store update value
    # newValue                  = case newResult of
        (ValueResult val _ _ _) = TIValue val
        (ExceptionResult e str) = TIException e str
	# iworld                    = if deleted iworld (snd ('SDS'.write newValue (taskInstanceValue instanceNo) iworld)) //TODO Check error
	//Determine user interface updates by comparing the previous UI to the newly calculated one
	# (out,iworld) = case newMeta.TIMeta.instanceType of
	    SessionInstance = case newResult of
	        (ValueResult _ _ (TaskRep newUI _) _)	
	            = case 'SDS'.read (taskInstanceRep instanceNo) iworld of
                    (Ok (TaskRep oldUI _),iworld)
                        //Editlets compute their own diffs we pass to the diff algorithm
    			        # (editletDiffs,iworld)		= getEditletDiffs iworld
                        # (updates,editletDiffs)    = diffUIDefinitions oldUI newUI event editletDiffs
                        # iworld                    = setEditletDiffs editletDiffs iworld
                        = (Left (lastEventNo event, updates),iworld)
            	    (_,iworld)
                        = (Left (lastEventNo event,[]),iworld) //TODO: Deal with error
            _	
            	= (Left (lastEventNo event,[]),iworld)
        AttachedInstance attachment _
            = (Right [no \\ (TaskId no _) <- attachment],iworld)
		_				= (Right [],iworld)
    //Store updated representation (after we did the diff with previous)
    # newRep                = case newResult of (ValueResult _ _ rep _) = rep ; _ = NoRep
	# iworld				= if deleted iworld (snd ('SDS'.write newRep (taskInstanceRep instanceNo) iworld)) //TODO Check error
    //Return the result
	= (Ok (newResult,out), iworld)
where
	getNextTaskNo iworld=:{IWorld|current={TaskEvalState|nextTaskNo}}	    = (nextTaskNo,iworld)
	getLocalShares iworld=:{IWorld|current={localShares}}	= (localShares,iworld)
	getLocalLists iworld=:{IWorld|current={localLists}}	    = (localLists,iworld)
	getLocalTasks iworld=:{IWorld|current={localTasks}}	    = (localTasks,iworld)
	getEditletDiffs iworld=:{IWorld|current={editletDiffs}}	= (editletDiffs,iworld)
    setEditletDiffs editletDiffs iworld=:{current} = {IWorld|iworld & current = {current & editletDiffs = editletDiffs}}

    finalizeSessionUI (ValueResult value info (TaskRep ui parts) tree)
        = (ValueResult value info (TaskRep (autoLayoutFinal ui) parts) tree)
    finalizeSessionUI res = res

	updateProgress now result progress
		# progress = {progress & firstEvent = Just (fromMaybe now progress.firstEvent), latestEvent = Nothing} //EXPERIMENT
		= case result of
			(ExceptionResult _ _)				= {progress & value = Exception, stable = True}
			(ValueResult (Value _ True) _ _ _)	= {progress & value = Stable, stable = True}
			(ValueResult _ _ (TaskRep ui _) _)	= {progress & value = Unstable, stable = False}
			_									= {progress & value = None, stable = False}


	lastEventNo (EditEvent eventNo _ _ _)     = eventNo
	lastEventNo (ActionEvent eventNo _ _)     = eventNo
	lastEventNo (FocusEvent eventNo _)        = eventNo
	lastEventNo (RefreshEvent (Just eventNo)) = eventNo
	lastEventNo _ = 0

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

	read iworld=:{current={taskInstance,localShares}}
		//Local share
		| instanceNo == taskInstance
			= case 'Data.Map'.get taskId localShares of
				Just encs	
					= case fromJSON encs of
						Just s	= (Ok s, iworld)
						_		= (Error ("Could not decode local shared state " +++ shareKey), iworld)
				_			= (Error ("Could not read local shared state " +++ shareKey), iworld)
		//Share of ancestor
		| otherwise
			= case 'SDS'.read (taskInstanceReduct instanceNo) iworld of
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
				
	write value iworld=:{current=current=:{taskInstance,localShares}}
		| instanceNo == taskInstance
			= (Ok Void, {iworld & current = {current & localShares = 'Data.Map'.put taskId (toJSON value) localShares}})
		| otherwise
			= case 'SDS'.read (taskInstanceReduct instanceNo) iworld of
				(Ok reduct,iworld)
					# reduct		= {TIReduct|reduct & shares = 'Data.Map'.put taskId (toJSON value) reduct.TIReduct.shares}
					# (_,iworld)	= 'SDS'.write reduct (taskInstanceReduct instanceNo) iworld
					= (Ok Void, iworld)
				(Error _,iworld)
					= (Error ("Could not write to remote shared state " +++ shareKey), iworld)

exposedShare :: !String -> ReadWriteShared r w | iTask r & iTask w & TC r & TC w
exposedShare url = createChangeOnWriteSDS "exposedShare" url read write
where
	read :: !*IWorld -> *(!MaybeErrorString r, !*IWorld) | TC r & JSONDecode{|*|} r
	read iworld=:{exposedShares}
		= case 'Data.Map'.get url exposedShares of
			Nothing
				= case readRemoteSDS url iworld of
					(Ok json, iworld) = case fromJSON json of
											Nothing = (Error ("Exposed share type mismatch: " +++ url), iworld)
											(Just val) = (Ok val, iworld)
					(Error e, iworld) = (Error e, iworld)
			Just (shared :: ReadWriteShared r^ w, _)	
				= 'SDS'.read shared iworld
			Just dyn
				= (Error ("Exposed share type mismatch: " +++ url), iworld)
				
	write val iworld=:{exposedShares}
		= case 'Data.Map'.get url exposedShares of
			Nothing
				= writeRemoteSDS (toJSON val) url iworld
			Just (shared :: ReadWriteShared r w^, z)		
				= 'SDS'.write val shared iworld
			Just _
				= (Error ("Exposed share type mismatch: " +++ url), iworld)
				
//Top list share has no items, and is therefore completely polymorphic
topListShare :: SharedTaskList a
topListShare = mapReadWrite (readPrj,writePrj) (fullInstanceMeta >+| currentInstanceShare)
where
	readPrj (instances, currentInstance) = {TaskList|listId = TopLevelTaskList, items = [toTaskListItem m \\ (_,m) <- ('Data.Map'.toList instances) | m.instanceType =!= SessionInstance], selfId = TaskId currentInstance 0}

    toTaskListItem {TIMeta|instanceNo,listId,name,progress,management}
	    = {taskId = TaskId instanceNo 0, listId = listId, name = name, value = NoValue, progressMeta = Just progress, managementMeta = Just management}

    writePrj [] instances = Nothing
    writePrj updates (instances,_) = Just (foldl applyUpdate instances updates)

    applyUpdate instances (TaskId instanceNo 0,management)
        = case 'Data.Map'.get instanceNo instances of
            Just meta   = 'Data.Map'.put instanceNo {TIMeta|meta&management=management} instances
            _           = instances
    applyUpdate instances _ = instances

currentInstanceShare :: ReadOnlyShared InstanceNo
currentInstanceShare = createReadOnlySDS (\iworld=:{current={taskInstance}} -> (taskInstance,iworld))

parListShare :: !TaskId !TaskId -> SharedTaskList a | iTask a
parListShare listId=:(TaskId instanceNo taskNo) entryId = createChangeOnWriteSDS NS_TASK_INSTANCES "meta-index" read write
where
	shareKey = toString listId
	read iworld=:{current={taskInstance,localLists}}
		| instanceNo == taskInstance		
			= case 'Data.Map'.get listId localLists of
				Just entries
					= (Ok {TaskList|listId = ParallelTaskList listId, items = [toItem e\\ e <- entries | hasValueResult e && not e.TaskListEntry.removed],selfId=entryId},iworld)
				_	= (Error ("Could not read local task list " +++ shareKey), iworld)
		| otherwise
			= case 'SDS'.read (taskInstanceReduct instanceNo) iworld of
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
        = case 'SDS'.read (taskInstanceMeta instanceNo) iworld of
	        (Error _,iworld) = (Error ("Could not read task meta data of task instance " <+++ instanceNo), iworld)
            (Ok meta,iworld) = case 'SDS'.write {TIMeta|meta & management=management} (taskInstanceMeta instanceNo) iworld of
                (Error _,iworld) = (Error ("Could not write task meta data of task instance " <+++ instanceNo), iworld)
                (Ok _,iworld)   = updateInstanceMeta updates iworld

    //Update the cached management meta in the task list reduct
    updateListReduct updates iworld=:{current=current=:{taskInstance,localLists}}
       | instanceNo == taskInstance
			= case 'Data.Map'.get listId localLists of
				Just entries    = (Ok Void,{iworld & current = {current & localLists = 'Data.Map'.put listId (applyUpdates updates entries) localLists}})
                _               = (Error ("Could not load local task list " +++ shareKey), iworld)
        | otherwise
			= case 'SDS'.read (taskInstanceReduct instanceNo) iworld of //TODO: Check if reading another shared during a write is ok??
				(Ok reduct, iworld)
					= case 'Data.Map'.get listId reduct.TIReduct.lists of					
                        Just entries
                            = 'SDS'.write {TIReduct|reduct & lists = 'Data.Map'.put listId (applyUpdates updates entries) reduct.TIReduct.lists} (taskInstanceReduct instanceNo) iworld
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
