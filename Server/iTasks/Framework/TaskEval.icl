implementation module iTasks.Framework.TaskEval

import StdList, StdBool, StdTuple, StdMisc
import Data.Error, Data.Func, Data.Tuple, Data.Either, Data.Functor, Data.List, Text.JSON
import iTasks.Framework.IWorld, iTasks.Framework.Task, iTasks.Framework.TaskState
import iTasks.Framework.TaskStore, iTasks.Framework.Util, iTasks.Framework.Generic
import iTasks.API.Core.Types, iTasks.API.Core.LayoutCombinators
import iTasks.Framework.UIDiff
import iTasks.Framework.SDSService

from iTasks.API.Core.TaskCombinators	import :: ParallelTaskType(..), :: ParallelTask(..)
from Data.Map				import qualified newMap, fromList, toList, get, put
from iTasks.Framework.SDS as SDS import qualified read, write, writeFilterMsg
from iTasks.API.Common.SDSCombinators     import >+|, mapReadWrite, mapReadWriteError
from StdFunc import const

derive gEq TIMeta, TIType

createClientTaskInstance :: !(Task a) !SessionId !InstanceNo !*IWorld -> *(!TaskId, !*IWorld) |  iTask a
createClientTaskInstance task sessionId instanceNo iworld=:{current={taskTime},clocks={localDate,localTime}}
	# worker				= AnonymousUser sessionId
	//Create the initial instance data in the store
	# mmeta					= defaultValue
	# pmeta					= {value=None,issuedAt=DateTime localDate localTime,issuedBy=worker,involvedUsers=[],firstEvent=Nothing,latestEvent=Nothing}
	# meta					= createMeta instanceNo "client" SessionInstance (TaskId 0 0) Nothing mmeta pmeta
	# (_,iworld)			= 'SDS'.write meta (taskInstanceMeta instanceNo) iworld
	# (_,iworld)			= 'SDS'.write (createReduct instanceNo task taskTime) (taskInstanceReduct instanceNo) iworld
	# (_,iworld)			= 'SDS'.write (TaskRep emptyUI []) (taskInstanceRep instanceNo) iworld
	# (_,iworld)			= 'SDS'.write (TIValue NoValue) (taskInstanceValue instanceNo) iworld
	= (TaskId instanceNo 0, iworld)	

createTaskInstance :: !(Task a) !*IWorld -> (!MaybeErrorString (!InstanceNo,InstanceKey),!*IWorld) | iTask a
createTaskInstance task iworld=:{current={taskTime},clocks={localDate,localTime}}
	# (instanceNo,iworld)	= newInstanceNo iworld
    # (instanceKey,iworld)  = newInstanceKey iworld
	# worker				= AnonymousUser instanceKey
	# mmeta					= defaultValue
	# pmeta					= {value=None,issuedAt=DateTime localDate localTime,issuedBy=worker,involvedUsers=[],firstEvent=Nothing,latestEvent=Nothing}
	# meta					= createMeta instanceNo instanceKey SessionInstance (TaskId 0 0) Nothing mmeta pmeta
	# (_,iworld)			= 'SDS'.write meta (taskInstanceMeta instanceNo) iworld
	# (_,iworld)			= 'SDS'.write (createReduct instanceNo task taskTime) (taskInstanceReduct instanceNo) iworld
	# (_,iworld)			= 'SDS'.write (TaskRep emptyUI []) (taskInstanceRep instanceNo) iworld
	# (_,iworld)			= 'SDS'.write (TIValue NoValue) (taskInstanceValue instanceNo) iworld
    = (Ok (instanceNo,instanceKey),iworld)

createDetachedTaskInstance :: !(Task a) !(Maybe InstanceNo) !(Maybe String) !ManagementMeta !User !TaskId !(Maybe [TaskId]) !*IWorld -> (!TaskId, !*IWorld) | iTask a
createDetachedTaskInstance task mbInstanceNo name mmeta issuer listId mbAttachment iworld=:{current={taskTime},clocks={localDate,localTime}}
	# (instanceNo,iworld)	= case mbInstanceNo of
        Nothing         = newInstanceNo iworld
        Just instanceNo = (instanceNo,iworld)
    # (instanceKey,iworld)  = newInstanceKey iworld
	# pmeta					= {value=None,issuedAt=DateTime localDate localTime,issuedBy=issuer,involvedUsers=[],firstEvent=Nothing,latestEvent=Nothing}
	# meta					= createMeta instanceNo instanceKey (maybe DetachedInstance (\attachment -> TmpAttachedInstance [listId:attachment] issuer) mbAttachment) listId name mmeta pmeta
	# (_,iworld)			= 'SDS'.write meta (taskInstanceMeta instanceNo) iworld
	# (_,iworld)			= 'SDS'.write (createReduct instanceNo task taskTime) (taskInstanceReduct instanceNo) iworld
	# (_,iworld)			= 'SDS'.write (TaskRep emptyUI []) (taskInstanceRep instanceNo) iworld
	# (_,iworld)			= 'SDS'.write (TIValue NoValue) (taskInstanceValue instanceNo) iworld
    # iworld                = if (isJust mbAttachment) (queueUrgentRefresh [instanceNo] iworld) iworld
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

//Evaluate a single task instance
evalTaskInstance :: !InstanceNo !Event !*IWorld -> (!MaybeErrorString (!EventNo,!TaskValue JSONNode,![UIUpdate]),!*IWorld)
evalTaskInstance instanceNo event iworld
    # iworld = resetUIUpdates instanceNo event iworld
    = evalTaskInstance` instanceNo event iworld
where
    evalTaskInstance` instanceNo event iworld=:{current=current=:{taskTime,user,taskInstance,nextTaskNo,localShares,localLists},clocks={localDate,localTime}}
    # (oldMeta, iworld)         = 'SDS'.read (taskInstanceMeta instanceNo) iworld
	| isError oldMeta			= (liftError oldMeta, iworld)
	# oldMeta=:{TIMeta|instanceType,instanceKey,listId,progress} = fromOk oldMeta
	# (oldReduct, iworld)		= 'SDS'.read (taskInstanceReduct instanceNo) iworld
	| isError oldReduct			= (liftError oldReduct, iworld)
	# oldReduct=:{TIReduct|task=Task eval,tree,nextTaskNo=curNextTaskNo,nextTaskTime,shares,lists,tasks} = fromOk oldReduct
    //Check exeption
    | progress.ProgressMeta.value === Exception
	    # (oldValue, iworld)		= 'SDS'.read (taskInstanceValue instanceNo) iworld
        = case oldValue of
            (Error e)                   = (Error e, iworld)
		    (Ok (TIException e msg))    = (Error msg, iworld)
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
    //Finalize task UI
    # newResult                 = finalizeUI instanceType newResult
    # tree                      = case newResult of
        (ValueResult _ _ _ newTree)  = newTree
        _                                                   = tree
	//Re-read old meta data because it may have been changed during the task evaluation
	# (oldMeta,deleted,iworld) = case 'SDS'.read (taskInstanceMeta instanceNo) iworld of
        (Ok meta, iworld)		= (meta,False, iworld)
		(Error e, iworld)		= (oldMeta,True, iworld) //If old meta is no longer there, it must have been removed
    # newMeta					= case instanceType of
       (SessionInstance)           = {TIMeta|oldMeta & progress = updateProgress (DateTime localDate localTime) newResult currentUser progress}
       (TmpAttachedInstance _ _)   = {TIMeta|oldMeta & progress = updateProgress (DateTime localDate localTime) newResult currentUser progress, instanceType = DetachedInstance}
       _                           = {TIMeta|oldMeta & progress = updateProgress (DateTime localDate localTime) newResult currentUser progress}
    //Store new meta data
    # (mbErr,iworld)            = if deleted (Ok Void,iworld) ('SDS'.write newMeta (taskInstanceMeta instanceNo) iworld)
    | mbErr=:(Error _)          = (liftError mbErr,iworld)

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
	# (mbErr,iworld)            = if deleted (Ok Void,iworld) ('SDS'.write newValue (taskInstanceValue instanceNo) iworld)
    | mbErr=:(Error _)          = (liftError mbErr,iworld)
	//Determine user interface updates by comparing the previous UI to the newly calculated one
	= case newResult of
        (ValueResult value _ newRep _)	
	        = case 'SDS'.read (taskInstanceRep instanceNo) iworld of
                (Ok oldRep, iworld)
                    # oldUI = case oldRep of (TaskRep oldUI _) = oldUI; _ = emptyUI
                    # newUI = case newRep of (TaskRep newUI _) = newUI; _ = emptyUI
                    //Editlets compute their own diffs we pass to the diff algorithm
    			    # (editletDiffs,iworld)		= getEditletDiffs iworld
                    # (updates,editletDiffs)    = diffUIDefinitions oldUI newUI event editletDiffs
                    # iworld                    = setEditletDiffs editletDiffs iworld
                    //Store the new reference UI for further updates
	                # (mbErr,iworld) = if deleted (Ok Void,iworld) ('SDS'.write newRep (taskInstanceRep instanceNo) iworld)
                    | mbErr=:(Error _)
                       = (liftError mbErr,iworld)
                    = (Ok (lastEventNo event,value,updates), iworld)
            	(Error e,iworld)
                    = (Error e, iworld)
        (ExceptionResult e msg)
            = (Error msg, iworld)

	getNextTaskNo iworld=:{IWorld|current={TaskEvalState|nextTaskNo}}	    = (nextTaskNo,iworld)
	getLocalShares iworld=:{IWorld|current={localShares}}	= (localShares,iworld)
	getLocalLists iworld=:{IWorld|current={localLists}}	    = (localLists,iworld)
	getLocalTasks iworld=:{IWorld|current={localTasks}}	    = (localTasks,iworld)
	getEditletDiffs iworld=:{IWorld|current={editletDiffs}}	= (editletDiffs,iworld)
    setEditletDiffs editletDiffs iworld=:{current} = {IWorld|iworld & current = {current & editletDiffs = editletDiffs}}

    finalizeUI instanceType (ValueResult value info (TaskRep ui parts) tree)
        # ui = if (instanceType === SessionInstance) (uiDefSetAttribute "session" "true" ui) ui
        = (ValueResult value info (TaskRep (autoLayoutFinal ui) parts) tree)
    finalizeUI instanceType res = res

	updateProgress now result currentUser progress
		# progress = {progress & firstEvent = Just (fromMaybe now progress.firstEvent), latestEvent = Nothing} //EXPERIMENT
		= case result of
			(ExceptionResult _ _)				= {ProgressMeta|progress & value = Exception}
			(ValueResult (Value _ stable) {TaskInfo|involvedUsers} _ _)	
                = {ProgressMeta|progress & value = if stable Stable Unstable, involvedUsers = [currentUser:involvedUsers]}
			(ValueResult _ {TaskInfo|involvedUsers} _ _)	
                = {ProgressMeta|progress & value = None, involvedUsers = [currentUser:involvedUsers]}
			_									= {ProgressMeta|progress & value = None}

	lastEventNo (EditEvent eventNo _ _ _)     = eventNo
	lastEventNo (ActionEvent eventNo _ _)     = eventNo
	lastEventNo (FocusEvent eventNo _)        = eventNo
	lastEventNo (RefreshEvent (Just eventNo)) = eventNo
	lastEventNo _ = 0

    resetUIUpdates instanceNo ResetEvent iworld = clearUIUpdates instanceNo iworld
    resetUIUpdates _ _ iworld = iworld

//Evaluate a task instance, just to refresh its state
refreshTaskInstance :: !InstanceNo !*IWorld -> *IWorld
refreshTaskInstance instanceNo iworld
	# (mbResult,iworld)	= evalTaskInstance instanceNo (RefreshEvent Nothing) iworld
	= case mbResult of
		(Ok (_,_,updates)) = addUIUpdates instanceNo updates iworld
		_	               = iworld

resetTaskInstance :: !InstanceNo !*IWorld -> *IWorld
resetTaskInstance instanceNo iworld
	# (mbResult,iworld)	= evalTaskInstance instanceNo ResetEvent iworld
    = case mbResult of
		(Ok (_,_,updates)) = addUIUpdates instanceNo updates iworld
        _                  = iworld

determineEventRoute :: Event (Map TaskId [TaskListEntry]) -> Map TaskId Int
determineEventRoute (ResetEvent) _			    = 'Data.Map'.newMap
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

queueRefresh :: ![InstanceNo] !*IWorld -> *IWorld
queueRefresh instanceNos iworld=:{refreshQueue}
	= {iworld & refreshQueue = removeDup (refreshQueue ++ instanceNos)}

queueUrgentRefresh :: ![InstanceNo] !*IWorld -> *IWorld
queueUrgentRefresh instanceNos iworld=:{refreshQueue}
	= {iworld & refreshQueue = removeDup (instanceNos ++ refreshQueue)}

dequeueRefresh :: !*IWorld -> (!Maybe InstanceNo, !*IWorld)
dequeueRefresh iworld=:{refreshQueue=[]} = (Nothing,iworld)
dequeueRefresh iworld=:{refreshQueue=[instanceNo:refreshQueue]} = (Just instanceNo,{iworld & refreshQueue = refreshQueue})

localShare :: !TaskId -> Shared a | iTask a
localShare taskId=:(TaskId instanceNo taskNo) = createReadWriteSDS "localShare" shareKey read write
where
	shareKey = toString taskId

	read Void iworld=:{current={taskInstance,localShares}}
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
				
	write Void value iworld=:{current=current=:{taskInstance,localShares}}
		| instanceNo == taskInstance
			= (Ok (const True), {iworld & current = {current & localShares = 'Data.Map'.put taskId (toJSON value) localShares}})
		| otherwise
			= case 'SDS'.read (taskInstanceReduct instanceNo) iworld of
				(Ok reduct,iworld)
					# reduct		= {TIReduct|reduct & shares = 'Data.Map'.put taskId (toJSON value) reduct.TIReduct.shares}
					# (_,iworld)	= 'SDS'.write reduct (taskInstanceReduct instanceNo) iworld
					= (Ok (const True), iworld)
				(Error _,iworld)
					= (Error ("Could not write to remote shared state " +++ shareKey), iworld)

exposedShare :: !String -> ReadWriteShared r w | iTask r & iTask w & TC r & TC w
exposedShare url = createReadWriteSDS "exposedShare" url read write
where
	read :: !Void !*IWorld -> *(!MaybeErrorString r, !*IWorld) | TC r & JSONDecode{|*|} r
	read Void iworld=:{exposedShares}
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
				
	write Void val iworld=:{exposedShares}
		= case 'Data.Map'.get url exposedShares of
			Nothing
				= appFst (fmap (const (const True))) (writeRemoteSDS (toJSON val) url iworld)
			Just (shared :: ReadWriteShared r w^, z)		
				= appFst (fmap (const (const True))) ('SDS'.write val shared iworld)
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
currentInstanceShare = createReadOnlySDS (\Void iworld=:{current={TaskEvalState|taskInstance}} -> (taskInstance,iworld))

parListShare :: !TaskId !TaskId -> SharedTaskList a | iTask a
parListShare listId=:(TaskId instanceNo taskNo) entryId = createReadWriteSDS NS_TASK_INSTANCES "meta-index" read write
where
	shareKey = toString listId
	read Void iworld=:{current={taskInstance,localLists}}
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

	write Void updates iworld
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
				Just entries    = (Ok (const True),{iworld & current = {current & localLists = 'Data.Map'.put listId (applyUpdates updates entries) localLists}})
                _               = (Error ("Could not load local task list " +++ shareKey), iworld)
        | otherwise
			= case 'SDS'.read (taskInstanceReduct instanceNo) iworld of //TODO: Check if reading another shared during a write is ok??
				(Ok reduct, iworld)
					= case 'Data.Map'.get listId reduct.TIReduct.lists of					
                        Just entries
                            = appFst (fmap (const (const True))) ('SDS'.write {TIReduct|reduct & lists = 'Data.Map'.put listId (applyUpdates updates entries) reduct.TIReduct.lists} (taskInstanceReduct instanceNo) iworld)
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
