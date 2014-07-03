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
from iTasks.Framework.SDS as SDS import qualified read, write, read, write
from iTasks.API.Core.SDSCombinators     import sdsFocus
from iTasks.API.Common.SDSCombinators   import >+|, mapReadWrite, mapReadWriteError
from StdFunc import const

derive gEq TIMeta, TIType

createClientTaskInstance :: !(Task a) !SessionId !InstanceNo !*IWorld -> *(!TaskId, !*IWorld) |  iTask a
createClientTaskInstance task sessionId instanceNo iworld=:{server={buildID},current={taskTime},clocks={localDate,localTime}}
	# worker				= AnonymousUser sessionId
	//Create the initial instance data in the store
	# mmeta					= defaultValue
	# pmeta					= {ProgressMeta|value=None,issuedAt=DateTime localDate localTime,issuedBy=worker,involvedUsers=[],firstEvent=Nothing,lastEvent=Nothing,connectedTo=Nothing,lastIO=Nothing}
	# meta					= createMeta instanceNo "client" True DetachedInstance (TaskId 0 0) Nothing mmeta pmeta buildID
	# (_,iworld)			= 'SDS'.write meta (sdsFocus instanceNo taskInstanceMeta) iworld
	# (_,iworld)			= 'SDS'.write (createReduct instanceNo task taskTime) (taskInstanceReduct instanceNo) iworld
	# (_,iworld)			= 'SDS'.write (TaskRep emptyUI []) (taskInstanceRep instanceNo) iworld
	# (_,iworld)			= 'SDS'.write (TIValue NoValue) (taskInstanceValue instanceNo) iworld
	= (TaskId instanceNo 0, iworld)	

createTaskInstance :: !(Task a) !*IWorld -> (!MaybeErrorString (!InstanceNo,InstanceKey),!*IWorld) | iTask a
createTaskInstance task iworld=:{server={buildID},current={taskTime},clocks={localDate,localTime}}
	# (instanceNo,iworld)	= newInstanceNo iworld
    # (instanceKey,iworld)  = newInstanceKey iworld
	# worker				= AnonymousUser instanceKey
	# mmeta					= defaultValue
	# pmeta					= {ProgressMeta|value=None,issuedAt=DateTime localDate localTime,issuedBy=worker,involvedUsers=[],firstEvent=Nothing,lastEvent=Nothing,connectedTo=Nothing,lastIO=Just (DateTime localDate localTime)}
	# meta					= createMeta instanceNo instanceKey True DetachedInstance (TaskId 0 0) Nothing mmeta pmeta buildID
	# (_,iworld)			= 'SDS'.write meta (sdsFocus instanceNo taskInstanceMeta) iworld
	# (_,iworld)			= 'SDS'.write (createReduct instanceNo task taskTime) (taskInstanceReduct instanceNo) iworld
	# (_,iworld)			= 'SDS'.write (TaskRep emptyUI []) (taskInstanceRep instanceNo) iworld
	# (_,iworld)			= 'SDS'.write (TIValue NoValue) (taskInstanceValue instanceNo) iworld
    = (Ok (instanceNo,instanceKey),iworld)

createDetachedTaskInstance :: !(Task a) !(Maybe InstanceNo) !(Maybe String) !TaskAttributes !User !TaskId !(Maybe [TaskId]) !*IWorld -> (!TaskId, !*IWorld) | iTask a
createDetachedTaskInstance task mbInstanceNo name attributes issuer listId mbAttachment iworld=:{server={buildID},current={taskTime},clocks={localDate,localTime}}
	# (instanceNo,iworld)	= case mbInstanceNo of
        Nothing         = newInstanceNo iworld
        Just instanceNo = (instanceNo,iworld)
    # (instanceKey,iworld)  = newInstanceKey iworld
	# pmeta					= {ProgressMeta|value=None,issuedAt=DateTime localDate localTime,issuedBy=issuer,involvedUsers=[],firstEvent=Nothing,lastEvent=Nothing,connectedTo=Nothing,lastIO=Nothing}
	# meta					= createMeta instanceNo instanceKey False (maybe DetachedInstance (\attachment -> TmpAttachedInstance [listId:attachment] issuer) mbAttachment) listId name attributes pmeta buildID
	# (_,iworld)			= 'SDS'.write meta (sdsFocus instanceNo taskInstanceMeta) iworld
	# (_,iworld)			= 'SDS'.write (createReduct instanceNo task taskTime) (taskInstanceReduct instanceNo) iworld
	# (_,iworld)			= 'SDS'.write (TaskRep emptyUI []) (taskInstanceRep instanceNo) iworld
	# (_,iworld)			= 'SDS'.write (TIValue NoValue) (taskInstanceValue instanceNo) iworld
    # iworld                = if (isJust mbAttachment) (queueUrgentRefresh [instanceNo] iworld) iworld
	= (TaskId instanceNo 0, iworld)

createMeta :: !InstanceNo !InstanceKey !Bool !TIType !TaskId !(Maybe String) !TaskAttributes !ProgressMeta !String -> TIMeta
createMeta instanceNo instanceKey session instanceType listId name attributes pmeta buildID
	= {TIMeta|instanceNo=instanceNo,instanceKey=instanceKey,instanceType=instanceType,session=session
      ,listId=listId,name=name,progress=pmeta,attributes=attributes,build=buildID}

createReduct :: !InstanceNo !(Task a) !TaskTime -> TIReduct | iTask a
createReduct instanceNo task taskTime
	= {TIReduct|task=toJSONTask task,tree=TCInit (TaskId instanceNo 0) 1,nextTaskNo=2,nextTaskTime=1,lastEventNo=0,shares = 'Data.Map'.newMap, lists = 'Data.Map'.newMap, tasks= 'Data.Map'.newMap}
where
	toJSONTask (Task eval) = Task eval`
	where
		eval` event repOpts tree iworld = case eval event repOpts tree iworld of
			(ValueResult val ts rep tree,iworld)	= (ValueResult (fmap toJSON val) ts rep tree, iworld)
			(ExceptionResult e,iworld)			    = (ExceptionResult e,iworld)

replaceTaskInstance :: !InstanceNo !(Task a) *IWorld -> (!MaybeErrorString (), !*IWorld) | iTask a
replaceTaskInstance instanceNo task iworld=:{server={buildID},current={taskTime}}
    # (meta, iworld)        = 'SDS'.read (sdsFocus instanceNo taskInstanceMeta) iworld
	| isError meta          = ((\(Error (e,msg)) -> Error msg) meta, iworld)
	# (_,iworld)			= 'SDS'.write (createReduct instanceNo task taskTime) (taskInstanceReduct instanceNo) iworld
	# (_,iworld)			= 'SDS'.write (TaskRep emptyUI []) (taskInstanceRep instanceNo) iworld
	# (_,iworld)			= 'SDS'.write (TIValue NoValue) (taskInstanceValue instanceNo) iworld
    # (_,iworld)            = 'SDS'.write {TIMeta|fromOk meta & build=buildID} (sdsFocus instanceNo taskInstanceMeta) iworld
    = (Ok (), iworld)

//Evaluate a single task instance
evalTaskInstance :: !InstanceNo !Event !*IWorld -> (!MaybeErrorString (!EventNo,!TaskValue JSONNode,![UIUpdate]),!*IWorld)
evalTaskInstance instanceNo event iworld
    # iworld = resetUIUpdates instanceNo event iworld
    = evalTaskInstance` instanceNo event iworld
where
    evalTaskInstance` instanceNo event iworld=:{current=current=:{taskTime,user,taskInstance,nextTaskNo,localShares,localLists},clocks={localDate,localTime}}
    # (oldMeta, iworld)         = 'SDS'.read (sdsFocus instanceNo taskInstanceMeta) iworld
	| isError oldMeta           = ((\(Error (e,msg)) -> Error msg) oldMeta, iworld)
	# oldMeta=:{TIMeta|instanceType,instanceKey,session,listId,progress} = fromOk oldMeta
	# (oldReduct, iworld)		= 'SDS'.read (taskInstanceReduct instanceNo) iworld
	| isError oldReduct			= ((\(Error (e,msg)) -> Error msg) oldReduct, iworld)
	# oldReduct=:{TIReduct|task=Task eval,tree,nextTaskNo=curNextTaskNo,nextTaskTime,shares,lists,tasks} = fromOk oldReduct
    //Check exeption
    | progress.ProgressMeta.value === Exception
	    # (oldValue, iworld)		= 'SDS'.read (taskInstanceValue instanceNo) iworld
        = case oldValue of
            (Error (e,msg))             = (Error msg, iworld)
		    (Ok (TIException e msg))    = (Error msg, iworld)
            (Ok _)                      = (Error "Exception no longer available", iworld)
    //Eval instance
    # (currentUser,currentSession,currentAttachment) = case (session,instanceType) of
        (True,_)                = (AnonymousUser instanceKey,Just instanceNo,[])
        (_,DetachedInstance)    = (SystemUser,Nothing,[])
        (_,AttachedInstance (attachment=:[TaskId sessionNo _:_]) worker)    = (worker,Just sessionNo,attachment)
        (_,TmpAttachedInstance (attachment=:[TaskId sessionNo _:_]) worker) = (worker,Just sessionNo,attachment)
    # evalOpts					= {TaskEvalOpts|useLayout=Nothing,modLayout=Nothing,noUI=False,callTrace=[]}
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
	# iworld					    = clearShareRegistrations instanceNo iworld
	//Apply task's eval function and take updated nextTaskId from iworld
	# (newResult,iworld=:{current})	= eval event evalOpts tree iworld
    //Finalize task UI
    # newResult                 = finalizeUI session newResult
    # tree                      = case newResult of
        (ValueResult _ _ _ newTree)  = newTree
        _                                                   = tree
    //Reset necessary 'current' values in iworld
    # iworld = {IWorld|iworld & current = {TaskEvalState|current & taskInstance = 0}}
	//Re-read old meta data because it may have been changed during the task evaluation
	# (oldMeta,deleted,iworld) = case 'SDS'.read (sdsFocus instanceNo taskInstanceMeta) iworld of
        (Ok meta, iworld)		= (meta,False, iworld)
		(Error e, iworld)		= (oldMeta,True, iworld) //If old meta is no longer there, it must have been removed
    # newMeta					= case (session,instanceType) of
       (True,_)                     = {TIMeta|oldMeta & progress = updateProgress (DateTime localDate localTime) newResult currentUser progress}
       (_,TmpAttachedInstance _ _)  = {TIMeta|oldMeta & progress = updateProgress (DateTime localDate localTime) newResult currentUser progress, instanceType = DetachedInstance}
       _                            = {TIMeta|oldMeta & progress = updateProgress (DateTime localDate localTime) newResult currentUser progress}
    //Store new meta data
    # (mbErr,iworld)            = if deleted (Ok Void,iworld) ('SDS'.write newMeta (sdsFocus instanceNo taskInstanceMeta) iworld)
    = case mbErr of
        Error (e,msg)          = (Error msg,iworld)
        Ok _
            //Store updated reduct
            # (nextTaskNo,iworld)		= getNextTaskNo iworld
            # (shares,iworld)			= getLocalShares iworld
            # (lists,iworld)			= getLocalLists iworld
            # (tasks,iworld)			= getLocalTasks iworld
            # newReduct					= {TIReduct|oldReduct & tree = tree, nextTaskNo = nextTaskNo, nextTaskTime = nextTaskTime + 1, lastEventNo = lastEventNo event, shares = shares, lists = lists, tasks = tasks}
            # iworld                    = if deleted iworld (snd ('SDS'.write newReduct (taskInstanceReduct instanceNo) iworld)) //TODO Check error
            //Store update value
            # newValue                  = case newResult of
                (ValueResult val _ _ _)     = TIValue val
                (ExceptionResult (e,str))   = TIException e str
            # (mbErr,iworld)            = if deleted (Ok Void,iworld) ('SDS'.write newValue (taskInstanceValue instanceNo) iworld)
            = case mbErr of
                Error (e,msg)          = (Error msg,iworld)
                Ok _
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
                                = case mbErr of
                                    (Error (e,msg)) = (Error msg,iworld)
                                    Ok _            = (Ok (lastEventNo event,value,updates), iworld)
                            (Error (e,msg),iworld)
                                = (Error msg, iworld)
                    (ExceptionResult (e,msg))
                        = (Error msg, iworld)

	getNextTaskNo iworld=:{IWorld|current={TaskEvalState|nextTaskNo}}	    = (nextTaskNo,iworld)
	getLocalShares iworld=:{IWorld|current={localShares}}	= (localShares,iworld)
	getLocalLists iworld=:{IWorld|current={localLists}}	    = (localLists,iworld)
	getLocalTasks iworld=:{IWorld|current={localTasks}}	    = (localTasks,iworld)
	getEditletDiffs iworld=:{IWorld|current={editletDiffs}}	= (editletDiffs,iworld)
    setEditletDiffs editletDiffs iworld=:{current} = {IWorld|iworld & current = {current & editletDiffs = editletDiffs}}

    finalizeUI session (ValueResult value info (TaskRep ui parts) tree)
        # ui = if session (uiDefSetAttribute "session" "true" ui) ui
        = (ValueResult value info (TaskRep (autoLayoutFinal ui) parts) tree)
    finalizeUI session res = res

	updateProgress now result currentUser progress
		# progress = {ProgressMeta|progress & firstEvent = Just (fromMaybe now progress.ProgressMeta.firstEvent), lastEvent = Nothing} //EXPERIMENT
		= case result of
			(ExceptionResult _)				    = {ProgressMeta|progress & value = Exception}
			(ValueResult (Value _ stable) {TaskEvalInfo|involvedUsers} _ _)	
                = {ProgressMeta|progress & value = if stable Stable Unstable, involvedUsers = [currentUser:involvedUsers]}
			(ValueResult _ {TaskEvalInfo|involvedUsers} _ _)	
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

updateInstanceLastIO ::![InstanceNo] !*IWorld -> *IWorld
updateInstanceLastIO [] iworld = iworld
updateInstanceLastIO [instanceNo:instanceNos] iworld=:{IWorld|clocks={localDate,localTime}}
    = case 'SDS'.read (sdsFocus instanceNo taskInstanceMeta) iworld of
        (Ok meta, iworld)
            # (_,iworld) = 'SDS'.write {TIMeta|meta & progress = {ProgressMeta|meta.TIMeta.progress & lastIO = Just (DateTime localDate localTime)}} (sdsFocus instanceNo taskInstanceMeta) iworld
            = updateInstanceLastIO instanceNos iworld
        (_,iworld) = updateInstanceLastIO instanceNos iworld

updateInstanceConnect :: !String ![InstanceNo] !*IWorld -> *IWorld
updateInstanceConnect client [] iworld = iworld
updateInstanceConnect client [instanceNo:instanceNos] iworld=:{IWorld|clocks={localDate,localTime}}
    = case 'SDS'.read (sdsFocus instanceNo taskInstanceMeta) iworld of
        (Ok meta, iworld)
            # (_,iworld) = 'SDS'.write {TIMeta|meta & progress = {ProgressMeta|meta.TIMeta.progress & connectedTo = Just client, lastIO = Just (DateTime localDate localTime)}} (sdsFocus instanceNo taskInstanceMeta) iworld
            = updateInstanceConnect client instanceNos iworld
        (_,iworld) = updateInstanceConnect client instanceNos iworld

updateInstanceDisconnect :: ![InstanceNo] !*IWorld -> *IWorld
updateInstanceDisconnect [] iworld = iworld
updateInstanceDisconnect [instanceNo:instanceNos] iworld=:{IWorld|clocks={localDate,localTime}}
    = case 'SDS'.read (sdsFocus instanceNo taskInstanceMeta) iworld of
        (Ok meta, iworld)
            # (_,iworld) = 'SDS'.write {TIMeta|meta & progress = {ProgressMeta|meta.TIMeta.progress & connectedTo = Nothing, lastIO = Just (DateTime localDate localTime)}} (sdsFocus instanceNo taskInstanceMeta) iworld
            = updateInstanceDisconnect instanceNos iworld
        (_,iworld) = updateInstanceDisconnect instanceNos iworld

localShare :: !TaskId -> Shared a | iTask a
localShare taskId=:(TaskId instanceNo taskNo) = createReadWriteSDS "localShare" shareKey read write
where
	shareKey = toString taskId
    decodeError = "Could not decode shared state " +++ shareKey
    readError   = "Could not read shared state " +++ shareKey

	read Void iworld=:{current={taskInstance,localShares}}
		//Local share
		| instanceNo == taskInstance
			= case 'Data.Map'.get taskId localShares of
				Just encs	
					= case fromJSON encs of
						Just s	= (Ok s, iworld)
						_		= (Error (dynamic decodeError,decodeError), iworld)
				_			= (Error (dynamic readError,readError), iworld)
		//Share of ancestor
		| otherwise
			= case 'SDS'.read (taskInstanceReduct instanceNo) iworld of
				(Ok reduct,iworld)
					=  case 'Data.Map'.get taskId reduct.TIReduct.shares of
						Just encs
							= case fromJSON encs of	
								Just s	= (Ok s, iworld)
								_		= (Error (dynamic decodeError,decodeError), iworld)
						_
							= (Error (dynamic readError,readError), iworld)
				(Error e,iworld)
					= (Error e, iworld)
				
	write Void value iworld=:{current=current=:{taskInstance,localShares}}
		| instanceNo == taskInstance
			= (Ok (const True), {iworld & current = {current & localShares = 'Data.Map'.put taskId (toJSON value) localShares}})
		| otherwise
			= case 'SDS'.read (taskInstanceReduct instanceNo) iworld of
				(Ok reduct,iworld)
					# reduct		= {TIReduct|reduct & shares = 'Data.Map'.put taskId (toJSON value) reduct.TIReduct.shares}
					# (_,iworld)	= 'SDS'.write reduct (taskInstanceReduct instanceNo) iworld
					= (Ok (const True), iworld)
				(Error e,iworld)
					= (Error e, iworld)

exposedShare :: !String -> RWShared p r w | iTask r & iTask w & TC r & TC w & TC p & JSONEncode{|*|} p
exposedShare url = SDSDynamic f
where
	f _ iworld=:{exposedShares}
        = case 'Data.Map'.get url exposedShares of
		    Nothing
			    = (Ok (createReadWriteSDS "exposedShare" url rread rwrite), iworld)
			Just (shared :: RWShared p^ r^ w^, _)	
				= (Ok shared, iworld)
			Just dyn
			    = (Error (dynamic mismatchError,mismatchError), iworld)

	rread p iworld
			= case readRemoteSDS (toJSON p) url iworld of
				(Ok json, iworld) = case fromJSON json of
										Nothing     = (Error (dynamic mismatchError,mismatchError), iworld)
										(Just val)  = (Ok val, iworld)
				(Error msg, iworld) = (Error (dynamic msg,msg), iworld)
	
	rwrite p val iworld
        = case writeRemoteSDS (toJSON p) (toJSON val) url iworld of
            (Ok _,iworld)       = (Ok (const True),iworld)
            (Error msg,iworld)  = (Error (dynamic msg,msg),iworld)
							
    mismatchError = "Exposed share type mismatch: " +++ url


//Top list share has no items, and is therefore completely polymorphic
topListShare :: SharedTaskList a
topListShare = mapReadWrite (readPrj,writePrj) (sdsFocus {InstanceFilter|instanceNo=Nothing,session=Just False} filteredInstanceMeta >+| currentInstanceShare)
where
	readPrj (instances, currentInstance) = {TaskList|listId = TopLevelTaskList, items = map toTaskListItem instances, selfId = TaskId currentInstance 0}

    toTaskListItem {TIMeta|instanceNo,listId,name,progress,attributes}
	    = {taskId = TaskId instanceNo 0, listId = listId, name = name, value = NoValue, progressMeta = Just progress, attributes = attributes}

    writePrj [] instances = Nothing
    writePrj updates (instances,_) = Just (foldl applyUpdate instances updates)

    applyUpdate instances (TaskId targetNo 0,attr) = map upd instances
    where
        upd m=:{TIMeta|instanceNo}
            | instanceNo == targetNo    = {TIMeta|m & attributes=attr}
                                        = m
    applyUpdate instances _ = instances

currentInstanceShare :: ReadOnlyShared InstanceNo
currentInstanceShare = createReadOnlySDS (\Void iworld=:{current={TaskEvalState|taskInstance}} -> (taskInstance,iworld))

parListShare :: !TaskId !TaskId -> SharedTaskList a | iTask a
parListShare listId=:(TaskId instanceNo taskNo) entryId = createReadWriteSDS NS_TASK_INSTANCES ("parListSDS-"+++toString listId) read write
where
	shareKey = toString listId
    readError = "Could not read task list " +++ shareKey

	read Void iworld=:{current={taskInstance,localLists}}
		| instanceNo == taskInstance		
			= case 'Data.Map'.get listId localLists of
				Just entries
					= (Ok {TaskList|listId = ParallelTaskList listId, items = [toItem e\\ e <- entries | hasValueResult e && not e.TaskListEntry.removed],selfId=entryId},iworld)
				_	= (Error (dynamic readError,readError), iworld)
		| otherwise
			= case 'SDS'.read (taskInstanceReduct instanceNo) iworld of
				(Ok reduct, iworld)
					= case 'Data.Map'.get listId reduct.TIReduct.lists of					
						Just entries
							= (Ok {TaskList|listId = ParallelTaskList listId, items = [toItem e\\ e <- entries | hasValueResult e && not e.TaskListEntry.removed],selfId = entryId},iworld)
						_	= (Error (dynamic readError,readError), iworld)
				(Error e,iworld)
					= (Error e, iworld)
					
    hasValueResult {TaskListEntry|lastEval=ValueResult _ _ _ _} = True
    hasValueResult _                                            = False //TODO: Figure out what to with exceptions on this level

	toItem {TaskListEntry|entryId,name,state,lastEval=ValueResult val _ _ _}
		= 	{taskId			= entryId
            ,listId         = listId
            ,name           = name
			,value			= deserialize val
			,attributes     = attributes
			,progressMeta	= progress
			}
	where
		(progress,attributes) = case state of
			DetachedState _ p a = (Just p,a)
			_					= (Nothing,'Data.Map'.newMap)
	
	deserialize NoValue	= NoValue
	deserialize (Value json stable) = maybe NoValue (\v -> Value v stable) (fromJSON json)

	write Void updates iworld
        = case updateInstanceMeta updates iworld of
            (Error e,iworld) = (Error e,iworld)
            (Ok Void,iworld) = updateListReduct updates iworld

    //Update the master index of task instance data
    updateInstanceMeta [] iworld = (Ok Void,iworld)
    updateInstanceMeta [(TaskId instanceNo 0,attr):updates] iworld
        = case 'SDS'.read (sdsFocus instanceNo taskInstanceMeta) iworld of
	        (Error e,iworld) = (Error e, iworld)
            (Ok meta,iworld) = case 'SDS'.write {TIMeta|meta & attributes=attr} (sdsFocus instanceNo taskInstanceMeta) iworld of
                (Error e,iworld) = (Error e, iworld)
                (Ok _,iworld)   = updateInstanceMeta updates iworld

    //Update the cached management meta in the task list reduct
    updateListReduct updates iworld=:{current=current=:{taskInstance,localLists}}
       | instanceNo == taskInstance
			= case 'Data.Map'.get listId localLists of
				Just entries    = (Ok (const True),{iworld & current = {current & localLists = 'Data.Map'.put listId (applyUpdates updates entries) localLists}})
                _               = (Error (dynamic readError,readError), iworld)
        | otherwise
			= case 'SDS'.read (taskInstanceReduct instanceNo) iworld of //TODO: Check if reading another shared during a write is ok??
				(Ok reduct, iworld)
					= case 'Data.Map'.get listId reduct.TIReduct.lists of					
                        Just entries
                            = appFst (fmap (const (const True))) ('SDS'.write {TIReduct|reduct & lists = 'Data.Map'.put listId (applyUpdates updates entries) reduct.TIReduct.lists} (taskInstanceReduct instanceNo) iworld)
						_	= (Error (dynamic readError,readError), iworld)
				(Error e,iworld)
					= (Error e, iworld)
    where
        applyUpdates [] entries = entries
        applyUpdates [(taskId,attr):updates] entries = applyUpdates updates (map (updateManagementMeta taskId attr) entries)

        updateManagementMeta taskId attr e=:{TaskListEntry|entryId,state=DetachedState s p _}
            | entryId == taskId     = {TaskListEntry|e & state = DetachedState s p attr}
                                    = e
        updateManagementMeta taskId attr e = e

