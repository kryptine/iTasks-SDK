implementation module iTasks._Framework.TaskStore

import StdOverloaded, StdBool, StdArray, StdTuple
from StdFunc import const, id, o
import Data.Maybe, Text, System.Time, Math.Random, Text.JSON, Data.Func, Data.Tuple, Data.List, Data.Error, System.FilePath, Data.Functor

import iTasks._Framework.IWorld, iTasks._Framework.TaskState, iTasks._Framework.Task, iTasks._Framework.Store
import iTasks._Framework.TaskEval, iTasks._Framework.Util, iTasks.UI.Definition
import iTasks._Framework.Serialization
import iTasks.API.Core.SDSCombinators, iTasks.API.Common.SDSCombinators

import qualified iTasks._Framework.SDS as SDS
from iTasks._Framework.SDS import :: SDSLensRead(..), :: SDSLensWrite(..), :: SDSLensNotify(..), :: RWShared(SDSDynamic), clearInstanceSDSRegistrations
import iTasks._Framework.SDSService
import iTasks._Framework.Client.Override

import qualified Data.Map as DM
import qualified Data.Queue as DQ
from Data.Queue import :: Queue(..)

//Derives required for storage of UI definitions
derive JSONEncode TaskResult, TaskEvalInfo, TIValue, ParallelTaskState, ParallelTaskChange, TIUIState
derive JSONEncode Queue, Event

derive JSONDecode TaskResult, TaskEvalInfo, TIValue, ParallelTaskState, ParallelTaskChange, TIUIState
derive JSONDecode Queue, Event

derive gDefault TIMeta
derive gEq ParallelTaskChange
derive gText ParallelTaskChange
derive class iTask InstanceFilter

//Master instance index on disk
taskInstanceIndex :: RWShared () [TIMeta] [TIMeta]
taskInstanceIndex = sdsFocus "instances" (cachedJSONFileStore NS_TASK_INSTANCES False False True (Just []))

//Next instance no counter
nextInstanceNo :: RWShared () Int Int
nextInstanceNo = sdsFocus "increment" (cachedJSONFileStore NS_TASK_INSTANCES False False True (Just 1))

taskInstanceIO :: RWShared InstanceNo (Maybe (!String,!DateTime)) (Maybe (!String,!DateTime))
taskInstanceIO = sdsLens "taskInstanceIO" (const ()) (SDSRead read) (SDSWrite write) (SDSNotifyConst notify) allInstanceIO
where
	read instanceNo m = Ok ('DM'.get instanceNo m)
	write instanceNo m (Just io) = Ok (Just ('DM'.put instanceNo io m))
	write instanceNo m Nothing = Ok (Just ('DM'.del instanceNo m))
	notify instanceNo _ 	= (==) instanceNo

	allInstanceIO :: RWShared () (Map InstanceNo (!String,!DateTime)) (Map InstanceNo (!String,DateTime)) 
	allInstanceIO = sdsFocus "io" (memoryStore NS_TASK_INSTANCES (Just 'DM'.newMap))

//Event queues of task instances
taskEvents :: RWShared () (Queue (InstanceNo,Event)) (Queue (InstanceNo,Event))
taskEvents = sdsFocus "events" (cachedJSONFileStore NS_TASK_INSTANCES False False True (Just 'DQ'.newQueue))

//Instance evaluation state
taskInstanceReduct :: RWShared InstanceNo TIReduct TIReduct
//taskInstanceReduct = sdsTranslate "taskInstanceReduct" (\t -> t +++> "-reduct") (memoryStore NS_TASK_INSTANCES Nothing)
taskInstanceReduct = sdsTranslate "taskInstanceReduct" (\t -> t +++> "-reduct") (cachedJSONFileStore NS_TASK_INSTANCES True False False Nothing)

//Last computed value for task instance
taskInstanceValue :: RWShared InstanceNo TIValue TIValue
taskInstanceValue = sdsTranslate "taskInstanceValue" (\t -> t +++> "-value") (cachedJSONFileStore NS_TASK_INSTANCES True False False Nothing)

//Local shared data
taskInstanceShares :: RWShared InstanceNo (Map TaskId JSONNode) (Map TaskId JSONNode)
taskInstanceShares = sdsTranslate "taskInstanceShares" (\t -> t +++> "-shares") (cachedJSONFileStore NS_TASK_INSTANCES True False False (Just 'DM'.newMap))

allUIChanges :: RWShared () (Map InstanceNo (Queue UIChange)) (Map InstanceNo (Queue UIChange)) 
allUIChanges = sdsFocus "allUIChanges" (memoryStore NS_TASK_INSTANCES (Just 'DM'.newMap))

taskInstanceUIChanges :: RWShared InstanceNo (Queue UIChange) (Queue UIChange) 
taskInstanceUIChanges = sdsLens "taskInstanceUIChanges" (const ()) (SDSRead read) (SDSWrite write) (SDSNotifyConst notify) allUIChanges
where
	read instanceNo uis = case 'DM'.get instanceNo uis of
		Just ui = Ok ui
		Nothing = Ok 'DQ'.newQueue

	write instanceNo uis ui = Ok (Just ('DM'.put instanceNo ui uis))
	notify instanceNo _ 	= (==) instanceNo

//Task instance parallel lists
taskInstanceParallelTaskLists :: RWShared InstanceNo (Map TaskId [ParallelTaskState]) (Map TaskId [ParallelTaskState])
taskInstanceParallelTaskLists = sdsTranslate "taskInstanceParallelLists" (\t -> t +++> "-tasklists") (cachedJSONFileStore NS_TASK_INSTANCES True False False (Just 'DM'.newMap))

newInstanceNo :: !*IWorld -> (!MaybeError TaskException InstanceNo,!*IWorld)
newInstanceNo iworld
	# (mbNewInstanceNo,iworld) = 'SDS'.read nextInstanceNo iworld
	= case mbNewInstanceNo of
		Ok instanceNo
			# (mbError,iworld) = 'SDS'.write (instanceNo + 1) nextInstanceNo iworld
            = case mbError of
                Ok _    = (Ok instanceNo,iworld)
                Error e = (Error e,iworld)
		Error e
            = (Error e,iworld)

newInstanceKey :: !*IWorld -> (!InstanceKey, !*IWorld)
newInstanceKey iworld=:{IWorld|random}
	= (toString (take 32 [toChar (97 +  abs (i rem 26)) \\ i <- random]) , {IWorld|iworld & random = drop 32 random})

newDocumentId :: !*IWorld -> (!DocumentId, !*IWorld)
newDocumentId iworld=:{IWorld|random}
	= (toString (take 32 [toChar (97 +  abs (i rem 26)) \\ i <- random]) , {IWorld|iworld & random = drop 32 random})
	
createClientTaskInstance :: !(Task a) !SessionId !InstanceNo !*IWorld -> *(!MaybeError TaskException TaskId, !*IWorld) |  iTask a
createClientTaskInstance task sessionId instanceNo iworld=:{server={buildID},current={taskTime},clocks={localDate,localTime}}
    //Create the initial instance data in the store
    # progress  = {InstanceProgress|value=None,attachedTo=[],firstEvent=Nothing,lastEvent=Nothing}
    # constants = {InstanceConstants|instanceKey="client",session=True,listId=TaskId 0 0,build=buildID,issuedAt=toDateTime localDate localTime}
    =            'SDS'.write (instanceNo, Just constants,Just progress,Just defaultValue) (sdsFocus instanceNo taskInstance) iworld
  `b` \iworld -> 'SDS'.write (createReduct defaultTonicOpts instanceNo task taskTime) (sdsFocus instanceNo taskInstanceReduct) iworld
  `b` \iworld -> 'SDS'.write (TIValue NoValue) (sdsFocus instanceNo taskInstanceValue) iworld
  `b` \iworld -> (Ok (TaskId instanceNo 0), iworld)

createTaskInstance :: !(Task a) !*IWorld -> (!MaybeError TaskException (!InstanceNo,InstanceKey),!*IWorld) | iTask a
createTaskInstance task iworld=:{server={buildID},current={taskTime},clocks={localDate,localTime}}
    # (mbInstanceNo,iworld) = newInstanceNo iworld
    # instanceNo            = fromOk mbInstanceNo
    # (instanceKey,iworld)  = newInstanceKey iworld
    # progress              = {InstanceProgress|value=None,attachedTo=[],firstEvent=Nothing,lastEvent=Nothing}
    # constants             = {InstanceConstants|instanceKey=instanceKey,session=True,listId=TaskId 0 0,build=buildID,issuedAt=toDateTime localDate localTime}
    =            'SDS'.write (instanceNo, Just constants,Just progress,Just defaultValue) (sdsFocus instanceNo taskInstance) iworld
  `b` \iworld -> 'SDS'.write (createReduct defaultTonicOpts instanceNo task taskTime) (sdsFocus instanceNo taskInstanceReduct) iworld
  `b` \iworld -> 'SDS'.write (TIValue NoValue) (sdsFocus instanceNo taskInstanceValue) iworld
  `b` \iworld -> (Ok (instanceNo,instanceKey), iworld)

(`b`) infixl 1 :: *(MaybeError e r, *st) (*st -> *(MaybeError e r`, *st)) -> *(MaybeError e r`, *st)
(`b`) (Ok _, st)    f = f st
(`b`) (Error e, st) _ = (Error e, st)

createDetachedTaskInstance :: !(Task a) !Bool !TaskEvalOpts !InstanceNo !TaskAttributes !TaskId !Bool !*IWorld -> (!MaybeError TaskException TaskId, !*IWorld) | iTask a
createDetachedTaskInstance task isTopLevel evalOpts instanceNo attributes listId refreshImmediate iworld=:{server={buildID},current={taskTime},clocks={localDate,localTime}}
    # (instanceKey,iworld) = newInstanceKey iworld
    # progress             = {InstanceProgress|value=None,attachedTo=[],firstEvent=Nothing,lastEvent=Nothing}
    # constants            = {InstanceConstants|instanceKey=instanceKey,session=False,listId=listId,build=buildID,issuedAt=toDateTime localDate localTime}
    =            'SDS'.write (instanceNo,Just constants,Just progress,Just attributes) (sdsFocus instanceNo taskInstance) iworld
  `b` \iworld -> 'SDS'.write (createReduct (if isTopLevel defaultTonicOpts evalOpts.tonicOpts) instanceNo task taskTime) (sdsFocus instanceNo taskInstanceReduct) iworld
  `b` \iworld -> 'SDS'.write (TIValue NoValue) (sdsFocus instanceNo taskInstanceValue) iworld
  `b` \iworld -> ( Ok (TaskId instanceNo 0)
				 , if refreshImmediate
                      (queueRefresh [(instanceNo,"First refresh of detached instance "<+++ instanceNo)] iworld)
                      iworld)

createReduct :: !TonicOpts !InstanceNo !(Task a) !TaskTime -> TIReduct | iTask a
createReduct tonicOpts instanceNo task taskTime
	= { TIReduct
      | task = toJSONTask task
      , tree = TCInit (TaskId instanceNo 0) 1
      , nextTaskNo = 1
      , nextTaskTime = 1
      , tasks = 'DM'.newMap
      , tonicRedOpts = tonicOpts
      }
where
	toJSONTask (Task eval) = Task eval`
	where
		eval` event repOpts tree iworld = case eval event repOpts tree iworld of
			(ValueResult val ts rep tree,iworld)	= (ValueResult (fmap toJSON val) ts rep tree, iworld)
			(ExceptionResult e,iworld)			    = (ExceptionResult e,iworld)

replaceTaskInstance :: !InstanceNo !(Task a) *IWorld -> (!MaybeError TaskException (), !*IWorld) | iTask a
replaceTaskInstance instanceNo task iworld=:{server={buildID},current={taskTime}}
    # (meta, iworld)        = 'SDS'.read (sdsFocus instanceNo taskInstance) iworld
    | isError meta          = (liftError meta, iworld)
    =            'SDS'.write (createReduct defaultTonicOpts instanceNo task taskTime) (sdsFocus instanceNo taskInstanceReduct) iworld
  `b` \iworld -> 'SDS'.write (TIValue NoValue) (sdsFocus instanceNo taskInstanceValue) iworld
  `b` \iworld -> let (_,Just constants,progress,attributes) = fromOk meta
                 in  'SDS'.write (instanceNo,Just {InstanceConstants|constants & build=buildID},progress,attributes) (sdsFocus instanceNo taskInstance) iworld
  `b` \iworld -> (Ok (), iworld)

deleteTaskInstance	:: !InstanceNo !*IWorld -> *(!MaybeError TaskException (), !*IWorld)
deleteTaskInstance instanceNo iworld
    //Delete all states
    # (mbe,iworld)    = deleteValue NS_TASK_INSTANCES (instanceNo +++> "-reduct") iworld
	| mbe =: (Error _) = (Error (exception (fromError mbe)),iworld)
    # (mbe,iworld)    = deleteValue NS_TASK_INSTANCES (instanceNo +++> "-value") iworld
	| mbe =: (Error _) = (Error (exception (fromError mbe)),iworld)
    # (mbe,iworld)    = deleteValue NS_TASK_INSTANCES (instanceNo +++> "-shares") iworld
	| mbe =: (Error _) = (Error (exception (fromError mbe)),iworld)
    # (mbe,iworld)    = deleteValue NS_TASK_INSTANCES (instanceNo +++> "-tasklists") iworld
	| mbe =: (Error _) = (Error (exception (fromError mbe)),iworld)
    # (mbe,iworld)    = 'SDS'.modify (\is -> ((),[i \\ i=:(no,_,_,_) <- is | no <> instanceNo])) (sdsFocus defaultValue filteredInstanceIndex) iworld
	| mbe =: (Error _) = (mbe,iworld)
    # (mbe,iworld)    = 'SDS'.modify (\(Queue f r) -> ((),Queue [e \\ e=:(no,_) <- f | no <> instanceNo] [e \\ e=:(no,_) <- r | no <> instanceNo])) taskEvents iworld
	| mbe =: (Error _) = (mbe,iworld)
    = (Ok (),iworld)

//Filtered interface to the instance index. This interface should always be used to access instance data
filteredInstanceIndex :: RWShared InstanceFilter [InstanceData] [InstanceData]
filteredInstanceIndex = sdsLens "filteredInstanceIndex" param (SDSRead read) (SDSWrite write) (SDSNotify notify) taskInstanceIndex
where
    param tfilter = ()

    read tfilter is = Ok (map (selectColumns tfilter) (selectRows tfilter is))

    write p is ws = Ok (Just (write` p is ws))
    where
        //Pairwise update (under the assumption that both lists are sorted by ascending instance number)
        write` p is [] = [i \\ i <- is | not (filterPredicate p i)] //Remove all items that match the filter but are not in write list
        write` p [] ws = [updateColumns p i w \\ w <- ws & i <- repeat defaultValue] //Add new items
        write` p [i=:{TIMeta|instanceNo}:is] [w=:(wNo,_,_,_):ws]
            | instanceNo == wNo     = [updateColumns p i w:write` p is ws] //Update the appropriate columns
            | filterPredicate p i   = write` p is [w:ws]    //If w is not the next element, it may be because it is outside the filter, if it isn't it is apparently deleted
                                    = [i:write` p is [w:ws]] //I was outside the filter, just leave it unchanged

    notify wfilter rs ws qfilter 
	    | not (overlappingColumns wfilter qfilter)      = False //If there are no overlapping columns, we definitely don't need to notify
	    | overlappingRows qfilter wfilter rs            = True  //If there are records that match both filters, we need to notify
        | matchingRows qfilter (newRows rs wfilter ws)  = True  //If there are new rows that the registered filter we need to notify
	    | otherwise                                     = False
    
    overlappingColumns x y
	    =    (x.InstanceFilter.includeConstants   && y.InstanceFilter.includeConstants)
	    || (x.InstanceFilter.includeProgress    && y.InstanceFilter.includeProgress)
	    || (x.InstanceFilter.includeAttributes  && y.InstanceFilter.includeAttributes)

    overlappingRows qfilter wfilter rs
	    = any (\r -> filterPredicate qfilter r && filterPredicate wfilter r) rs
    matchingRows qfilter rs
        = any (filterPredicate qfilter) rs

    newRows rs wfilter ws =  [updateColumns wfilter defaultValue w \\ w=:(no,_,_,_) <- ws | not (isMember no existingInstances)]
    where	
        existingInstances = [instanceNo\\ {TIMeta|instanceNo} <- rs]

    selectRows tfilter is = filter (filterPredicate tfilter) is
    selectColumns {InstanceFilter|includeConstants,includeProgress,includeAttributes} {TIMeta|instanceNo,instanceKey,listId,session,build,issuedAt,progress,attributes}
        # constants  = if includeConstants (Just {InstanceConstants|instanceKey=instanceKey,listId=listId,session=session,build=build,issuedAt=issuedAt}) Nothing
        # progress   = if includeProgress (Just progress) Nothing
        # attributes = if includeAttributes (Just attributes) Nothing
        = (instanceNo,constants,progress,attributes)

    updateColumns {InstanceFilter|includeConstants,includeProgress,includeAttributes} i (iNo,mbC,mbP,mbA)
        # i = if includeConstants (maybe i (\{InstanceConstants|instanceKey,listId,session,build,issuedAt}
                                            -> {TIMeta|i & instanceKey=instanceKey,listId=listId,session=session,build=build,issuedAt=issuedAt}) mbC) i
        # i = if includeProgress (maybe i (\progress -> {TIMeta|i & progress = progress}) mbP) i
        # i = if includeAttributes (maybe i (\attributes -> {TIMeta|i & attributes = attributes}) mbA) i
        = {TIMeta|i & instanceNo = iNo}

    filterPredicate {InstanceFilter|onlyInstanceNo,notInstanceNo,onlySession,matchAttribute} i
        =   (maybe True (\m -> isMember i.TIMeta.instanceNo m) onlyInstanceNo)
        &&  (maybe True (\m -> not (isMember i.TIMeta.instanceNo m)) notInstanceNo)
        &&  (maybe True (\m -> i.TIMeta.session == m) onlySession)
		&&  (maybe True (\(mk,mv) -> (maybe False ((==) mv) ('DM'.get mk i.TIMeta.attributes))) matchAttribute)

    notifyFun _ ws qfilter = any (filterPredicate qfilter) ws

//Filtered views on the instance index
taskInstance :: RWShared InstanceNo InstanceData InstanceData
taskInstance = sdsLens "taskInstance" param (SDSRead read) (SDSWriteConst write) (SDSNotifyConst notify) filteredInstanceIndex
where
    param no = {InstanceFilter|onlyInstanceNo=Just [no],notInstanceNo=Nothing,onlySession=Nothing,matchAttribute=Nothing
               ,includeConstants=True,includeProgress=True,includeAttributes=True}
    read no [data]  = Ok data
    read no _       = Error (exception ("Could not find task instance "<+++ no))
    write no data   = Ok (Just [data])
    notify no _     = (==) no

taskInstanceConstants :: ROShared InstanceNo InstanceConstants
taskInstanceConstants = sdsLens "taskInstanceConstants" param (SDSRead read) (SDSWriteConst write) (SDSNotifyConst notify) filteredInstanceIndex
where
    param no = {InstanceFilter|onlyInstanceNo=Just [no],notInstanceNo=Nothing,onlySession=Nothing,matchAttribute=Nothing
               ,includeConstants=True,includeProgress=False,includeAttributes=False}
    read no [(_,Just c,_,_)]    = Ok c
    read no _                   = Error (exception ("Could not find constants for task instance "<+++ no))
    write _ _                   = Ok Nothing
    notify _ _                  = const False

taskInstanceProgress :: RWShared InstanceNo InstanceProgress InstanceProgress
taskInstanceProgress = sdsLens "taskInstanceProgress" param (SDSRead read) (SDSWrite write) (SDSNotifyConst notify) filteredInstanceIndex
where
    param no = {InstanceFilter|onlyInstanceNo=Just [no],notInstanceNo=Nothing,onlySession=Nothing,matchAttribute=Nothing
               ,includeConstants=False,includeProgress=True,includeAttributes=False}
    read no [(_,_,Just p,_)]    = Ok p
    read no _                   = Error (exception ("Could not find progress for task instance "<+++ no))
    write no [(n,c,_,a)] p      = Ok (Just [(n,c,Just p,a)])
    write no _ _                = Error (exception ("Could not find progress for task instance "<+++ no))
    notify no _                 = (==) no

taskInstanceAttributes :: RWShared InstanceNo TaskAttributes TaskAttributes
taskInstanceAttributes = sdsLens "taskInstanceAttributes" param (SDSRead read) (SDSWrite write) (SDSNotifyConst notify) filteredInstanceIndex
where
    param no = {InstanceFilter|onlyInstanceNo=Just [no],notInstanceNo=Nothing,onlySession=Nothing,matchAttribute=Nothing
               ,includeConstants=False,includeProgress=False,includeAttributes=True}
    read no [(_,_,_,Just a)]    = Ok a
    read no _                   = Error (exception ("Could not find attributes for task instance "<+++ no))
    write no [(n,c,p,_)] a      = Ok (Just [(n,c,p,Just a)])
    write no _ _                = Error (exception ("Could not find attributes for task instance "<+++ no))
    notify no _                 = (==) no

//Top list share has no items, and is therefore completely polymorphic
topLevelTaskList :: RWShared TaskListFilter (!TaskId,![TaskListItem a]) [(!TaskId,!TaskAttributes)]
topLevelTaskList = sdsLens "topLevelTaskList" param (SDSRead read) (SDSWrite write) (SDSNotifyConst notify)
                     (sdsFocus filter filteredInstanceIndex >+| currentInstanceShare)
where
    param _ = ()
    filter = {InstanceFilter|onlyInstanceNo=Nothing,notInstanceNo=Nothing,onlySession=Just False,matchAttribute=Nothing
             ,includeConstants=True,includeProgress=True,includeAttributes=True}
    read _ (instances,curInstance) = Ok (TaskId 0 0, items)
    where
        items = [{TaskListItem|taskId = TaskId instanceNo 0, listId = listId
                 , detached = True, self = instanceNo == curInstance
                 , value = NoValue, progress = Just progress, attributes = attributes
                 } \\ (instanceNo,Just {InstanceConstants|listId},Just progress, Just attributes) <- instances]

    write _ _ [] = Ok Nothing
    write _ (instances,_) updates = Ok (Just (map (updateInstance updates) instances))
    where
        updateInstance updates (instanceNo,c,p,a) = (instanceNo,c,p,foldr updateAttributes a updates)
        where
            updateAttributes (TaskId targetNo 0,attrNew) attrOld = if (targetNo == instanceNo) (Just attrNew) attrOld
            updateAttributes _ attrOld = attrOld

    notify _ _ _ = True

//Evaluation state of instances
localShare :: RWShared TaskId a a | iTask a
localShare = sdsLens "localShare" param (SDSRead read) (SDSWrite write) (SDSNotifyConst notify) taskInstanceShares
where
    param (TaskId instanceNo _) = instanceNo
    read taskId shares = case 'DM'.get taskId shares of
        Just json = case fromJSON json of
            (Just r)    = Ok r
            Nothing     = Error (exception ("Failed to decode json of local share " <+++ taskId))
        Nothing
            = Error (exception ("Could not find local share " <+++ taskId))
    write taskId shares w = Ok (Just ('DM'.put taskId (toJSON w) shares))
    notify taskId _ = (==) taskId

derive gText ParallelTaskState

taskInstanceParallelTaskList :: RWShared (TaskId,TaskListFilter) [ParallelTaskState] [ParallelTaskState]
taskInstanceParallelTaskList = sdsLens "taskInstanceParallelTaskList" param (SDSRead read) (SDSWrite write) (SDSNotifyConst notify) taskInstanceParallelTaskLists
where
    param (TaskId instanceNo _,listFilter) = instanceNo
    read (taskId,listFilter) lists = case 'DM'.get taskId lists of
        Just list = Ok (filter (inFilter listFilter) list)
        Nothing = Error (exception ("Could not find parallel task list of " <+++ taskId))
    write (taskId,listFilter) lists w
		= Ok (Just ('DM'.put taskId (merge listFilter (fromMaybe [] ('DM'.get taskId lists)) w) lists))

    notify (taskId,listFilter) states (regTaskId,regListFilter)
        # states = filter (inFilter listFilter) states //Ignore the states outside our filter
        //Different list, so eliminate
        | taskId <> regTaskId = False
        //No overlap in columns: eliminate
        | not ((listFilter.TaskListFilter.includeValue && regListFilter.TaskListFilter.includeValue)
          || (listFilter.TaskListFilter.includeAttributes && regListFilter.TaskListFilter.includeAttributes)
          || (listFilter.TaskListFilter.includeProgress && regListFilter.TaskListFilter.includeProgress)) = False
        //Check if the written records match the registered filter
        | maybe False (\taskIds -> all (\t -> not (isMember t taskIds)) [taskId \\{ParallelTaskState|taskId} <- states]) regListFilter.onlyTaskId
            = False
        | maybe False (\indices -> all (\i -> not (isMember i indices)) [index \\{ParallelTaskState|index} <- states]) regListFilter.onlyIndex
            = False
        //Looks like we can't eliminate, so we may need to notify
        | otherwise
            = True

    inFilter {TaskListFilter|onlyTaskId,onlyIndex} {ParallelTaskState|taskId,index}
        =  maybe True (\taskIds -> isMember taskId taskIds) onlyTaskId
        && maybe True (\indices -> isMember index indices) onlyIndex

	//ASSUMPTION: BOTH LISTS ARE SORTED BY TASK ID
	merge listFilter [o:os] [n:ns]
		| o.ParallelTaskState.taskId == n.ParallelTaskState.taskId //Potential update
			| inFilter listFilter o = [n:merge listFilter os ns] //Only update the item if it matches the filter
			| otherwise 			= [o:merge listFilter os ns]
		| o.ParallelTaskState.taskId < n.ParallelTaskState.taskId //The taskId of the old item is not in the written set
			| inFilter listFilter o = merge listFilter os [n:ns]  	//The old item was in the filter, so it was removed
			| otherwise 			= [o:merge listFilter os [n:ns]] //The old item was not in the filter, so it is ok that is not in the written list
		| otherwise
			| inFilter listFilter n = [n:merge listFilter [o:os] ns]
			| otherwise 			= merge listFilter [o:os] ns

	merge listFilter [] ns			= filter (inFilter listFilter) ns //All new elements are only added if they are within the filter
	merge listFilter os [] 			= filter (not o inFilter listFilter) os //Only keep old elements if they were outside the filter

taskInstanceParallelTaskListItem :: RWShared (TaskId,TaskId,Bool) ParallelTaskState ParallelTaskState
taskInstanceParallelTaskListItem = sdsLens "taskInstanceParallelTaskListItem" param (SDSRead read) (SDSWrite write) (SDSNotifyConst notify) taskInstanceParallelTaskList
where
    //In this SDS the include value and include attributes flags are used to indicate what is written for notification
    //During a read the whole ParallelTaskState record is used
    param (listId,taskId,includeValue)
        = (listId,{TaskListFilter|onlyIndex=Nothing,onlyTaskId=Just [taskId],onlySelf=False,includeValue=includeValue,includeAttributes=False,includeProgress=False})
    read p=:(listId,taskId,_) [] = Error (exception ("Could not find parallel task " <+++ taskId <+++ " in list " <+++ listId))
    read p=:(_,taskId,_) [x:xs] = if (x.ParallelTaskState.taskId == taskId) (Ok x) (read p xs)
    write (_,taskId,_) list pts = Ok (Just [if (x.ParallelTaskState.taskId == taskId) pts x \\ x <- list])
    notify (listId,taskId,_) _ = (==) taskId o snd3

taskInstanceEmbeddedTask :: RWShared TaskId (Task a) (Task a) | iTask a
taskInstanceEmbeddedTask = sdsLens "taskInstanceEmbeddedTask" param (SDSRead read) (SDSWrite write) (SDSNotifyConst notify) taskInstanceReduct
where
    param (TaskId instanceNo _) = instanceNo
    read taskId {TIReduct|tasks} = case fmap unwrapTask ('DM'.get taskId tasks) of
        Just task = Ok task
        _         = Error (exception ("Could not find embedded task " <+++ taskId))
    write taskId r=:{TIReduct|tasks} w = Ok (Just {TIReduct|r & tasks = 'DM'.put taskId (dynamic w :: Task a^) tasks})
    notify taskId _ = (==) taskId

parallelTaskList :: RWShared (!TaskId,!TaskId,!TaskListFilter) (!TaskId,![TaskListItem a]) [(!TaskId,!TaskAttributes)] | iTask a
parallelTaskList
    = sdsSequence "parallelTaskList" param2 read (SDSWriteConst write1) (SDSWriteConst write2) filteredTaskStates filteredInstanceIndex
where
    filteredTaskStates
        = sdsLens "parallelTaskListStates" param (SDSRead read) (SDSWrite write) (SDSNotifyConst notify) taskInstanceParallelTaskList
    where
        param (listId,selfId,listFilter=:{TaskListFilter|onlySelf,onlyTaskId})
            = (listId,{TaskListFilter|listFilter & onlyTaskId = if onlySelf (Just [selfId:fromMaybe [] onlyTaskId]) onlyTaskId})

        read (listId,selfId,listFilter) states  = Ok (listId,items)
        where
            items = [{TaskListItem|taskId = taskId, listId = listId
                     , detached = detached, self = taskId == selfId
                     , value = decode value, progress = Nothing, attributes = attributes
                     } \\ {ParallelTaskState|taskId,detached,attributes,value,change} <- states | change =!= Just RemoveParallelTask]

            decode NoValue	= NoValue
            decode (Value json stable) = maybe NoValue (\v -> Value v stable) (fromJSON json)

        write (listId,selfId,listFilter) states []                              = Ok Nothing
        write (listId,selfId,{TaskListFilter|includeAttributes=False}) states _ = Ok Nothing
        write (listId,selfId,listFilter) states [(t,a):updates]
            # states = [if (taskId == t) {ParallelTaskState|pts & attributes = a} pts \\ pts=:{ParallelTaskState|taskId} <- states]
            = write (listId,selfId,listFilter) states updates

        notify (listId,_,_) states (regListId,_,_) = regListId == listId //Only check list id, the listFilter is checked one level up


    param2 _ (listId,items) = {InstanceFilter|onlyInstanceNo=Just [instanceNo \\ {TaskListItem|taskId=(TaskId instanceNo _),detached} <- items | detached],notInstanceNo=Nothing
                     ,onlySession=Nothing, matchAttribute=Nothing, includeConstants = False, includeAttributes = True,includeProgress = True}

    read ((listId,items),detachedInstances)
        # detachedProgress = 'DM'.fromList [(TaskId instanceNo 0,progress) \\ (instanceNo,_,Just progress,_) <- detachedInstances]
        # detachedAttributes= 'DM'.fromList [(TaskId instanceNo 0,attributes) \\ (instanceNo,_,_,Just attributes) <- detachedInstances]
        = (listId,[{TaskListItem|item & progress = 'DM'.get taskId detachedProgress
                                , attributes = if detached (fromMaybe 'DM'.newMap ('DM'.get taskId detachedAttributes)) attributes}
                  \\ item=:{TaskListItem|taskId,detached,attributes} <- items])

    write1 p w = Ok (Just w)
    write2 p w = Ok Nothing //TODO: Write attributes of detached instances

queueEvent :: !InstanceNo !Event !*IWorld -> *IWorld
//Special case for refresh events, queue only if the instance has no events in the queue yet
queueEvent instanceNo event=:(RefreshEvent r) iworld 
	# (_,iworld) = 'SDS'.modify (queueIfNotScheduled instanceNo event) taskEvents iworld
	= iworld
where
	queueIfNotScheduled instanceNo event q=:('DQ'.Queue front back)
		| isMember instanceNo (map fst (front ++ back)) = ((),q)
														= ((),'DQ'.enqueue (instanceNo,event) q)
//Standard case...
queueEvent instanceNo event iworld 
	# (_,iworld) = 'SDS'.modify (\q -> ((),'DQ'.enqueue (instanceNo,event) q)) taskEvents iworld
	= iworld

queueRefresh :: ![(InstanceNo,String)] !*IWorld -> *IWorld
queueRefresh instances iworld
    //Clear the instance's share change registrations, we are going to evaluate anyway
	# iworld	= clearInstanceSDSRegistrations (map fst instances) iworld
	# iworld 	= foldl (\w (i,r) -> queueEvent i (RefreshEvent r) w) iworld instances
	= iworld

dequeueEvent :: !*IWorld -> (!Maybe (InstanceNo,Event),!*IWorld)
dequeueEvent iworld
	= case 'SDS'.modify 'DQ'.dequeue taskEvents iworld of
		(Ok mbEvent,iworld) 	= (mbEvent,iworld)
		(Error (_,e),iworld) 	= (Nothing,iworld) //TODO handle errors

queueUIChange :: !InstanceNo !UIChange !*IWorld -> *IWorld
queueUIChange instanceNo change iworld
	# (_,iworld) = 'SDS'.modify (\q -> ((),'DQ'.enqueue change q)) (sdsFocus instanceNo taskInstanceUIChanges) iworld
	= iworld

queueUIChanges :: !InstanceNo ![UIChange] !*IWorld -> *IWorld
queueUIChanges instanceNo changes iworld
	# (_,iworld) = 'SDS'.modify (\q -> ((),enqueueAll changes q)) (sdsFocus instanceNo taskInstanceUIChanges) iworld
	= iworld
where
	enqueueAll [] q = q
	enqueueAll [x:xs] q = enqueueAll xs ('DQ'.enqueue x q)

createDocument :: !String !String !String !*IWorld -> (!MaybeError FileError Document, !*IWorld)
createDocument name mime content iworld
	# (documentId, iworld)	= newDocumentId iworld
	# document				= {Document|documentId = documentId, contentUrl = "/download/"+++documentId, name = name, mime = mime, size = size content}
	# iworld				= blobStoreWrite NS_DOCUMENT_CONTENT (documentId +++ "-data") content iworld
	# (_,iworld)			= 'SDS'.write document (sdsFocus documentId (sdsTranslate "document_meta" (\d -> d +++ "-meta") (jsonFileStore NS_DOCUMENT_CONTENT  False False Nothing))) iworld	
	= (Ok document,iworld)
	
loadDocumentContent	:: !DocumentId !*IWorld -> (!Maybe String, !*IWorld)
loadDocumentContent documentId iworld
	= case blobStoreRead NS_DOCUMENT_CONTENT (documentId +++ "-data") iworld of
        (Ok content,iworld) = (Just content,iworld)
        (Error e,iworld)    = (Nothing,iworld)

loadDocumentMeta :: !DocumentId !*IWorld -> (!Maybe Document, !*IWorld)
loadDocumentMeta documentId iworld
	= case ('SDS'.read (sdsFocus documentId (sdsTranslate "document_meta" (\d -> d+++"-meta") (jsonFileStore NS_DOCUMENT_CONTENT False False Nothing))) iworld) of
        (Ok doc,iworld)     = (Just doc,iworld)
        (Error e,iworld)    = (Nothing,iworld)

documentLocation :: !DocumentId !*IWorld -> (!FilePath,!*IWorld)
documentLocation documentId iworld=:{server={buildID,paths={dataDirectory}}}
	= (dataDirectory </>"stores"</> NS_DOCUMENT_CONTENT </> (documentId +++ "-data.txt"),iworld)

//OBSOLETE
exposedShare :: !String -> RWShared p r w | iTask r & iTask w & TC r & TC w & TC p & JSONEncode{|*|} p
exposedShare url = SDSDynamic f
where
	f _ iworld=:{exposedShares}
        = case 'DM'.get url exposedShares of
		    Nothing
			    = (Ok ('SDS'.createReadWriteSDS "exposedShare" url rread rwrite), iworld)
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

