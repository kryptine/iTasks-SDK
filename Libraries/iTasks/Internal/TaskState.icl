implementation module iTasks.Internal.TaskState

import StdEnv
import Data.Maybe, Data.Either, Text, System.Time, Math.Random, Text.GenJSON, Data.Func, Data.Tuple, Data.List, Data.Error, System.FilePath, Data.Functor, Data.Set.GenJSON

import Text.GenJSON, StdString, Data.Func, Data.GenEq, Data.Maybe, Data.Functor, Data.Map.GenJSON, Data.Set.GenJSON
import iTasks.UI.Definition, iTasks.UI.Layout
import iTasks.WF.Definition
import iTasks.WF.Derives
from iTasks.WF.Combinators.Core import :: AttachmentStatus

from iTasks.Internal.Task	import exception
from iTasks.Util.DeferredJSON import :: DeferredJSON
import iTasks.Internal.Serialization, iTasks.Internal.Generic.Visualization

import iTasks.Engine
import iTasks.Internal.IWorld, iTasks.Internal.Task, iTasks.Internal.Store
import iTasks.Internal.TaskEval, iTasks.Internal.Util, iTasks.UI.Definition
import iTasks.Internal.TaskIO
import iTasks.Internal.Serialization
import iTasks.Internal.Generic.Defaults
import iTasks.Internal.Generic.Visualization
import iTasks.Util.DeferredJSON

import iTasks.UI.Tune
import iTasks.UI.Layout.Default

import qualified iTasks.Internal.SDS as SDS
from iTasks.SDS.Definition import :: SDSLensRead(..), :: SDSLensWrite(..), :: SDSLensNotify(..)
import iTasks.SDS.Combinators.Core, iTasks.SDS.Combinators.Common
import iTasks.SDS.Sources.Store
import iTasks.Internal.DynamicUtil
import iTasks.Internal.SDSService
import iTasks.Internal.Generic.Defaults
import iTasks.WF.Combinators.Core
import iTasks.WF.Combinators.Common
import iTasks.WF.Derives
import iTasks.Extensions.Document

from Data.Map import instance Functor (Map k)
import qualified Data.Map as DM
import Data.Map.GenJSON
import qualified Data.Set as DS
import Data.Set.GenJSON
import qualified Data.Queue as DQ
from Data.Queue import :: Queue(..)
from Control.Applicative import class Alternative(<|>)
import Data.GenEq

derive JSONEncode TaskMeta, TIType, TIValue, TIReduct, TaskChange, TaskResult, TaskEvalInfo
derive JSONDecode TaskMeta, TIType, TIValue, TIReduct, TaskChange, TaskResult, TaskEvalInfo

derive gDefault InstanceProgress, TIType, TaskId, ValueStatus, InstanceFilter

gDefault{|TaskMeta|}
	= {taskId= TaskId 0 0,instanceType=gDefault{|*|},build="",createdAt=gDefault{|*|},valuestatus=gDefault{|*|},attachedTo=[],instanceKey=Nothing
	  ,firstEvent=Nothing,lastEvent=Nothing,taskAttributes='DM'.newMap,managementAttributes='DM'.newMap,unsyncedAttributes = 'DS'.newSet
	  ,change = Nothing, initialized = False}

derive gEq TaskChange
derive gText TaskChange, Set

derive class iTask InstanceFilter

mergeTaskAttributes :: !(!TaskAttributes,!TaskAttributes) -> TaskAttributes
mergeTaskAttributes (explicit,implicit) = 'DM'.union explicit implicit

//Unfiltered administration
rawTaskIndex         = storeShare NS_TASK_INSTANCES False InJSONFile (Just [])
rawTaskNoCounter     = storeShare NS_TASK_INSTANCES False InJSONFile (Just 1)

rawInstanceIO        = storeShare NS_TASK_INSTANCES False InMemory (Just 'DM'.newMap)

rawInstanceReduct         = mbStoreShare NS_TASK_INSTANCES True InDynamicFile
rawInstanceValue          = mbStoreShare NS_TASK_INSTANCES True InDynamicFile
rawInstanceShares         = mbStoreShare NS_TASK_INSTANCES True InDynamicFile
rawInstanceParallels      = mbStoreShare NS_TASK_INSTANCES True InDynamicFile
rawInstanceParallelValues = mbStoreShare NS_TASK_INSTANCES True InDynamicFile

//Master instance index
taskInstanceIndex :: SimpleSDSLens [TaskMeta]
taskInstanceIndex = sdsFocus "instances" rawTaskIndex

//Next instance no counter
nextInstanceNo :: SimpleSDSLens Int
nextInstanceNo = sdsFocus "increment" rawTaskNoCounter

taskInstanceIO :: SDSLens InstanceNo (Maybe (!String,!Timespec)) (Maybe (!String,!Timespec))
taskInstanceIO = sdsLens "taskInstanceIO" (const ()) (SDSRead read) (SDSWrite write) (SDSNotifyConst notify) (Just r1) allInstanceIO
where
	read instanceNo m = Ok ('DM'.get instanceNo m)
	write instanceNo m (Just io) = Ok (Just ('DM'.put instanceNo io m))
	write instanceNo m Nothing = Ok (Just ('DM'.del instanceNo m))
	notify instanceNo _ _ 	= (==) instanceNo
  r1 instanceNo ws = Ok ('DM'.get instanceNo ws)

allInstanceIO :: SimpleSDSLens (Map InstanceNo (!String,Timespec))
allInstanceIO = sdsFocus "io" rawInstanceIO

//Instance evaluation state
taskInstanceReduct :: SDSLens InstanceNo (Maybe TIReduct) (Maybe TIReduct)
taskInstanceReduct = sdsTranslate "taskInstanceReduct" (\t -> t +++> "-reduct") rawInstanceReduct

//Last computed value for task instance
taskInstanceValue :: SDSLens InstanceNo (Maybe TIValue) (Maybe TIValue)
taskInstanceValue = sdsTranslate "taskInstanceValue" (\t -> t +++> "-value") rawInstanceValue

//Local shared data
taskInstanceShares :: SDSLens InstanceNo (Maybe (Map TaskId DeferredJSON)) (Maybe (Map TaskId DeferredJSON))
taskInstanceShares = sdsTranslate "taskInstanceShares" (\t -> t +++> "-shares") rawInstanceShares

//Task instance parallel lists
taskInstanceParallelTaskLists :: SDSLens InstanceNo (Maybe (Map TaskId [TaskMeta])) (Maybe (Map TaskId [TaskMeta]))
taskInstanceParallelTaskLists = sdsTranslate "taskInstanceParallelLists" (\t -> t +++> "-tasklists") rawInstanceParallels

taskInstanceParallelValues :: SDSLens InstanceNo (Maybe (Map TaskId (Map TaskId (TaskValue DeferredJSON)))) (Maybe (Map TaskId (Map TaskId (TaskValue DeferredJSON))))
taskInstanceParallelValues = sdsTranslate "taskInstanceParallelValues" (\t -> t +++> "-parvalues") rawInstanceParallelValues

newInstanceNo :: !*IWorld -> (!MaybeError TaskException InstanceNo,!*IWorld)
newInstanceNo iworld
	# (mbNewInstanceNo,iworld) = 'SDS'.read nextInstanceNo 'SDS'.EmptyContext iworld
	= case mbNewInstanceNo of
		Ok ('SDS'.ReadingDone instanceNo)
			# (mbError,iworld) = 'SDS'.write (instanceNo + 1) nextInstanceNo 'SDS'.EmptyContext iworld
			= case mbError of
				Ok _    = (Ok instanceNo,iworld)
				Error e = (Error e,iworld)
		Error e
			= (Error e,iworld)

newInstanceKey :: !*IWorld -> (!InstanceKey, !*IWorld)
newInstanceKey iworld = generateRandomString 32 iworld

createClientTaskInstance :: !(Task a) !String !InstanceNo !*IWorld -> *(!MaybeError TaskException TaskId, !*IWorld) |  iTask a
createClientTaskInstance task sessionId instanceNo iworld=:{options={appVersion},current={taskTime},clock}
    //Create the initial instance data in the store
    # progress  = {InstanceProgress|value=Unstable,instanceKey=Nothing,attachedTo=[],firstEvent=Nothing,lastEvent=Nothing}
    # constants = {InstanceConstants|type=SessionInstance,build=appVersion,issuedAt=clock}
    =            'SDS'.write (TaskId instanceNo 0, Just constants,Just progress,Just defaultValue) (sdsFocus instanceNo taskInstance) 'SDS'.EmptyContext iworld
  `b` \iworld -> 'SDS'.write (Just (createReduct instanceNo task taskTime)) (sdsFocus instanceNo taskInstanceReduct) 'SDS'.EmptyContext iworld
  `b` \iworld -> 'SDS'.write (Just (TIValue NoValue)) (sdsFocus instanceNo taskInstanceValue) 'SDS'.EmptyContext iworld
  `b` \iworld -> (Ok (TaskId instanceNo 0), iworld)

createSessionTaskInstance :: !(Task a) !TaskAttributes !*IWorld -> (!MaybeError TaskException (!InstanceNo,InstanceKey),!*IWorld) | iTask a
createSessionTaskInstance task attributes iworld=:{options={appVersion,autoLayout},current={taskTime},clock}
	# task = if autoLayout (ApplyLayout defaultSessionLayout @>> task) task
    # (mbInstanceNo,iworld) = newInstanceNo iworld
    # (Ok instanceNo,iworld) = newInstanceNo iworld
    # (instanceKey,iworld)  = newInstanceKey iworld
    # progress              = {InstanceProgress|value=Unstable,instanceKey=Just instanceKey,attachedTo=[],firstEvent=Nothing,lastEvent=Nothing}
    # constants             = {InstanceConstants|type=SessionInstance,build=appVersion,issuedAt=clock}
    =            'SDS'.write (TaskId instanceNo 0, Just constants,Just progress,Just (attributes,'DM'.newMap)) (sdsFocus instanceNo taskInstance) 'SDS'.EmptyContext iworld
  `b` \iworld -> 'SDS'.write (Just (createReduct instanceNo task taskTime)) (sdsFocus instanceNo taskInstanceReduct) 'SDS'.EmptyContext iworld
  `b` \iworld -> 'SDS'.write (Just (TIValue NoValue)) (sdsFocus instanceNo taskInstanceValue) 'SDS'.EmptyContext iworld
  `b` \iworld -> (Ok (instanceNo,instanceKey), iworld)

createStartupTaskInstance :: !(Task a) !TaskAttributes !*IWorld -> (!MaybeError TaskException InstanceNo, !*IWorld) | iTask a
createStartupTaskInstance task attributes iworld=:{options={appVersion,autoLayout},current={taskTime},clock}
    # (Ok instanceNo,iworld) = newInstanceNo iworld
    # progress              = {InstanceProgress|value=Unstable,instanceKey=Nothing,attachedTo=[],firstEvent=Nothing,lastEvent=Nothing}
    # constants             = {InstanceConstants|type=StartupInstance,build=appVersion,issuedAt=clock}
    =            'SDS'.write (TaskId instanceNo 0, Just constants,Just progress,Just (attributes,'DM'.newMap)) (sdsFocus instanceNo taskInstance) 'SDS'.EmptyContext iworld
  `b` \iworld -> 'SDS'.write (Just (createReduct instanceNo task taskTime)) (sdsFocus instanceNo taskInstanceReduct) 'SDS'.EmptyContext iworld
  `b` \iworld -> 'SDS'.write (Just (TIValue NoValue)) (sdsFocus instanceNo taskInstanceValue) 'SDS'.EmptyContext iworld
  `b` \iworld -> (Ok instanceNo, queueEvent instanceNo ResetEvent iworld)

(`b`) infixl 1 :: *(MaybeError e r, *st) (*st -> *(MaybeError e r`, *st)) -> *(MaybeError e r`, *st)
(`b`) (Ok _, st)    f = f st
(`b`) (Error e, st) _ = (Error e, st)

createDetachedTaskInstance :: !(Task a) !Bool !TaskEvalOpts !InstanceNo !TaskAttributes !TaskId !Bool !*IWorld -> (!MaybeError TaskException TaskId, !*IWorld) | iTask a
createDetachedTaskInstance task isTopLevel evalOpts instanceNo attributes listId refreshImmediate iworld=:{options={appVersion,autoLayout},current={taskTime},clock}
	# task = if autoLayout (ApplyLayout defaultSessionLayout @>> task) task
    # (instanceKey,iworld) = newInstanceKey iworld
	# mbListId             = if (listId == TaskId 0 0) Nothing (Just listId)
    # progress             = {InstanceProgress|value=Unstable,instanceKey=Just instanceKey,attachedTo=[],firstEvent=Nothing,lastEvent=Nothing}
    # constants            = {InstanceConstants|type=PersistentInstance mbListId,build=appVersion,issuedAt=clock}
    =            'SDS'.write (TaskId instanceNo 0,Just constants,Just progress,Just (attributes,'DM'.newMap)) (sdsFocus instanceNo taskInstance) 'SDS'.EmptyContext iworld
  `b` \iworld -> 'SDS'.write (Just (createReduct instanceNo task taskTime)) (sdsFocus instanceNo taskInstanceReduct) 'SDS'.EmptyContext iworld
  `b` \iworld -> 'SDS'.write (Just (TIValue NoValue)) (sdsFocus instanceNo taskInstanceValue) 'SDS'.EmptyContext iworld
  `b` \iworld -> ( Ok (TaskId instanceNo 0)
				 , if refreshImmediate
					  (queueEvent instanceNo ResetEvent iworld)
					  iworld)

createReduct :: !InstanceNo !(Task a) !TaskTime -> TIReduct | iTask a
createReduct instanceNo task taskTime
	= { TIReduct
	  | task = task @ DeferredJSON
	  , nextTaskNo = 1
	  , nextTaskTime = 1
	  , tasks = 'DM'.newMap
	  }

replaceTaskInstance :: !InstanceNo !(Task a) *IWorld -> (!MaybeError TaskException (), !*IWorld) | iTask a
replaceTaskInstance instanceNo task iworld=:{options={appVersion},current={taskTime}}
	# (meta, iworld)        = 'SDS'.read (sdsFocus instanceNo taskInstance) 'SDS'.EmptyContext iworld
	| isError meta          = (liftError meta, iworld)
	=            'SDS'.write (Just (createReduct instanceNo task taskTime)) (sdsFocus instanceNo taskInstanceReduct) 'SDS'.EmptyContext iworld
  `b` \iworld -> 'SDS'.write (Just (TIValue NoValue)) (sdsFocus instanceNo taskInstanceValue) 'SDS'.EmptyContext iworld
  `b` \iworld -> let (_,Just constants,progress,attributes) ='SDS'.directResult (fromOk meta)
				 in  'SDS'.write (TaskId instanceNo 0,Just {InstanceConstants|constants & build=appVersion},progress,attributes) (sdsFocus instanceNo taskInstance) 'SDS'.EmptyContext iworld
  `b` \iworld -> (Ok (), iworld)

deleteTaskInstance	:: !InstanceNo !*IWorld -> *(!MaybeError TaskException (), !*IWorld)
deleteTaskInstance instanceNo iworld=:{IWorld|options={EngineOptions|persistTasks}}
	//Delete in index
	# taskFilter = {defaultValue & includeSessions = True, includeDetached = True, includeStartup = True}
	# (mbe,iworld)    = 'SDS'.modify (\is -> [i \\ i=:(taskId,_,_,_) <- is | taskId <> TaskId instanceNo 0])
		(sdsFocus taskFilter filteredInstanceIndex) 'SDS'.EmptyContext iworld
	| mbe =: (Error _) = (toME mbe,iworld)
	//Remove all edit/action/edit events from the queue
	# iworld = clearEvents instanceNo iworld
	//Queue a final destroy event
	# iworld = queueEvent instanceNo DestroyEvent iworld
	= (Ok (),iworld)
  where
	toME (Ok ('SDS'.ModifyingDone _)) = Ok ()
	toME (Error e) = (Error e)

//Filtered interface to the instance index. This interface should always be used to access instance data
filteredInstanceIndex :: SDSLens InstanceFilter [InstanceData] [InstanceData]
filteredInstanceIndex
	= sdsLens "filteredInstanceIndex" param (SDSRead read) (SDSWrite write) (SDSNotify notify) Nothing taskInstanceIndex
where
    param tfilter = ()

    read tfilter is = Ok (map (selectColumns tfilter) (selectRows tfilter is))

    write p is ws = Ok (Just (write` p is ws))
    where
        //Pairwise update (under the assumption that both lists are sorted by ascending instance number)
        write` p is [] = [i \\ i <- is | not (filterPredicate p i)] //Remove all items that match the filter but are not in write list
        write` p [] ws = [updateColumns p i w \\ w <- ws & i <- repeat defaultValue] //Add new items
        write` p [i=:{TaskMeta|taskId}:is] [w=:(wNo,_,_,_):ws]
            | taskId == wNo         = [updateColumns p i w:write` p is ws] //Update the appropriate columns
            | filterPredicate p i   = write` p is [w:ws]    //If w is not the next element, it may be because it is outside the filter, if it isn't it is apparently deleted
                                    = [i:write` p is [w:ws]] //I was outside the filter, just leave it unchanged

    notify wfilter rs ws ts qfilter
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

    newRows rs wfilter ws =  [updateColumns wfilter defaultValue w \\ w=:(taskId,_,_,_) <- ws | not (isMember taskId existingInstances)]
    where	
        existingInstances = [taskId \\ {TaskMeta|taskId} <- rs]

    selectRows tfilter is = filter (filterPredicate tfilter) is
    selectColumns {InstanceFilter|includeConstants,includeProgress,includeAttributes}
		{TaskMeta|taskId,instanceType,build,createdAt,valuestatus,attachedTo,instanceKey,firstEvent,lastEvent,taskAttributes,managementAttributes}
		# listId = case instanceType of
			(TIPersistent _ (Just listId)) = listId
			_                              = TaskId 0 0
		# type = case instanceType of
			(TIStartup) = StartupInstance
			(TISession _) = SessionInstance
			(TIPersistent _ mbListId) = PersistentInstance mbListId

        # constants  = if includeConstants (Just {InstanceConstants|type=type,build=build,issuedAt=createdAt}) Nothing
        # progress   = if includeProgress (Just {InstanceProgress|value=valuestatus,attachedTo=attachedTo,instanceKey=instanceKey,firstEvent=firstEvent,lastEvent=lastEvent}) Nothing
        # attributes = if includeAttributes (Just (managementAttributes,taskAttributes)) Nothing
        = (taskId,constants,progress,attributes)

    updateColumns {InstanceFilter|includeConstants,includeProgress,includeAttributes} i (iNo,mbC,mbP,mbA)
        # i = if includeConstants (maybe i (\{InstanceConstants|type,build,issuedAt}
                                            -> {TaskMeta|i & instanceType = instanceType i type mbP ,build=build,createdAt=issuedAt}) mbC) i
        # i = if includeProgress (maybe i (\{InstanceProgress|value,attachedTo,instanceKey,firstEvent,lastEvent}-> {TaskMeta|i & valuestatus=value,attachedTo=attachedTo,instanceKey=instanceKey,firstEvent=firstEvent,lastEvent=lastEvent}) mbP) i
        # i = if includeAttributes (maybe i (\(managementAttributes,taskAttributes) ->
			{TaskMeta|i & managementAttributes = managementAttributes, taskAttributes = taskAttributes}) mbA) i
        = {TaskMeta|i & taskId = iNo}
	where
		instanceType _ (StartupInstance) _ = TIStartup
		instanceType _ (SessionInstance) (Just {InstanceProgress|instanceKey=Just key}) = TISession key
		instanceType _ (PersistentInstance mbListId) (Just {InstanceProgress|instanceKey=Just key}) = TIPersistent key mbListId

		instanceType {instanceType} _ _ = instanceType

    filterPredicate {InstanceFilter|onlyInstanceNo,notInstanceNo,includeSessions,includeDetached,includeStartup,matchAttribute} i
        =   (maybe True (\m -> isMember i.TaskMeta.taskId m) onlyInstanceNo)
        &&  (maybe True (\m -> not (isMember i.TaskMeta.taskId m)) notInstanceNo)
        &&  ((includeSessions && i.instanceType =: (TISession _)) ||
		     (includeDetached && i.instanceType =: (TIPersistent _ _)) ||
		     (includeStartup && i.instanceType =: (TIStartup))
		    )
		&&  (maybe True (\(mk,mv) -> (maybe False ((==) mv) ('DM'.get mk i.TaskMeta.taskAttributes) || maybe False ((==) mv) ('DM'.get mk i.TaskMeta.managementAttributes)  ) ) matchAttribute)

	notifyFun _ ws qfilter = any (filterPredicate qfilter) ws

//Filtered views on the instance index
taskInstance :: SDSLens InstanceNo (InstanceData) (InstanceData)
taskInstance = sdsLens "taskInstance" param (SDSRead read) (SDSWriteConst write) (SDSNotifyConst notify) (Just \p ws -> read p ws) filteredInstanceIndex
where
	param no = {InstanceFilter|onlyInstanceNo=Just [TaskId no 0],notInstanceNo=Nothing,includeSessions=True,includeDetached=True,includeStartup=True,matchAttribute=Nothing
			   ,includeConstants=True,includeProgress=True,includeAttributes=True}
	read no [(n,c,p,a)] = Ok (n,c,p,a)
	read no _           = Error (exception ("Could not find task instance "<+++ no))
	write no data       = Ok (Just [data])
	notify no _         = const ((==) no)

taskInstanceConstants :: SDSLens InstanceNo InstanceConstants ()
taskInstanceConstants = sdsLens "taskInstanceConstants" param (SDSRead read) (SDSWrite write) (SDSNotifyConst notify) (Just \p ws -> Ok ())  filteredInstanceIndex
where
	param no = {InstanceFilter|onlyInstanceNo=Just [TaskId no 0],notInstanceNo=Nothing,includeSessions=True,includeDetached=True,includeStartup=True,matchAttribute=Nothing
			   ,includeConstants=True,includeProgress=False,includeAttributes=False}
	read no [(_,Just c,_,_)]    = Ok c
	read no _                   = Error (exception ("Could not find constants for task instance "<+++ no))
	write _ _ _                 = Ok Nothing
	notify _ _                  = const (const False)

taskInstanceProgress :: SDSLens InstanceNo InstanceProgress InstanceProgress
taskInstanceProgress = sdsLens "taskInstanceProgress" param (SDSRead read) (SDSWrite write) (SDSNotifyConst notify) (Just \p ws -> read p ws) filteredInstanceIndex
where
	param no = {InstanceFilter|onlyInstanceNo=Just [TaskId no 0],notInstanceNo=Nothing,includeSessions=True,includeDetached=True,includeStartup=True,matchAttribute=Nothing
			   ,includeConstants=False,includeProgress=True,includeAttributes=False}
	read no [(_,_,Just p,_)]    = Ok p
	read no _                   = Error (exception ("Could not find progress for task instance "<+++ no))
	write no [(n,c,_,a)] p      = Ok (Just [(n,c,Just p,a)])
	write no _ _                = Error (exception ("Could not find progress for task instance "<+++ no))
	notify no _                 = const ((==) no)

taskInstanceAttributes :: SDSLens InstanceNo (TaskAttributes,TaskAttributes) (TaskAttributes,TaskAttributes)
taskInstanceAttributes = sdsLens "taskInstanceAttributes" param (SDSRead read) (SDSWrite write) (SDSNotifyConst notify) Nothing filteredInstanceIndex
where
	param no = {InstanceFilter|onlyInstanceNo=Just [TaskId no 0],notInstanceNo=Nothing,includeSessions=True,includeDetached=True,includeStartup=True,matchAttribute=Nothing
			   ,includeConstants=False,includeProgress=False,includeAttributes=True}

	read no [(_,_,_,Just a)] = Ok a
	read no _                = Error (exception ("Could not find attributes for task instance "<+++ no))

	write no [(n,c,p,a)] a` = Ok (Just [(n,c,p,Just a`)])
	notify no _                 = const ((==) no)

//Top list share has no items, and is therefore completely polymorphic
topLevelTaskList :: SDSLens TaskListFilter (!TaskId,![TaskListItem a]) [(TaskId,TaskAttributes)]
topLevelTaskList = sdsLens "topLevelTaskList" param (SDSRead read) (SDSWrite write) (SDSNotifyConst notify) (Just reducer)
					 ((sdsFocus filter filteredInstanceIndex) >*| currentInstanceShare)
where
    param _ = ()
    filter = {InstanceFilter|onlyInstanceNo=Nothing,notInstanceNo=Nothing,includeSessions=False,includeDetached=True,includeStartup=False
		,matchAttribute=Nothing,includeConstants=True,includeProgress=True,includeAttributes=True}
    read _ (instances,curInstance) = Ok (TaskId 0 0, items)
    where
        items = [{TaskListItem|taskId = taskId, listId = listId
                 , detached = True, self = taskId == (TaskId curInstance 0)
                 , value = NoValue, progress = Just progress, attributes = mergeTaskAttributes attributes
                 } \\ (taskId,Just {InstanceConstants|type=PersistentInstance (Just listId)},Just progress, Just attributes) <- instances]

    write _ _ [] = Ok Nothing
    write _ (instances,_) updates = Ok (Just (map (updateInstance updates) instances))
    where
        updateInstance updates (taskId,c,p,a) = (taskId,c,p,foldr updateAttributes a updates)
        where
            updateAttributes (target,attrNew) attrOld = if (target == taskId) (Just (attrNew,'DM'.newMap)) attrOld
            updateAttributes _ attrOld = attrOld

    notify _ _ _ _ = True

    reducer :: TaskListFilter [InstanceData] -> MaybeError TaskException [(TaskId,TaskAttributes)]
	reducer p ws = Ok (map ff ws)
	where
	  ff (i, _, _, Just attr) = (i, mergeTaskAttributes attr)

//Evaluation state of instances
localShare :: SDSLens TaskId a a | iTask a
localShare = sdsLens "localShare" param (SDSRead read) (SDSWrite write) (SDSNotifyConst notify) (Just reducer) (removeMaybe (Just 'DM'.newMap) taskInstanceShares)
where
	param (TaskId instanceNo _) = instanceNo
	read taskId shares = case 'DM'.get taskId shares of
		Just json = case fromDeferredJSON json of
			Just r  = Ok r
			Nothing = Error (exception ("Failed to decode json of local share " <+++ taskId))
		Nothing
			= Error (exception ("Could not find local share " <+++ taskId))
	write taskId shares w = Ok (Just ('DM'.put taskId (DeferredJSON w) shares))
	notify taskId _ = const ((==) taskId)
	reducer taskId shares = read taskId shares


taskInstanceParallelTaskList :: SDSLens (TaskId,TaskListFilter) [TaskMeta] [TaskMeta]
taskInstanceParallelTaskList = sdsLens "taskInstanceParallelTaskList" param (SDSRead read) (SDSWrite write) (SDSNotifyConst notify) (Just \p ws -> read p ws) (removeMaybe (Just 'DM'.newMap) taskInstanceParallelTaskLists)
where
	param (TaskId instanceNo _,listFilter) = instanceNo
	read (taskId,listFilter) lists = case 'DM'.get taskId lists of
		Just list = Ok (map snd (filter (inFilter listFilter) (enumerate list)))
		Nothing = Error (exception ("Could not find parallel task list of " <+++ taskId))
	write (taskId,listFilter) lists w
		= Ok (Just ('DM'.put taskId (merge listFilter (maybe [] enumerate ('DM'.get taskId lists)) w) lists))

	notify (taskId,listFilter) states ts (regTaskId,regListFilter)
		# states = filter (inFilter listFilter) (enumerate states)//Ignore the states outside our filter
		//Different list, so eliminate
		| taskId <> regTaskId = False
		//No overlap in columns: eliminate
		| not ((listFilter.TaskListFilter.includeValue && regListFilter.TaskListFilter.includeValue)
		  || (listFilter.TaskListFilter.includeAttributes && regListFilter.TaskListFilter.includeAttributes)
		  || (listFilter.TaskListFilter.includeProgress && regListFilter.TaskListFilter.includeProgress)) = False
		//Check if the written records match the registered filter
		| maybe False (\taskIds -> all (\t -> not (isMember t taskIds)) [taskId \\(_,{TaskMeta|taskId}) <- states]) regListFilter.onlyTaskId
			= False
		| maybe False (\indices -> all (\i -> not (isMember i indices)) (map fst states)) regListFilter.onlyIndex
			= False
		//Looks like we can't eliminate, so we may need to notify
		| otherwise
			= True

	//enumerate = zip2 [0..]
	enumerate l = [(i,x) \\ x <- l & i <- [0..]]

	inFilter {TaskListFilter|onlyTaskId,onlyIndex} (index, {TaskMeta|taskId})
		=  maybe True (\taskIds -> isMember taskId taskIds) onlyTaskId
		&& maybe True (\indices -> isMember index indices) onlyIndex

	//ASSUMPTION: BOTH LISTS ARE SORTED BY TASK ID
	merge:: TaskListFilter [(Int,TaskMeta)] [TaskMeta] -> [TaskMeta]
	merge listFilter os ns = merge` os ns
	where
		listLength = length os	

		merge` [(i,o):os] [n:ns]
			| o.TaskMeta.taskId == n.TaskMeta.taskId //Potential update
				| inFilter listFilter (i,o) = [n:merge` os ns] //Only update the item if it matches the filter
				| otherwise 			    = [o:merge` os ns]
			| o.TaskMeta.taskId < n.TaskMeta.taskId //The taskId of the old item is not in the written set
				| inFilter listFilter (i,o) = merge` os [n:ns]  	//The old item was in the filter, so it was removed
				| otherwise 			    = [o:merge` os [n:ns]] //The old item was not in the filter, so it is ok that is not in the written list
			| otherwise
				| inFilter listFilter (i,n) = [n:merge` [(i,o):os] ns]
				| otherwise 			    = merge` [(i,o):os] ns

		merge` [] ns = [n \\ (i,n) <- zip2 [listLength ..] ns | inFilter listFilter (i,n)] //All new elements are only added if they are within the filter
		merge` os [] = [o \\ (i,o) <- os | not (inFilter listFilter (i,o))] //Only keep old elements if they were outside the filter

taskInstanceParallelTaskListValues :: SDSLens (TaskId,TaskListFilter) (Map TaskId (TaskValue DeferredJSON)) (Map TaskId (TaskValue DeferredJSON)) 
taskInstanceParallelTaskListValues = sdsLens "taskInstanceParallelTaskListValues" param (SDSRead read) (SDSWrite write) (SDSNotifyConst notify) (Just \p ws -> read p ws) (removeMaybe (Just 'DM'.newMap) taskInstanceParallelValues)
where
	param (TaskId instanceNo _,listFilter) = instanceNo
	read (listId,listFilter) lists = case 'DM'.get listId lists of
		Just list = Ok list //TODO: Prune the map based on the listfilter
		Nothing = Ok  'DM'.newMap //Error (exception ("Could not find parallel values of " <+++ listId))

	write (taskId,listFilter) lists w //
		= Ok (Just ('DM'.put taskId w lists))

	notify (listId,listFilter) states ts (regListId,regListFilter)
		= listId == regListId //TODO: If we keep this SDS, we need to be more precise in notifying based on the filter
	
taskInstanceParallelTaskListItem :: SDSLens (TaskId,TaskId) TaskMeta TaskMeta
taskInstanceParallelTaskListItem = sdsLens "taskInstanceParallelTaskListItem" param (SDSRead read) (SDSWrite write) (SDSNotifyConst notify) (Just reducer) taskInstanceParallelTaskList
where
	param (listId,taskId)
		= (listId,{TaskListFilter|onlyIndex=Nothing,onlyTaskId=Just [taskId],onlySelf=False,includeValue=False,includeAttributes=True,includeProgress=False})
	read p=:(listId,taskId) [] = Error (exception ("Could not find parallel task " <+++ taskId <+++ " in list " <+++ listId))
	read p=:(_,taskId) [x:xs] = if (x.TaskMeta.taskId == taskId) (Ok x) (read p xs)
	write (_,taskId) list pts = Ok (Just [if (x.TaskMeta.taskId == taskId) pts x \\ x <- list])
	notify (listId,taskId) _ = const ((==) taskId o snd)
	reducer p ws = read p ws

taskInstanceParallelTaskListValue :: SDSLens (TaskId,TaskId) (TaskValue DeferredJSON) (TaskValue DeferredJSON) 
taskInstanceParallelTaskListValue = sdsLens "taskInstanceParallelTaskListValue" param (SDSRead read) (SDSWrite write) (SDSNotifyConst notify) (Just reducer) taskInstanceParallelTaskListValues
where
	param (listId,taskId)
		= (listId,{TaskListFilter|onlyIndex=Nothing,onlyTaskId=Just [taskId],onlySelf=False,includeValue=True,includeAttributes=False,includeProgress=False})
	read p=:(listId,taskId) values = case 'DM'.get taskId values of
		(Just x) = (Ok x)
		_        = Error (exception ("Could not find parallel task " <+++ taskId <+++ " in list " <+++ listId))
	write (_,taskId) values value = Ok (Just ('DM'.put taskId value values))
	notify (listId,taskId) _ = const ((==) taskId o snd)
	reducer p ws = read p ws


taskInstanceEmbeddedTask :: SDSLens TaskId (Task a) (Task a) | iTask a
taskInstanceEmbeddedTask = sdsLens "taskInstanceEmbeddedTask" param (SDSRead read) (SDSWrite write) (SDSNotifyConst notify) (Just reducer) taskInstanceReduct
where
	param (TaskId instanceNo _) = instanceNo
	read taskId (Just {TIReduct|tasks}) = case ('DM'.get taskId tasks) of
		(Just dyn) = Ok (unwrapTask dyn)
		_          = Error (exception ("Could not find embedded task " <+++ taskId))
	read taskId Nothing = Error (exception ("Could not find task instance for " <+++ taskId))

	write taskId (Just r=:{TIReduct|tasks}) w = Ok (Just (Just {TIReduct|r & tasks = 'DM'.put taskId (dynamic w :: Task a^) tasks}))
	write taskId Nothing w = Error (exception ("Could not find task instance for " <+++ taskId))

	notify taskId _ = const ((==) taskId)
	reducer p reduct = read p reduct

parallelTaskList :: SDSSequence (!TaskId,!TaskId,!TaskListFilter) (!TaskId,![TaskListItem a]) [(TaskId,TaskAttributes)] | iTask a
parallelTaskList
	= sdsSequence "parallelTaskList" id param2 (\_ _ -> Right read) (SDSWriteConst write1) (SDSWriteConst write2) filteredTaskStates filteredInstanceIndex
where
	filteredTaskStates
		= sdsLens "parallelTaskListStates" param (SDSRead read) (SDSWrite write) (SDSNotifyConst notify) (Just lensReducer) (taskInstanceParallelTaskList >*< taskInstanceParallelTaskListValues)
	where
		param (listId,selfId,listFilter=:{TaskListFilter|onlySelf,onlyTaskId})
			= (listId,{TaskListFilter|listFilter & onlyTaskId = if onlySelf (Just [selfId:fromMaybe [] onlyTaskId]) onlyTaskId})

		read (listId,selfId,listFilter) (states,values)  = Ok (listId,items)
		where
			items = [{TaskListItem|taskId = taskId, listId = listId
					 , detached = isDetached listId taskId, self = taskId == selfId
					 , value = maybe NoValue decode ('DM'.get taskId values), progress = Nothing, attributes = 'DM'.union managementAttributes taskAttributes
					 } \\ {TaskMeta|taskId,taskAttributes,managementAttributes,change} <- states | change =!= Just RemoveTask]

			decode NoValue	= NoValue
			decode (Value json stable) = maybe NoValue (\v -> Value v stable) (fromDeferredJSON json)
	
			//When the task is part of another instance than the listid we can conclude that the task is detached
			isDetached (TaskId listInstance _) (TaskId taskInstance _) = taskInstance <> listInstance

		write (listId,selfId,{TaskListFilter|includeAttributes=False}) _ _ = Ok Nothing
		write (listId,selfId,listFilter) (states,values) [] = Ok (Just (states,values))
		write (listId,selfId,listFilter) (states,values) [(t,a):updates]
			# states = [if (taskId == t) {TaskMeta|meta & managementAttributes = a, unsyncedAttributes = 'DS'.fromList $ 'DM'.keys a} meta \\ meta=:{TaskMeta|taskId} <- states]
			= (write (listId,selfId,listFilter) (states,values) updates)

		notify (listId,_,_) states ts (regListId,_,_) = regListId == listId //Only check list id, the listFilter is checked one level up

		lensReducer (listId, selfId, listFilter) (ws,_)
			= (Ok ([(taskId, managementAttributes) \\ {TaskMeta|taskId,managementAttributes,change} <- ws | change =!= Just RemoveTask]))

	param2 _ (listId,items) = {InstanceFilter|onlyInstanceNo=Just [taskId \\ {TaskListItem|taskId,detached} <- items | detached],notInstanceNo=Nothing
					 ,includeSessions=True,includeDetached=True,includeStartup=True,matchAttribute=Nothing, includeConstants = False, includeAttributes = True,includeProgress = True}

	read ((listId,items),detachedInstances)
		# detachedProgress = 'DM'.fromList [(taskId,progress) \\ (taskId,_,Just progress,_) <- detachedInstances]
		# detachedAttributes= 'DM'.fromList [(taskId,mergeTaskAttributes attributes) \\ (taskId,_,_,Just attributes) <- detachedInstances]
		= (listId,[{TaskListItem|item & progress = 'DM'.get taskId detachedProgress
								, attributes = if detached (fromMaybe 'DM'.newMap ('DM'.get taskId detachedAttributes)) attributes}
				  \\ item=:{TaskListItem|taskId,detached,attributes} <- items])

	write1 p w = Ok (Just w)
	write2 p w = Ok Nothing //TODO: Write attributes of detached instances


newDocumentId :: !*IWorld -> (!DocumentId, !*IWorld)
newDocumentId iworld = generateRandomString 32 iworld

documentContent :: SDSLens String String String
documentContent = sdsTranslate "documentContent" (\docId -> docId +++ "-content") (blobStoreShare NS_DOCUMENT_CONTENT False Nothing)

createDocument :: !String !String !String !*IWorld -> (!MaybeError FileError Document, !*IWorld)
createDocument name mime content iworld
	# (documentId, iworld)	= newDocumentId iworld
	# document				= {Document|documentId = documentId, contentUrl = "/download/"+++documentId, name = name, mime = mime, size = size content}
	# (_,iworld)            = 'SDS'.write content (sdsFocus documentId documentContent) 'SDS'.EmptyContext iworld
	# (_,iworld)			= 'SDS'.write document (sdsFocus documentId (sdsTranslate "document_meta" (\d -> d +++ "-meta") (jsonFileStore NS_DOCUMENT_CONTENT  False False Nothing))) 'SDS'.EmptyContext iworld
	= (Ok document,iworld)

loadDocumentContent	:: !DocumentId !*IWorld -> (!Maybe String, !*IWorld)
loadDocumentContent documentId iworld
	= case 'SDS'.read (sdsFocus documentId documentContent) 'SDS'.EmptyContext iworld of
		(Ok ('SDS'.ReadingDone content),iworld) = (Just content,iworld)
		(Error e,iworld)    = (Nothing,iworld)

loadDocumentMeta :: !DocumentId !*IWorld -> (!Maybe Document, !*IWorld)
loadDocumentMeta documentId iworld
	= case ('SDS'.read (sdsFocus documentId (sdsTranslate "document_meta" (\d -> d+++"-meta") (jsonFileStore NS_DOCUMENT_CONTENT False False Nothing))) 'SDS'.EmptyContext iworld) of
		(Ok ('SDS'.ReadingDone doc),iworld)     = (Just doc,iworld)
		(Error e,iworld)    = (Nothing,iworld)

documentLocation :: !DocumentId !*IWorld -> (!FilePath,!*IWorld)
documentLocation documentId iworld=:{options={storeDirPath}}
	= (storeDirPath </> NS_DOCUMENT_CONTENT </> (documentId +++ "-content"),iworld)
