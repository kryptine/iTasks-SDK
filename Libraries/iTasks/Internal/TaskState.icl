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
import qualified Control.Monad

derive JSONEncode TaskMeta, InstanceType, TIValue, TIReduct, TaskChange, TaskResult, TaskEvalInfo, ExtendedTaskListFilter
derive JSONDecode TaskMeta, InstanceType, TIValue, TIReduct, TaskChange, TaskResult, TaskEvalInfo, ExtendedTaskListFilter

derive gDefault InstanceProgress, InstanceType, TaskId, ValueStatus, TaskListFilter

gDefault{|TaskMeta|}
	= {taskId= TaskId 0 0,instanceType=gDefault{|*|},build="",createdAt=gDefault{|*|},valuestatus=gDefault{|*|},attachedTo=[],instanceKey=Nothing
	  ,firstEvent=Nothing,lastEvent=Nothing,taskAttributes='DM'.newMap,managementAttributes='DM'.newMap,unsyncedAttributes = 'DS'.newSet
	  ,change = Nothing, initialized = False}

gDefault{|ExtendedTaskListFilter|}
	= {includeSessions=True,includeDetached=True,includeStartup=True,includeTaskReduct=False}

derive gEq TaskChange
derive gText TaskChange, Set, ExtendedTaskListFilter

instance < TaskMeta where
	(<) {TaskMeta|taskId=t1} {TaskMeta|taskId=t2} = t1 < t2

mergeTaskAttributes :: !(!TaskAttributes,!TaskAttributes) -> TaskAttributes
mergeTaskAttributes (explicit,implicit) = 'DM'.union explicit implicit

//Unfiltered administration
rawTaskIndex         = storeShare NS_TASK_INSTANCES False InJSONFile (Just [])
rawTaskNoCounter     = storeShare NS_TASK_INSTANCES False InJSONFile (Just 1)

rawInstanceIO        = storeShare NS_TASK_INSTANCES False InMemory (Just 'DM'.newMap)

rawInstanceReduct         = mbStoreShare NS_TASK_INSTANCES True InDynamicFile
rawInstanceValue          = mbStoreShare NS_TASK_INSTANCES True InDynamicFile
rawInstanceShares         = mbStoreShare NS_TASK_INSTANCES True InDynamicFile

allTaskLists :: SDSLens TaskId [TaskMeta] [TaskMeta]
allTaskLists  = sdsTranslate "allTaskLists" param $ storeShare NS_TASK_INSTANCES False InJSONFile (Just [])
where
	param (TaskId instanceNo taskNo) = "tasklist-"+++toString instanceNo +++ "-" +++toString taskNo

allTaskValues :: SDSLens TaskId (Map TaskId DeferredJSON) (Map TaskId DeferredJSON)
allTaskValues = sdsTranslate "allTaskValues" param $ storeShare NS_TASK_INSTANCES True InDynamicFile (Just 'DM'.newMap)
where
	param (TaskId instanceNo taskNo) = "taskvalues-"+++toString instanceNo +++ "-" +++toString taskNo

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
	# meta = {defaultValue & taskId= TaskId instanceNo 0, instanceType=SessionInstance,build=appVersion,createdAt=clock}
    =            'SDS'.write meta (sdsFocus instanceNo taskInstance) 'SDS'.EmptyContext iworld
  `b` \iworld -> 'SDS'.write (Just (createReduct instanceNo task taskTime)) (sdsFocus instanceNo taskInstanceReduct) 'SDS'.EmptyContext iworld
  `b` \iworld -> 'SDS'.write (Just (TIValue NoValue)) (sdsFocus instanceNo taskInstanceValue) 'SDS'.EmptyContext iworld
  `b` \iworld -> (Ok (TaskId instanceNo 0), iworld)

createSessionTaskInstance :: !(Task a) !TaskAttributes !*IWorld -> (!MaybeError TaskException (!InstanceNo,InstanceKey),!*IWorld) | iTask a
createSessionTaskInstance task attributes iworld=:{options={appVersion,autoLayout},current={taskTime},clock}
	# task = if autoLayout (ApplyLayout defaultSessionLayout @>> task) task
    # (Ok instanceNo,iworld) = newInstanceNo iworld
    # (instanceKey,iworld)  = newInstanceKey iworld
	# meta = {defaultValue & taskId= TaskId instanceNo 0,instanceType=SessionInstance
		,instanceKey = Just instanceKey,build=appVersion,createdAt=clock,taskAttributes=attributes}
    =            'SDS'.write meta (sdsFocus instanceNo taskInstance) 'SDS'.EmptyContext iworld
  `b` \iworld -> 'SDS'.write (Just (createReduct instanceNo task taskTime)) (sdsFocus instanceNo taskInstanceReduct) 'SDS'.EmptyContext iworld
  `b` \iworld -> 'SDS'.write (Just (TIValue NoValue)) (sdsFocus instanceNo taskInstanceValue) 'SDS'.EmptyContext iworld
  `b` \iworld -> (Ok (instanceNo,instanceKey), iworld)

createStartupTaskInstance :: !(Task a) !TaskAttributes !*IWorld -> (!MaybeError TaskException InstanceNo, !*IWorld) | iTask a
createStartupTaskInstance task attributes iworld=:{options={appVersion,autoLayout},current={taskTime},clock}
    # (Ok instanceNo,iworld) = newInstanceNo iworld
	# meta = {defaultValue & taskId= TaskId instanceNo 0,instanceType=StartupInstance,build=appVersion,createdAt=clock,taskAttributes=attributes}
    =            'SDS'.write meta (sdsFocus instanceNo taskInstance) 'SDS'.EmptyContext iworld
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
	# meta = {defaultValue & taskId = TaskId instanceNo 0,instanceType=PersistentInstance mbListId,build=appVersion
		,createdAt=clock,taskAttributes=attributes, instanceKey=Just instanceKey}
    =            'SDS'.write meta (sdsFocus instanceNo taskInstance) 'SDS'.EmptyContext iworld
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
	# meta                  ='SDS'.directResult (fromOk meta)
	=            'SDS'.write (Just (createReduct instanceNo task taskTime)) (sdsFocus instanceNo taskInstanceReduct) 'SDS'.EmptyContext iworld
  `b` \iworld -> 'SDS'.write (Just (TIValue NoValue)) (sdsFocus instanceNo taskInstanceValue) 'SDS'.EmptyContext iworld
  `b` \iworld -> let  in  'SDS'.write {TaskMeta|meta & build=appVersion} (sdsFocus instanceNo taskInstance) 'SDS'.EmptyContext iworld
  `b` \iworld -> (Ok (), iworld)

deleteTaskInstance	:: !InstanceNo !*IWorld -> *(!MaybeError TaskException (), !*IWorld)
deleteTaskInstance instanceNo iworld=:{IWorld|options={EngineOptions|persistTasks}}
	//Delete in index
	# param = (TaskId 0 0,TaskId 0 0,defaultValue,defaultValue) 
	# (mbe,iworld)    = 'SDS'.modify (\(_,is) -> [i \\ i=:{TaskMeta|taskId} <- is | taskId <> TaskId instanceNo 0])
		(sdsFocus param taskListMetaData) 'SDS'.EmptyContext iworld
	| mbe =: (Error _) = (toME mbe,iworld)
	//Remove all edit/action/edit events from the queue
	# iworld = clearEvents instanceNo iworld
	//Queue a final destroy event
	# iworld = queueEvent instanceNo DestroyEvent iworld
	= (Ok (),iworld)
  where
	toME (Ok ('SDS'.ModifyingDone _)) = Ok ()
	toME (Error e) = (Error e)

taskListMetaData :: SDSLens (!TaskId,!TaskId,!TaskListFilter,!ExtendedTaskListFilter) (!TaskId,![TaskMeta]) [TaskMeta]
taskListMetaData
	= sdsLens "taskListMetaData" param (SDSRead read) (SDSWrite write) (SDSNotify notify) Nothing allTaskLists
where
	param (listId,_,_,_) = listId
	read (listId,selfId,tfilter,efilter) rows
		= Ok (listId, map snd $ filter (inFilter tfilter efilter) $ enumerate rows)

	write (listId,selfId,tfilter,efilter) rows updates
		= Ok $ Just $ update (inFilter tfilter efilter) (enumerate $ sort rows) (sort updates)
	where
		update pred [(i,o):os] [n:ns]
			| o.TaskMeta.taskId == n.TaskMeta.taskId //Potential update
				| pred (i,o) = [n:update pred os ns] //Only update the item if it matches the filter
				| otherwise  = [o:update pred os ns]
			| o.TaskMeta.taskId < n.TaskMeta.taskId //The taskId of the old item is not in the written set
				| pred (i,o) = update pred os [n:ns] //The old item was in the filter, so it was removed
				| otherwise  = [o:update pred os [n:ns]] //The old item was not in the filter, so it is ok that is not in the written list
			| otherwise
				| pred (-1,n) = [n:update pred [(i,o):os] ns] //New items don't have an index yet
				| otherwise  = update pred [(i,o):os] ns

		update pred [] ns = [n \\ n <- ns | pred (-1,n)] //All new elements are only added if they are within the filter
		update pred os [] = [o \\ (i,o) <- os | not (pred (i,o))] //Only keep old elements if they were outside the filter

	notify (plistId,pselfId,ptfilter,pefilter) rows updates ts (qlistId,qselfId,qtfilter,qefilter)
		| plistId <> qlistId = False //Not the same list
		| pselfId == qselfId = False //To prevent self referencing loops, don't notify when the writing branch is the same as the registered branch
		//Check overlap:
		//This is similar to updating, but now we just determine the rows that would be affected and then check
		//if these rows also matched the registered filter
		| otherwise = check (inFilter ptfilter pefilter) (inFilter qtfilter qefilter) (enumerate $ sort rows) (sort updates)
	where
		check ppred qpred [] [] = False //We haven't found a reason to notify
		check ppred qpred [(i,o):os] [n:ns]
			| o.TaskMeta.taskId == n.TaskMeta.taskId //Potential update
				| ppred (i,o) && qpred (i,o) = True //This item would be updated, and matches the registered filter
				| otherwise  = check ppred qpred os ns
			| o.TaskMeta.taskId < n.TaskMeta.taskId //The taskId of the old item is not in the written set
				| ppred (i,o) && qpred (i,o) = True //This item would be deleted, and matches the registered filter
				| otherwise  = check ppred qpred os [n:ns] //The old item was not in the filter, so it is ok that is not in the written list
			| otherwise
				| ppred (-1,n) && qpred (-1,n) = True//This item would be inserted, and matches the registered filter
				| otherwise  = check ppred qpred [(i,o):os] ns
		check ppred qpred [] [n:ns]
			| ppred (-1,n) && qpred (-1,n) = True //This item would be newly appended and macthes the registered filter
			| otherwise = check ppred qpred [] ns
		check ppred qpred [(i,o):os] []
			| (not $ ppred (i,o)) && qpred (i,o) = True //This item would be deleted and matches the registered filter
			| otherwise = check ppred qpred os []

	enumerate l = [(i,x) \\ x <- l & i <- [0..]]

	inFilter
		{TaskListFilter|onlyTaskId,notTaskId,onlyIndex,onlyAttribute}
		{ExtendedTaskListFilter|includeSessions,includeDetached,includeStartup}
		(index, {TaskMeta|taskId,instanceType,taskAttributes,managementAttributes})
		=  maybe True (\taskIds -> isMember taskId taskIds) onlyTaskId
		&& maybe True (\taskIds -> not (isMember taskId taskIds)) notTaskId
		&& maybe True (\indices -> isMember index indices) onlyIndex
		&& maybe True (\(mk,mv) ->
			(maybe False ((==) mv) ('DM'.get mk taskAttributes)
			|| maybe False ((==) mv) ('DM'.get mk managementAttributes))) onlyAttribute
        && ((includeSessions && instanceType =: (SessionInstance)) ||
		    (includeDetached && instanceType =: (PersistentInstance _)) ||
		    (includeStartup && instanceType =: (StartupInstance))
		   )

taskListValueData :: SDSLens (!TaskId,!TaskId,!TaskListFilter,!ExtendedTaskListFilter) (Map TaskId (TaskValue a)) (Map TaskId (TaskValue a)) | iTask a
taskListValueData = sdsLens "taskListValueData" param (SDSRead read) (SDSWrite write) (SDSNotify notify) Nothing allTaskValues
where
	param (listId,_,_,_) = listId

	//FIXME: Filter values when reading and writing
	//As it is now, it is a big memory leak because we can't erase values 
	read (listId,selfId,tfilter,efilter) values
		= Ok $ fmap decode values
	write (listId,selfId,tfilter,efilter) values updates
		= Ok $ Just $ 'DM'.union (fmap encode updates) values
	
	decode deferred = fromMaybe NoValue $ fromDeferredJSON deferred
	encode value = DeferredJSON value

	inFilter tfilter efilter (index,(taskId,value)) = True

	notify _ _ _ _ _ = False
	

//Filtered views on the instance index
taskInstance :: SDSLens InstanceNo TaskMeta TaskMeta
taskInstance = sdsLens "taskInstance" param (SDSRead read) (SDSWriteConst write) (SDSNotifyConst notify) Nothing taskListMetaData
where
	param no = (TaskId 0 0, TaskId 0 0, {TaskListFilter|defaultValue & onlyTaskId = Just [TaskId no 0]},
			{ExtendedTaskListFilter|defaultValue & includeSessions=True,includeDetached=True,includeStartup=True})
	read no (_,[meta]) = Ok meta 
	read no _ = Error (exception ("Could not find task instance "<+++ no))
	write no data       = Ok (Just [data])
	notify no _         = const ((==) no)

taskInstanceConstants :: SDSLens InstanceNo InstanceConstants ()
taskInstanceConstants = sdsLens "taskInstanceConstants" param (SDSRead read) (SDSWrite write) (SDSNotifyConst notify) (Just \p ws -> Ok ()) taskListMetaData
where
	param no = (TaskId 0 0, TaskId 0 0, {TaskListFilter|defaultValue & onlyTaskId = Just [TaskId no 0]},
			{ExtendedTaskListFilter|defaultValue & includeSessions=True,includeDetached=True,includeStartup=True})

	read no (_,[{TaskMeta|instanceType,build,createdAt}]) = Ok {InstanceConstants|type=instanceType,build=build,issuedAt=createdAt}
	read no _                   = Error (exception ("Could not find constants for task instance "<+++ no))
	write _ _ _                 = Ok Nothing
	notify _ _                  = const (const False)

taskInstanceProgress :: SDSLens InstanceNo InstanceProgress InstanceProgress
taskInstanceProgress = sdsLens "taskInstanceProgress" param (SDSRead read) (SDSWrite write) (SDSNotifyConst notify) Nothing taskListMetaData 
where
	param no = (TaskId 0 0, TaskId 0 0, {TaskListFilter|defaultValue & onlyTaskId = Just [TaskId no 0]},
			{ExtendedTaskListFilter|defaultValue & includeSessions=True,includeDetached=True,includeStartup=True})

	read no (_,[{TaskMeta|valuestatus,attachedTo,instanceKey,firstEvent,lastEvent}]) 
		= Ok {InstanceProgress|value=valuestatus,attachedTo=attachedTo,instanceKey=instanceKey,firstEvent=firstEvent,lastEvent=lastEvent}
	read no _                   = Error (exception ("Could not find progress for task instance "<+++ no))
	write no (_,[meta]) {InstanceProgress|value,attachedTo,instanceKey,firstEvent,lastEvent}
		= Ok (Just [{TaskMeta|meta & valuestatus = value, attachedTo = attachedTo,
			instanceKey = instanceKey,firstEvent = firstEvent, lastEvent = lastEvent }])
	write no _ _                = Error (exception ("Could not find progress for task instance "<+++ no))
	notify no _                 = const ((==) no)

taskInstanceAttributes :: SDSLens InstanceNo (TaskAttributes,TaskAttributes) (TaskAttributes,TaskAttributes)
taskInstanceAttributes = sdsLens "taskInstanceAttributes" param (SDSRead read) (SDSWrite write) (SDSNotifyConst notify) Nothing taskListMetaData
where
	param no = (TaskId 0 0, TaskId 0 0, {TaskListFilter|defaultValue & onlyTaskId = Just [TaskId no 0]},
			{ExtendedTaskListFilter|defaultValue & includeSessions=True,includeDetached=True,includeStartup=True})

	read no (_,[{TaskMeta|taskAttributes,managementAttributes}]) = Ok (taskAttributes,managementAttributes)
	read no _ = Error (exception ("Could not find attributes for task instance "<+++ no))

	write no (_,[meta]) (taskAttributes,managementAttributes) = Ok (Just [{TaskMeta|meta & taskAttributes = taskAttributes, managementAttributes = managementAttributes}])
	notify no _ = const ((==) no)

parallelTaskList :: SDSLens (!TaskId,!TaskId,!TaskListFilter) (!TaskId,![TaskListItem a]) [(TaskId,TaskAttributes)] | iTask a
parallelTaskList
	= sdsLens "parallelTaskList" param (SDSRead read) (SDSWrite write) (SDSNotifyConst notify) Nothing
		(taskListMetaData >*| taskListValueData)
where
	param (listId,selfId,listfilter) = (listId,selfId,listfilter,defaultValue)

	read (_,selfId,listfilter) ((listId,items),values) = Ok (listId, map (setValue o toTaskListItem selfId) items)
	where
		setValue item=:{TaskListItem|taskId} = {TaskListItem|item & value = fromMaybe NoValue ('DM'.get taskId values)}

	write (_,selfId,listfilter) ((listId,items),_) ws
		= Just <$> 'Control.Monad'.sequence
			[maybe (Error $ exception $ "Could not find task id "<+++ taskId <+++ "in list " <+++ listId)
				(\m -> Ok {TaskMeta|m & managementAttributes = managementAttributes}) ('DM'.get taskId itemsMap)
			\\ (taskId,managementAttributes) <- ws]
	where
		itemsMap = 'DM'.fromList [(taskId,meta) \\ meta=:{TaskMeta|taskId} <- items]

	notify _ _ _ _ = False

	toTaskListItem selfId {TaskMeta|taskId,instanceType,valuestatus,attachedTo,instanceKey,firstEvent,lastEvent,taskAttributes,managementAttributes}
		# listId = case instanceType of (PersistentInstance (Just listId)) = listId ; _ = (TaskId 0 0)
		= {TaskListItem|taskId = taskId, listId = listId, detached = True, self = taskId == selfId
		  ,value = NoValue, progress = Just progress, attributes = mergeTaskAttributes (taskAttributes,managementAttributes)}
	where
		progress = {InstanceProgress|value=valuestatus,attachedTo=attachedTo,instanceKey=instanceKey,firstEvent=firstEvent,lastEvent=lastEvent}

topLevelTaskList :: SDSLens TaskListFilter (!TaskId,![TaskListItem a]) [(TaskId,TaskAttributes)] | iTask a
topLevelTaskList = sdsTranslate "topLevelTaskListWrapper" id
	//This wrapping is rather pointless, but the rest of the system expects toLevelTaskList to be a lens instead of a sequence
	(
	sdsSequence "topLevelTaskList" //First read the currecnt instance to determine who is accessing the list
		param1 param2 read (SDSWriteConst write1) (SDSWriteConst write2) currentInstanceShare parallelTaskList
	)
where
	param1 listfilter = ()
	param2 listfilter curInstance = (TaskId 0 0, TaskId curInstance 0, listfilter)
	read _ _ = Right snd
	write1 _ _ = Ok Nothing
	write2 _ ws = Ok $ Just ws

taskInstanceParallelTaskList :: SDSLens (TaskId,TaskListFilter) (TaskId,[TaskMeta]) [TaskMeta]
taskInstanceParallelTaskList = sdsTranslate "taskInstanceParallelTaskList" param taskListMetaData
where
	param (listId,listfilter) = (listId,listId,listfilter,defaultValue)

taskInstanceParallelTaskListValues :: SDSLens (TaskId,TaskListFilter) (Map TaskId (TaskValue a)) (Map TaskId (TaskValue a)) | iTask a
taskInstanceParallelTaskListValues
	= sdsTranslate "taskInstanceParallelTaskListValues" param taskListValueData
where
	param (listId,listfilter) = (listId,listId,listfilter,defaultValue)

taskInstanceParallelTaskListItem :: SDSLens (TaskId,TaskId) TaskMeta TaskMeta
taskInstanceParallelTaskListItem = sdsLens "taskInstanceParallelTaskListItem" param (SDSRead read) (SDSWrite write) (SDSNotifyConst notify) (Just reducer) taskInstanceParallelTaskList
where
	param (listId,taskId)
		= (listId,{TaskListFilter|onlyIndex=Nothing,onlyTaskId=Just [taskId],notTaskId=Nothing,onlySelf=False,onlyAttribute=Nothing,includeValue=False,includeAttributes=True,includeProgress=False})
	read p=:(listId,taskId) (_,[]) = Error (exception ("Could not find parallel task " <+++ taskId <+++ " in list " <+++ listId))
	read p=:(_,taskId) (listId,[x:xs]) = if (x.TaskMeta.taskId == taskId) (Ok x) (read p (listId,xs))
	write (_,taskId) (_,list) pts = Ok (Just [if (x.TaskMeta.taskId == taskId) pts x \\ x <- list])
	notify (listId,taskId) _ = const ((==) taskId o snd)
	reducer p=:(listId,_) ws = read p (listId,ws)

taskInstanceParallelTaskListValue :: SDSLens (TaskId,TaskId) (TaskValue a) (TaskValue a) | iTask a
taskInstanceParallelTaskListValue
	= sdsLens "taskInstanceParallelTaskListValue" param (SDSRead read) (SDSWrite write) (SDSNotifyConst notify)
		(Just reducer) taskInstanceParallelTaskListValues
where
	param (listId,taskId)
		= (listId,{TaskListFilter|onlyIndex=Nothing,onlyTaskId=Just [taskId],notTaskId=Nothing,onlySelf=False,onlyAttribute=Nothing,includeValue=True,includeAttributes=False,includeProgress=False})
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
