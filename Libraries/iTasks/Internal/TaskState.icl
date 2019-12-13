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

derive JSONEncode TaskMeta, InstanceType, TaskChange, TaskResult, TaskEvalInfo, ExtendedTaskListFilter
derive JSONDecode TaskMeta, InstanceType, TaskChange, TaskResult, TaskEvalInfo, ExtendedTaskListFilter

derive gDefault InstanceType, TaskId, TaskListFilter

gDefault{|TaskMeta|}
	= {taskId= TaskId 0 0,instanceType=gDefault{|*|},build="",createdAt=gDefault{|*|},nextTaskNo=1,nextTaskTime=1
		,status=Right False,attachedTo=[],connectedTo=Nothing,instanceKey=Nothing
		,firstEvent=Nothing,lastEvent=Nothing, lastIO = Nothing
		,taskAttributes='DM'.newMap,managementAttributes='DM'.newMap,unsyncedAttributes = 'DS'.newSet
		,change = Nothing, initialized = False}

gDefault{|ExtendedTaskListFilter|} = fullExtendedTaskListFilter

derive gEq TaskChange
derive gText TaskChange, Set, ExtendedTaskListFilter

instance < TaskMeta where
	(<) {TaskMeta|taskId=t1} {TaskMeta|taskId=t2} = t1 < t2

fullExtendedTaskListFilter :: ExtendedTaskListFilter
fullExtendedTaskListFilter =
	 {includeSessions=True,includeDetached=True,includeStartup=True,includeTaskReduct=False,includeTaskIO=False}

encodeTaskValue :: (TaskValue a) -> TaskValue DeferredJSON | iTask a
encodeTaskValue (Value dec stable) = Value (DeferredJSON dec) stable
encodeTaskValue NoValue = NoValue

decodeTaskValue :: (TaskValue DeferredJSON) -> TaskValue a | iTask a
decodeTaskValue (Value enc stable) = maybe NoValue (\dec -> Value dec stable) (fromDeferredJSON enc)
decodeTaskValue NoValue = NoValue

allTaskLists :: SDSLens TaskId [TaskMeta] [TaskMeta]
allTaskLists  = sdsTranslate "allTaskLists" param $ storeShare NS_TASK_INSTANCES False InJSONFile (Just [])
where
	param (TaskId instanceNo taskNo) = "tasklist-"+++toString instanceNo +++ "-" +++toString taskNo

allTaskValues :: SDSLens TaskId (Map TaskId (TaskValue DeferredJSON)) (Map TaskId (TaskValue DeferredJSON))
allTaskValues = sdsTranslate "allTaskValues" param $ storeShare NS_TASK_INSTANCES True InDynamicFile (Just 'DM'.newMap)
where
	param (TaskId instanceNo taskNo) = "taskvalues-"+++toString instanceNo +++ "-" +++toString taskNo

allTaskReducts :: SDSLens TaskId (Map TaskId (Task DeferredJSON)) (Map TaskId (Task DeferredJSON))
allTaskReducts = sdsTranslate "allTaskReducts" param $ storeShare NS_TASK_INSTANCES True InDynamicFile (Just 'DM'.newMap)
where
	param (TaskId instanceNo taskNo) = "taskreducts-"+++toString instanceNo +++ "-" +++toString taskNo

//Next instance no counter
nextInstanceNo :: SimpleSDSLens Int
nextInstanceNo = sdsFocus "increment" $ storeShare NS_TASK_INSTANCES False InJSONFile (Just 1)

//Local shared data
taskInstanceShares :: SDSLens InstanceNo (Maybe (Map TaskId DeferredJSON)) (Maybe (Map TaskId DeferredJSON))
taskInstanceShares = sdsTranslate "taskInstanceShares" (\t -> t +++> "-shares") (mbStoreShare NS_TASK_INSTANCES True InDynamicFile)

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
    = 'SDS'.write meta (sdsFocus (instanceNo,False,False,False) taskInstance) 'SDS'.EmptyContext iworld
	`b` \iworld -> 'SDS'.write (task @ DeferredJSON) (sdsFocus instanceNo taskInstanceTask) 'SDS'.EmptyContext iworld
	`b` \iworld -> (Ok (TaskId instanceNo 0), iworld)

createSessionTaskInstance :: !(Task a) !TaskAttributes !*IWorld -> (!MaybeError TaskException (!InstanceNo,InstanceKey),!*IWorld) | iTask a
createSessionTaskInstance task attributes iworld=:{options={appVersion,autoLayout},current={taskTime},clock}
	# task = if autoLayout (ApplyLayout defaultSessionLayout @>> task) task
    # (Ok instanceNo,iworld) = newInstanceNo iworld
    # (instanceKey,iworld)  = newInstanceKey iworld
	# meta = {defaultValue & taskId= TaskId instanceNo 0,instanceType=SessionInstance
		,instanceKey = Just instanceKey,build=appVersion,createdAt=clock,taskAttributes=attributes}
    = 'SDS'.write meta (sdsFocus (instanceNo,False,False,False) taskInstance) 'SDS'.EmptyContext iworld
	`b` \iworld -> 'SDS'.write (task @ DeferredJSON) (sdsFocus instanceNo taskInstanceTask) 'SDS'.EmptyContext iworld
	`b` \iworld -> (Ok (instanceNo,instanceKey), iworld)

createStartupTaskInstance :: !(Task a) !TaskAttributes !*IWorld -> (!MaybeError TaskException InstanceNo, !*IWorld) | iTask a
createStartupTaskInstance task attributes iworld=:{options={appVersion,autoLayout},current={taskTime},clock}
    # (Ok instanceNo,iworld) = newInstanceNo iworld
	# meta = {defaultValue & taskId= TaskId instanceNo 0,instanceType=StartupInstance,build=appVersion,createdAt=clock,taskAttributes=attributes}
	= 'SDS'.write meta (sdsFocus (instanceNo,False,False,False) taskInstance) 'SDS'.EmptyContext iworld
	`b` \iworld -> 'SDS'.write (task @ DeferredJSON) (sdsFocus instanceNo taskInstanceTask) 'SDS'.EmptyContext iworld
	`b` \iworld -> (Ok instanceNo, queueEvent instanceNo ResetEvent iworld)

createDetachedTaskInstance :: !(Task a) !TaskEvalOpts !InstanceNo !TaskAttributes !TaskId !Bool !*IWorld -> (!MaybeError TaskException TaskId, !*IWorld) | iTask a
createDetachedTaskInstance task evalOpts instanceNo attributes listId refreshImmediate iworld=:{options={appVersion,autoLayout},current={taskTime},clock}
	# task = if autoLayout (ApplyLayout defaultSessionLayout @>> task) task
    # (instanceKey,iworld) = newInstanceKey iworld
	# mbListId             = if (listId == TaskId 0 0) Nothing (Just listId)
	# meta = {defaultValue & taskId = TaskId instanceNo 0, instanceType=PersistentInstance mbListId,build=appVersion
		,createdAt=clock,managementAttributes=attributes, instanceKey=Just instanceKey}
	= 'SDS'.write meta (sdsFocus (instanceNo,False,False,False) taskInstance) 'SDS'.EmptyContext iworld
	`b` \iworld -> 'SDS'.write (task @ DeferredJSON) (sdsFocus instanceNo taskInstanceTask) 'SDS'.EmptyContext iworld
	`b` \iworld -> ( Ok (TaskId instanceNo 0)
				 , if refreshImmediate
					  (queueEvent instanceNo ResetEvent iworld)
					  iworld)

replaceTaskInstance :: !InstanceNo !(Task a) *IWorld -> (!MaybeError TaskException (), !*IWorld) | iTask a
replaceTaskInstance instanceNo task iworld=:{options={appVersion},current={taskTime}}
	# (meta, iworld)        = 'SDS'.read (sdsFocus (instanceNo,False,False,False) taskInstance) 'SDS'.EmptyContext iworld
	| isError meta          = (liftError meta, iworld)
	# meta                  ='SDS'.directResult (fromOk meta)
	= 'SDS'.write (task @ DeferredJSON) (sdsFocus instanceNo taskInstanceTask) 'SDS'.EmptyContext iworld
	`b` \iworld -> let  in  'SDS'.write {TaskMeta|meta & build=appVersion} (sdsFocus (instanceNo,True,True,True) taskInstance) 'SDS'.EmptyContext iworld
	`b` \iworld -> (Ok (), iworld)

deleteTaskInstance	:: !InstanceNo !*IWorld -> *(!MaybeError TaskException (), !*IWorld)
deleteTaskInstance instanceNo iworld=:{IWorld|options={EngineOptions|persistTasks}}
	//Delete in index
	# param = (TaskId 0 0,TaskId 0 0,fullTaskListFilter,fullExtendedTaskListFilter)
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

(`b`) infixl 1 :: *(MaybeError e r, *st) (*st -> *(MaybeError e r`, *st)) -> *(MaybeError e r`, *st)
(`b`) (Ok _, st)    f = f st
(`b`) (Error e, st) _ = (Error e, st)

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
		| otherwise = check (inFilter ptfilter pefilter) (inFilter qtfilter qefilter) (relevantColumns ptfilter pefilter qtfilter qefilter)
			(enumerate $ sort rows) (sort updates)
	where
		check ppred qpred rel [] [] = False //We haven't found a reason to notify
		check ppred qpred rel [(i,o):os] [n:ns]
			| o.TaskMeta.taskId == n.TaskMeta.taskId //Potential update
				| ppred (i,o) && qpred (i,o) = rel //This item would be updated, and matches the registered filter
				| otherwise  = check ppred qpred rel os ns
			| o.TaskMeta.taskId < n.TaskMeta.taskId //The taskId of the old item is not in the written set
				| ppred (i,o) && qpred (i,o) = True //This item would be deleted, and matches the registered filter
				| otherwise  = check ppred qpred rel os [n:ns] //The old item was not in the filter, so it is ok that is not in the written list
			| otherwise
				| ppred (-1,n) && qpred (-1,n) = True//This item would be inserted, and matches the registered filter
				| otherwise  = check ppred qpred rel [(i,o):os] ns
		check ppred qpred rel [] [n:ns]
			| ppred (-1,n) && qpred (-1,n) = True //This item would be newly appended and macthes the registered filter
			| otherwise = check ppred qpred rel [] ns
		check ppred qpred rel [(i,o):os] []
			| ppred (i,o) && qpred (i,o) = True  //This item would be deleted and matches the registered filter
			| otherwise = check ppred qpred rel os []

	//For updated rows, we only notify if the write affects one of the relevant columns selected in the registered paramater
	relevantColumns ptfilter pefilter qtfilter qefilter
		=   (ptfilter.includeValue && qtfilter.includeValue)
		||  (ptfilter.includeTaskAttributes && qtfilter.includeTaskAttributes)
		||  (ptfilter.includeManagementAttributes && qtfilter.includeManagementAttributes)
		||  (ptfilter.includeProgress && qtfilter.includeProgress)
		||  (pefilter.includeTaskReduct && qefilter.includeTaskReduct)
		||  (pefilter.includeTaskIO && qefilter.includeTaskIO)

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

taskListDynamicValueData :: SDSLens (!TaskId,!TaskId,!TaskListFilter,!ExtendedTaskListFilter) (Map TaskId (TaskValue DeferredJSON)) (Map TaskId (TaskValue DeferredJSON))
taskListDynamicValueData = taskIdIndexedStore "taskListDynamicValueData" allTaskValues

taskListDynamicTaskData :: SDSLens (!TaskId,!TaskId,!TaskListFilter,!ExtendedTaskListFilter) (Map TaskId (Task DeferredJSON)) (Map TaskId (Task DeferredJSON))
taskListDynamicTaskData = taskIdIndexedStore "taskListDynamicTaskData" allTaskReducts

taskIdIndexedStore name sds = sdsLens name param (SDSRead read) (SDSWrite write) (SDSNotify notify) Nothing sds
where
	param (listId,_,_,_) = listId

	read (listId,selfId,tfilter,efilter) values
		= Ok $ 'DM'.fromList [value \\ value=:(taskId,_) <- 'DM'.toList values | inFilter tfilter taskId]

	write (listId,selfId,tfilter,efilter) values updates
		# updates = 'DM'.filterWithKey (\k v -> inFilter tfilter k) updates //Only consider updates that match the filter
		# selection = 'DM'.filterWithKey (\k v -> inFilter tfilter k) values //Find the orignal selection
		# deletes = 'DM'.keys $ 'DM'.intersection selection updates //The elements that are in the selecion, but not in the updates should be deleted
		= Ok $ Just $ 'DM'.union updates $ 'DM'.delList deletes values

	//We only use the taskId to select
	inFilter {TaskListFilter|onlyTaskId,notTaskId,onlyIndex,onlyAttribute} taskId
		=  maybe True (\taskIds -> isMember taskId taskIds) onlyTaskId
		&& maybe True (\taskIds -> not (isMember taskId taskIds)) notTaskId

	//We don't notify at all for these stores.
	notify _ _ _ _ _ = False
	
taskListTypedValueData :: SDSLens (!TaskId,!TaskId,!TaskListFilter,!ExtendedTaskListFilter) (Map TaskId (TaskValue a)) (Map TaskId (TaskValue a)) | iTask a
taskListTypedValueData = sdsLens "taskListTypedValueData" id (SDSRead read) (SDSWriteConst write) (SDSNotifyConst notify) Nothing taskListDynamicValueData
where
	read param values = Ok $ fmap decodeTaskValue values
	write param updates = Ok $ Just $ encodeTaskValue <$> updates
	notify _ _ _ _ = True

taskListTypedTaskData :: SDSLens (!TaskId,!TaskId,!TaskListFilter,!ExtendedTaskListFilter) (Map TaskId (Task a)) (Map TaskId (Task a)) | iTask a
taskListTypedTaskData = sdsLens "taskListTypedTaskData" id (SDSRead read) (SDSWriteConst write) (SDSNotifyConst notify) Nothing taskListDynamicTaskData
where
	read param tasks = Ok $ fmap (\t -> t @? decodeTaskValue) tasks
	write param updates = Ok $ Just $ (\t -> t @? encodeTaskValue) <$> updates
	notify _ _ _ _ = True

//Filtered views on the instance index

taskInstance :: SDSLens (InstanceNo,Bool,Bool,Bool) TaskMeta TaskMeta
taskInstance = sdsLens "taskInstance" param (SDSRead read) (SDSWriteConst write) (SDSNotifyConst notify) Nothing taskListMetaData
where
	param (no,includeTaskIO,includeProgress,includeTaskAttributes)
		# tfilter = {TaskListFilter|fullTaskListFilter & onlyTaskId = Just [TaskId no 0],includeProgress=includeProgress,includeTaskAttributes=includeTaskAttributes}
		# efilter = {ExtendedTaskListFilter|fullExtendedTaskListFilter & includeTaskIO=includeTaskIO}
		= (TaskId 0 0, TaskId no 0, tfilter,efilter)
	read (no,_,_,_) (_,[meta]) = Ok meta
	read (no,_,_,_) _ = Error (exception ("Could not find task instance "<+++ no))
	write _ data       = Ok (Just [data])
	notify _ _ _ _     = True

taskInstanceAttributes :: SDSLens InstanceNo (TaskAttributes,TaskAttributes) (TaskAttributes,TaskAttributes)
taskInstanceAttributes = sdsLens "taskInstanceAttributes" param (SDSRead read) (SDSWrite write) (SDSNotifyConst notify) Nothing taskListMetaData
where
	param no = (TaskId 0 0, TaskId no 0, {TaskListFilter|defaultValue & onlyTaskId = Just [TaskId no 0]},defaultValue)

	read no (_,[{TaskMeta|taskAttributes,managementAttributes}]) = Ok (taskAttributes,managementAttributes)
	read no _ = Error (exception ("Could not find attributes for task instance "<+++ no))

	write no (_,[meta]) (taskAttributes,managementAttributes) = Ok (Just [{TaskMeta|meta & taskAttributes = taskAttributes, managementAttributes = managementAttributes}])
	notify no _ _ _ = True

//Last computed value for task instance
taskInstanceValue :: SDSLens InstanceNo (TaskValue DeferredJSON) (TaskValue DeferredJSON)
taskInstanceValue = sdsLens "taskInstanceValue" param (SDSRead read) (SDSWrite write) (SDSNotifyConst notify) Nothing taskListDynamicValueData
where
	param no = (TaskId 0 0, TaskId no 0, {TaskListFilter|defaultValue & onlyTaskId = Just [TaskId no 0]}, defaultValue)

	read no values = maybe (Error $ exception ("Could not find value for task instance "<+++ no)) Ok ('DM'.get (TaskId no 0) values)
	write no values value = Ok $ Just $ 'DM'.put (TaskId no 0) value values
	notify _  _ _ _ = True

taskInstanceTask :: SDSLens InstanceNo (Task DeferredJSON) (Task DeferredJSON)
taskInstanceTask = sdsLens "taskInstanceTask" param (SDSRead read) (SDSWrite write) (SDSNotifyConst notify) Nothing taskListDynamicTaskData
where
	param no = (TaskId 0 0, TaskId no 0, {TaskListFilter|defaultValue & onlyTaskId = Just [TaskId no 0]}, defaultValue)

	read no tasks = maybe (Error $ exception ("Could not find task for task instance "<+++ no)) Ok ('DM'.get (TaskId no 0) tasks)
	write no tasks task = Ok $ Just $ 'DM'.put (TaskId no 0) task tasks
	notify _  _ _ _ = True

parallelTaskList :: SDSLens (!TaskId,!TaskId,!TaskListFilter) (!TaskId,![TaskListItem a]) [(TaskId,TaskAttributes)] | iTask a
parallelTaskList
	= sdsLens "parallelTaskList" param (SDSRead read) (SDSWrite write) (SDSNotifyConst notify) Nothing
		(taskListMetaData >*| taskListTypedValueData)
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

	notify _ _ _ _ = True

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

toTaskListItem :: !TaskId !TaskMeta -> TaskListItem a
toTaskListItem selfId {TaskMeta|taskId=taskId=:(TaskId instanceNo taskNo),instanceType
	,attachedTo,instanceKey,firstEvent,lastEvent,taskAttributes,managementAttributes}
	# listId = case instanceType of (PersistentInstance (Just listId)) = listId ; _ = (TaskId 0 0)
	= {TaskListItem|taskId = taskId, listId = listId, detached = taskNo == 0, self = taskId == selfId
	  ,value = NoValue, taskAttributes = 'DM'.union taskAttributes progressAttributes, managementAttributes = managementAttributes}
where
	progressAttributes = 'DM'.fromList
		[("attachedTo",toJSON attachedTo)
		,("instanceKey",toJSON instanceKey)
		,("firstEvent",toJSON firstEvent)
		,("lastEvent",toJSON lastEvent)
		]

taskInstanceParallelTaskList :: SDSLens (TaskId,TaskListFilter) (TaskId,[TaskMeta]) [TaskMeta]
taskInstanceParallelTaskList = sdsTranslate "taskInstanceParallelTaskList" param taskListMetaData
where
	param (listId,listfilter) = (listId,listId,listfilter,defaultValue)

taskInstanceParallelTaskListValues :: SDSLens (TaskId,TaskListFilter) (Map TaskId (TaskValue a)) (Map TaskId (TaskValue a)) | iTask a
taskInstanceParallelTaskListValues
	= sdsTranslate "taskInstanceParallelTaskListValues" param taskListTypedValueData
where
	param (listId,listfilter) = (listId,listId,listfilter,defaultValue)

taskInstanceParallelTaskListTasks :: SDSLens (TaskId,TaskListFilter) (Map TaskId (Task a)) (Map TaskId (Task a)) | iTask a
taskInstanceParallelTaskListTasks
	= sdsTranslate "taskInstanceParallelTaskListTasks" param taskListTypedTaskData
where
	param (listId,listfilter) = (listId,listId,listfilter,defaultValue)

taskInstanceParallelTaskListItem :: SDSLens (TaskId,TaskId,Bool) TaskMeta TaskMeta
taskInstanceParallelTaskListItem = sdsLens "taskInstanceParallelTaskListItem" param (SDSRead read) (SDSWrite write) (SDSNotifyConst notify) (Just reducer) taskInstanceParallelTaskList
where
	param (listId,taskId,includeValue) = (listId,{TaskListFilter|fullTaskListFilter & onlyTaskId=Just [taskId], includeValue = includeValue})
	read p=:(_,taskId,_) (listId,[x]) = Ok x
	read p=:(listId,taskId,_) (_,_) = Error (exception ("Could not find parallel task " <+++ taskId <+++ " in list " <+++ listId))

	write (_,taskId,_) (_,list) pts = Ok (Just [pts])
	notify (listId,taskId,_) _ _ _ = True
	reducer p=:(listId,_,_) ws = read p (listId,ws)

taskInstanceParallelTaskListValue :: SDSLens (TaskId,TaskId) (TaskValue a) (TaskValue a) | iTask a
taskInstanceParallelTaskListValue
	= sdsLens "taskInstanceParallelTaskListValue" param (SDSRead read) (SDSWrite write) (SDSNotifyConst notify)
		(Just reducer) taskInstanceParallelTaskListValues
where
	param (listId,taskId)
		= (listId,{TaskListFilter|fullTaskListFilter & onlyTaskId=Just [taskId],includeValue=True})
	read p=:(listId,taskId) values = case 'DM'.get taskId values of
		(Just x) = (Ok x)
		_        = Error (exception ("Could not find parallel task " <+++ taskId <+++ " in list " <+++ listId))
	write (_,taskId) values value = Ok (Just ('DM'.put taskId value values))
	notify (listId,taskId) _ _ _ = True
	reducer p ws = read p ws

taskInstanceParallelTaskListTask :: SDSLens (TaskId,TaskId) (Task a) (Task a) | iTask a
taskInstanceParallelTaskListTask
	= sdsLens "taskInstanceParallelTaskListTask" param (SDSRead read) (SDSWrite write) (SDSNotifyConst notify)
		(Just reducer) taskInstanceParallelTaskListTasks
where
	param (listId,taskId)
		= (listId,{TaskListFilter|fullTaskListFilter & onlyTaskId=Just [taskId]})
	read p=:(listId,taskId) tasks = case 'DM'.get taskId tasks of
		(Just x) = (Ok x)
		_        = Error (exception ("Could not find parallel task " <+++ taskId <+++ " in list " <+++ listId))
	write (_,taskId) tasks task = Ok (Just ('DM'.put taskId task tasks))
	notify (listId,taskId) _ _ _ = True
	reducer p ws = read p ws

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

updateInstanceConnect :: !String ![InstanceNo] !*IWorld -> *(!MaybeError TaskException (), !*IWorld)
updateInstanceConnect client instances iworld=:{IWorld|clock}
	= updateInstanceIO (\tm -> {TaskMeta|tm & lastIO = Just clock, connectedTo = Just client}) instances iworld

updateInstanceLastIO ::![InstanceNo] !*IWorld -> *(!MaybeError TaskException (), !*IWorld)
updateInstanceLastIO instances iworld=:{IWorld|clock}
	= updateInstanceIO (\tm -> {TaskMeta|tm & lastIO = Just clock}) instances iworld

updateInstanceDisconnect :: ![InstanceNo] !*IWorld -> *(!MaybeError TaskException (), !*IWorld)
updateInstanceDisconnect instances iworld=:{IWorld|clock}
	= updateInstanceIO (\tm -> {TaskMeta|tm & lastIO = Just clock, connectedTo = Nothing}) instances iworld

updateInstanceIO update instances iworld=:{IWorld|clock}
	= case modify (map update o snd) sds EmptyContext iworld of
		(Ok (ModifyingDone _),iworld) = (Ok (), iworld)
		(Error e,iworld) = (Error e,iworld)
where
	sds = sdsFocus (TaskId 0 0, TaskId 0 0, listFilter, defaultValue) taskListMetaData
	listFilter = {TaskListFilter|defaultValue & onlyTaskId = Just [TaskId n 0 \\ n <- instances]}

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
