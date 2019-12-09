implementation module iTasks.SDS.Sources.System

import iTasks.SDS.Definition
import iTasks.SDS.Combinators.Core
import iTasks.SDS.Combinators.Common
import iTasks.Extensions.DateTime //FIXME: Extensions should not be part of core
import System.Time
import Data.Func, Data.Either

import iTasks.Engine
import iTasks.Internal.SDS
import iTasks.Internal.IWorld
import iTasks.Internal.Util
import iTasks.WF.Definition
import iTasks.WF.Derives

import iTasks.Internal.TaskState
import iTasks.WF.Combinators.Core

import StdTuple, StdList, StdString
from iTasks.Internal.TaskEval  import currentInstanceShare
from StdFunc import id, o, const

import qualified Data.Map as DM

derive gDefault TaskListFilter, TaskId

NS_SYSTEM_DATA :== "SystemData"

currentDateTime :: SDSParallel () DateTime ()
currentDateTime = iworldLocalDateTime

currentTime :: SDSLens () Time ()
currentTime = mapRead toTime iworldLocalDateTime

currentDate :: SDSLens () Date ()
currentDate = mapRead toDate iworldLocalDateTime

currentUTCDateTime :: SDSLens () DateTime ()
currentUTCDateTime = mapRead timestampToGmDateTime currentTimestamp

currentUTCTime :: SDSLens () Time ()
currentUTCTime = mapRead (toTime o timestampToGmDateTime) currentTimestamp

currentUTCDate :: SDSLens () Date ()
currentUTCDate = mapRead (toDate o timestampToGmDateTime) currentTimestamp

currentTimestamp :: SDSLens () Timestamp ()
currentTimestamp = toReadOnly (sdsFocus {start=Timestamp 0,interval=Timestamp 1} iworldTimestamp)

currentTimespec :: SDSLens () Timespec ()
currentTimespec = toReadOnly (sdsFocus {start=zero,interval=zero} iworldTimespec)

// Workflow processes
topLevelTasks :: SharedTaskList ()
topLevelTasks = topLevelTaskList

currentSessions :: SDSLens () [TaskListItem ()] ()
currentSessions
    = mapRead (map (toTaskListItem self) o snd) (toReadOnly (sdsFocus param taskListMetaData))
where
	self = TaskId 0 0
	param = (TaskId 0 0,self,defaultValue,efilter)
	efilter = {ExtendedTaskListFilter|defaultValue & includeSessions = True, includeDetached = False, includeStartup = False}

currentProcesses :: SDSLens () [TaskListItem ()] ()
currentProcesses
    = mapRead (map (toTaskListItem self) o snd) (toReadOnly (sdsFocus param taskListMetaData))
where
	self = TaskId 0 0
	param = (TaskId 0 0,self,defaultValue,efilter)
	efilter = {ExtendedTaskListFilter|defaultValue & includeSessions = False, includeDetached = True, includeStartup = False}

taskInstanceFromMetaData :: TaskMeta -> TaskInstance
taskInstanceFromMetaData {TaskMeta|taskId=taskId=:(TaskId instanceNo _),instanceType,build,createdAt,valuestatus,instanceKey,firstEvent,lastEvent,taskAttributes,managementAttributes}
	# session = (instanceType =: SessionInstance )
	# listId = case instanceType of
		(PersistentInstance (Just listId)) = listId
		_ = (TaskId 0 0)
    = {TaskInstance|instanceNo = instanceNo, instanceKey = instanceKey, session = session, listId = listId, build = build
      ,attributes = mergeTaskAttributes (taskAttributes,managementAttributes), value = valuestatus, issuedAt = createdAt, firstEvent = firstEvent, lastEvent = lastEvent}

currentTaskInstanceNo :: SDSSource () InstanceNo ()
currentTaskInstanceNo = createReadOnlySDS (\() iworld=:{current={taskInstance}} -> (taskInstance,iworld))

currentTaskInstanceAttributes :: SDSSequence () TaskAttributes TaskAttributes
currentTaskInstanceAttributes= sdsSequence "currentTaskInstanceAttributes" param1 param2 read (SDSWriteConst write1) (SDSWrite write2) currentTaskInstanceNo taskListMetaData
where
	param1 _ = ()
	param2 _ selfNo = (TaskId 0 0, TaskId selfNo 0, tfilter selfNo, defaultValue)
	where
		tfilter no = {TaskListFilter|defaultValue & onlyTaskId = Just [TaskId no 0]}

	read no selfNo = Right $ \(_,(_,[{TaskMeta|taskAttributes,managementAttributes}])) -> mergeTaskAttributes (taskAttributes,managementAttributes)
	write1 _ _ = Ok Nothing
	write2 _ (_,[meta]) update = Ok $ Just $ [{TaskMeta|meta & managementAttributes = 'DM'.union update meta.TaskMeta.managementAttributes}]

allTaskInstances :: SDSSequence () [TaskInstance] ()
allTaskInstances= sdsSequence "allTaskInstances" param1 param2 read (SDSWriteConst write1) (SDSWriteConst write2) currentTaskInstanceNo taskListMetaData
where
	param1 _ = ()
	param2 _ selfNo = (TaskId 0 0,TaskId selfNo 0, fullTaskListFilter,fullExtendedTaskListFilter)
	read _ selfNo = Right $ \(_,(_,meta)) -> map taskInstanceFromMetaData meta
	write1 _ _ = Ok Nothing
	write2 _ _ = Ok Nothing

detachedTaskInstances :: SDSSequence () [TaskInstance] ()
detachedTaskInstances = sdsSequence "detachedTaskInstances" param1 param2 read (SDSWriteConst write1) (SDSWriteConst write2) currentTaskInstanceNo taskListMetaData
where
	param1 _ = ()
	param2 _ selfNo = (TaskId 0 0,TaskId selfNo 0,tfilter,efilter)
	where
		tfilter = {TaskListFilter|fullTaskListFilter & includeProgress = True, includeManagementAttributes = True}
		efilter = {ExtendedTaskListFilter|fullExtendedTaskListFilter & includeSessions = False, includeDetached = True, includeStartup = False}
	read _ selfNo = Right $ \(_,(_,meta)) -> map taskInstanceFromMetaData meta

	write1 _ _ = Ok Nothing
	write2 _ _ = Ok Nothing

taskInstanceByNo :: SDSSequence InstanceNo TaskInstance TaskAttributes
taskInstanceByNo = sdsSequence "taskInstanceByNo" param1 param2 read (SDSWriteConst write1) (SDSWrite write2) currentTaskInstanceNo taskListMetaData
where
	param1 _ = ()
	param2 no selfNo = (TaskId 0 0, TaskId selfNo 0, tfilter no, defaultValue)
	where
		tfilter no = {TaskListFilter|defaultValue & onlyTaskId = Just [TaskId no 0]}

	read no selfNo = Right $ \(_,(_,[meta])) -> taskInstanceFromMetaData meta
	write1 _ _ = Ok Nothing
	write2 no (_,[meta]) update = Ok $ Just $ [{TaskMeta|meta & managementAttributes = 'DM'.union update meta.TaskMeta.managementAttributes}]

taskInstanceAttributesByNo :: SDSSequence InstanceNo TaskAttributes TaskAttributes
taskInstanceAttributesByNo = sdsSequence "taskInstanceAttributesByNo" param1 param2 read (SDSWriteConst write1) (SDSWrite write2) currentTaskInstanceNo taskListMetaData
where
	param1 _ = ()
	param2 no selfNo = (TaskId 0 0, TaskId selfNo 0, tfilter no, defaultValue)
	where
		tfilter no = {TaskListFilter|defaultValue & onlyTaskId = Just [TaskId no 0]}

	read no selfNo = Right $ \(_,(_,[{TaskMeta|taskAttributes,managementAttributes}])) -> mergeTaskAttributes (taskAttributes,managementAttributes)
	write1 _ _ = Ok Nothing
	write2 no (_,[meta]) update = Ok $ Just $ [{TaskMeta|meta & managementAttributes = 'DM'.union update meta.TaskMeta.managementAttributes}]

taskInstancesByAttribute :: SDSLens (!String,!JSONNode) [TaskInstance] ()
taskInstancesByAttribute
    =
      (sdsProject (SDSLensRead readInstances) (SDSBlindWrite \_. Ok Nothing) Nothing
       (sdsTranslate "taskInstancesByAttribute" param taskListMetaData))
where
	self = TaskId 0 0
	param p = (TaskId 0 0,self, tfilter p, defaultValue)
	tfilter p = {TaskListFilter|defaultValue & onlyAttribute = Just p}

    readInstances (_,is) = Ok (map taskInstanceFromMetaData is)

currentTopTask :: SDSLens () TaskId ()
currentTopTask = mapRead (\currentInstance -> TaskId currentInstance 0) currentInstanceShare

applicationName :: SDSSource () String ()
applicationName = createReadOnlySDS appName
where
	appName () iworld=:{IWorld|options={EngineOptions|appName}} = (appName,iworld)

applicationVersion :: SDSSource () String ()
applicationVersion = createReadOnlySDS appBuild
where
	appBuild () iworld=:{IWorld|options={EngineOptions|appVersion}} = (appVersion,iworld)

applicationDirectory :: SDSSource () FilePath ()
applicationDirectory = createReadOnlySDS appDir
where
	appDir () iworld=:{IWorld|options={EngineOptions|appPath}} = (takeDirectory appPath,iworld)

applicationOptions :: SDSSource () EngineOptions ()
applicationOptions = createReadOnlySDS options
where
	options () iworld=:{IWorld|options} = (options,iworld)


