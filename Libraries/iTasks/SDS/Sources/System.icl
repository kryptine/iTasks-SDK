implementation module iTasks.SDS.Sources.System

import iTasks.SDS.Definition
import iTasks.SDS.Combinators.Core
import iTasks.SDS.Combinators.Common
import iTasks.Extensions.DateTime
import System.Time

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

derive gText ExtendedTaskListFilter
derive JSONEncode ExtendedTaskListFilter
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

toTaskListItem :: !TaskId !TaskMeta -> TaskListItem a 
toTaskListItem selfId {TaskMeta|taskId,instanceType,valuestatus,attachedTo,instanceKey,firstEvent,lastEvent,taskAttributes,managementAttributes}
	# listId = case instanceType of (PersistentInstance (Just listId)) = listId ; _ = (TaskId 0 0)
	= {TaskListItem|taskId = taskId, listId = listId, detached = True, self = taskId == selfId
	  ,value = NoValue, progress = Just progress, attributes = mergeTaskAttributes (taskAttributes,managementAttributes)}
where
	progress = {InstanceProgress|value=valuestatus,attachedTo=attachedTo,instanceKey=instanceKey,firstEvent=firstEvent,lastEvent=lastEvent}

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
currentTaskInstanceAttributes
	= sdsSequence "currentTaskInstanceAttributes"
		id
		(\_ no -> no)
		(\_ _ -> Right snd)
		(SDSWriteConst (\_ _ -> Ok Nothing))
    (SDSWrite (\no r w -> (Ok (Just w))))
		currentTaskInstanceNo
		taskInstanceAttributesByNo

allTaskInstances :: SDSLens () [TaskInstance] ()
allTaskInstances
    = (sdsProject (SDSLensRead readInstances) (SDSBlindWrite \_. Ok Nothing) Nothing
       (sdsFocus param taskListMetaData))
where
	self = TaskId 0 0
	param = (TaskId 0 0,self,defaultValue,defaultValue)

    readInstances (_,is) = Ok (map taskInstanceFromMetaData is)

detachedTaskInstances :: SDSLens () [TaskInstance] ()
detachedTaskInstances
    =  (sdsProject (SDSLensRead readInstances) (SDSBlindWrite \_. Ok Nothing) Nothing
       (sdsFocus param taskListMetaData))
where
	self = TaskId 0 0
	param = (TaskId 0 0,self,defaultValue,efilter)
	efilter = {ExtendedTaskListFilter|defaultValue & includeSessions = False, includeDetached = True, includeStartup = False}

    readInstances (_,is) = Ok (map taskInstanceFromMetaData is)

taskInstanceByNo :: SDSLens InstanceNo TaskInstance TaskAttributes
taskInstanceByNo
    = sdsProject (SDSLensRead readItem) (SDSLensWrite writeItem) Nothing
      (sdsTranslate "taskInstanceByNo" param taskListMetaData)
where
	self = TaskId 0 0
	param no = (TaskId 0 0,self,tfilter no,defaultValue)
	tfilter no = {TaskListFilter|defaultValue & onlyTaskId = Just [TaskId no 0]}

    readItem (_,[i]) = Ok (taskInstanceFromMetaData i)
    readItem _      = Error (exception "Task instance not found")

    writeItem (_,[meta]) new = Ok (Just [{TaskMeta|meta & managementAttributes = 'DM'.union new meta.TaskMeta.managementAttributes}])
    writeItem _ _ = Error (exception "Task instance not found")

taskInstanceAttributesByNo :: SDSLens InstanceNo TaskAttributes TaskAttributes
taskInstanceAttributesByNo
    = sdsProject (SDSLensRead readItem) (SDSLensWrite writeItem) Nothing
      (sdsTranslate "taskInstanceAttributesByNo" param taskListMetaData)
where
	self = TaskId 0 0
	param no = (TaskId 0 0,self,tfilter no,defaultValue)
	tfilter no = {TaskListFilter|defaultValue & onlyTaskId = Just [TaskId no 0]}

    readItem (_,[{TaskMeta|taskAttributes,managementAttributes}]) = Ok (mergeTaskAttributes (taskAttributes,managementAttributes))
    readItem _ = Error (exception "Task instance not found")

    writeItem (_,[meta]) new = Ok (Just [{TaskMeta|meta & managementAttributes = 'DM'.union new meta.TaskMeta.managementAttributes}])
    writeItem _ _ = Error (exception "Task instance not found")

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


