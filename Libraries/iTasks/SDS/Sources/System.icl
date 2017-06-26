implementation module iTasks.SDS.Sources.System

import iTasks.SDS.Definition
import iTasks.SDS.Combinators.Core
import iTasks.SDS.Combinators.Common
import iTasks.API.Core.Types
import System.Time

import iTasks._Framework.SDS
import iTasks._Framework.IWorld
import iTasks._Framework.Util

import iTasks._Framework.TaskStore
import StdTuple
from iTasks._Framework.TaskEval  import currentInstanceShare

NS_SYSTEM_DATA :== "SystemData"

currentDateTime :: SDS () DateTime ()
currentDateTime = mapRead (\(d,t) -> toDateTime d t) (iworldLocalDate |+| iworldLocalTime)

currentTime :: SDS () Time ()
currentTime = toReadOnly iworldLocalTime
		
currentDate :: SDS () Date ()
currentDate = toReadOnly iworldLocalDate

currentUTCDateTime :: SDS () DateTime ()
currentUTCDateTime = mapRead (\(d,t) -> toDateTime d t) (iworldUTCDate |+| iworldUTCTime)

currentUTCTime :: SDS () Time ()
currentUTCTime = toReadOnly iworldUTCTime

currentUTCDate :: SDS () Date ()
currentUTCDate = toReadOnly iworldUTCDate

currentTimestamp :: SDS () Timestamp ()
currentTimestamp = mapRead datetimeToTimestamp currentUTCDateTime


// Workflow processes
topLevelTasks :: SharedTaskList ()
topLevelTasks = topLevelTaskList

currentSessions :: SDS () [TaskListItem ()] ()
currentSessions
    = mapRead (map toTaskListItem) (toReadOnly (sdsFocus filter filteredInstanceIndex))
where
    filter = {InstanceFilter|onlyInstanceNo=Nothing,notInstanceNo=Nothing,onlySession=Just True,matchAttribute=Nothing
             ,includeConstants=True,includeProgress=True,includeAttributes=True}

currentProcesses :: SDS () [TaskListItem ()] ()
currentProcesses
    = mapRead (map toTaskListItem) (toReadOnly (sdsFocus filter filteredInstanceIndex))
where
    filter = {InstanceFilter|onlyInstanceNo=Nothing,notInstanceNo=Nothing,onlySession=Just False,matchAttribute=Nothing
             ,includeConstants=True,includeProgress=True,includeAttributes=True}

toTaskListItem :: !InstanceData -> TaskListItem a
toTaskListItem (instanceNo,Just {InstanceConstants|listId},Just progress, Just attributes) //TODO Set self for current evaluating instance
	= {TaskListItem|taskId = TaskId instanceNo 0, listId = listId, detached = True, self = False, value = NoValue, progress = Just progress, attributes = attributes}

taskInstanceFromInstanceData :: InstanceData -> TaskInstance
taskInstanceFromInstanceData (instanceNo,Just {InstanceConstants|session,listId,build,issuedAt},Just progress=:{InstanceProgress|value,instanceKey,firstEvent,lastEvent},Just attributes)
    = {TaskInstance|instanceNo = instanceNo, instanceKey = instanceKey, session = session, listId = listId, build = build
      ,attributes = attributes, value = value, issuedAt = issuedAt, firstEvent = firstEvent, lastEvent = lastEvent}

currentTaskInstanceNo :: SDS () InstanceNo ()
currentTaskInstanceNo = createReadOnlySDS (\() iworld=:{current={taskInstance}} -> (taskInstance,iworld))

currentTaskInstanceAttributes :: SDS () TaskAttributes TaskAttributes
currentTaskInstanceAttributes
	= sdsSequence "currentTaskInstanceAttributes" 
		(\_ no -> no) snd (SDSWriteConst (\_ _ -> Ok Nothing))  (SDSWriteConst (\no w -> (Ok (Just w))))
		currentTaskInstanceNo
		taskInstanceAttributes

allTaskInstances :: SDS () [TaskInstance] ()
allTaskInstances
    = toReadOnly
      (sdsProject (SDSLensRead readInstances) SDSNoWrite
       (sdsFocus {InstanceFilter|onlyInstanceNo=Nothing,notInstanceNo=Nothing,onlySession=Nothing,matchAttribute=Nothing,includeConstants=True,includeProgress=True,includeAttributes=True} filteredInstanceIndex))
where
    readInstances is = Ok (map taskInstanceFromInstanceData is)

detachedTaskInstances :: SDS () [TaskInstance] ()
detachedTaskInstances
    = toReadOnly
      (sdsProject (SDSLensRead readInstances) SDSNoWrite
       (sdsFocus {InstanceFilter|onlyInstanceNo=Nothing,notInstanceNo=Nothing,onlySession=Just False,matchAttribute=Nothing,includeConstants=True,includeProgress=True,includeAttributes=True} filteredInstanceIndex))
where
    readInstances is = Ok (map taskInstanceFromInstanceData is)

taskInstanceByNo :: SDS InstanceNo TaskInstance TaskAttributes
taskInstanceByNo
    = sdsProject (SDSLensRead readItem) (SDSLensWrite writeItem)
      (sdsTranslate "taskInstanceByNo" filter filteredInstanceIndex)
where
    filter no = {InstanceFilter|onlyInstanceNo=Just [no],notInstanceNo=Nothing,onlySession=Nothing,matchAttribute=Nothing,includeConstants=True,includeProgress=True,includeAttributes=True}

    readItem [i]    = Ok (taskInstanceFromInstanceData i)
    readItem _      = Error (exception "Task instance not found")

    writeItem [(n,c,p,_)] a = Ok (Just [(n,c,p,Just a)])
    writeItem _ _   = Error (exception "Task instance not found")

taskInstanceAttributesByNo :: SDS InstanceNo TaskAttributes TaskAttributes
taskInstanceAttributesByNo
    = sdsProject (SDSLensRead readItem) (SDSLensWrite writeItem)
      (sdsTranslate "taskInstanceAttributesByNo" filter filteredInstanceIndex)
where
    filter no = {InstanceFilter|onlyInstanceNo=Just [no],notInstanceNo=Nothing,onlySession=Nothing,matchAttribute=Nothing,includeConstants=False,includeProgress=False,includeAttributes=True}

    readItem [(_,_,_,Just a)]    = Ok a
    readItem _      = Error (exception "Task instance not found")

    writeItem [(n,c,p,_)] a = Ok (Just [(n,c,p,Just a)])
    writeItem _ _   = Error (exception "Task instance not found")

taskInstancesByAttribute :: SDS (!String,!String) [TaskInstance] ()
taskInstancesByAttribute 
    = toReadOnly
      (sdsProject (SDSLensRead readInstances) SDSNoWrite
       (sdsTranslate "taskInstancesByAttribute" (\p -> {InstanceFilter|onlyInstanceNo=Nothing,notInstanceNo=Nothing,onlySession=Nothing,matchAttribute=Just p,includeConstants=True,includeProgress=True,includeAttributes=True}) filteredInstanceIndex))
where
    readInstances is = Ok (map taskInstanceFromInstanceData is)

currentTopTask :: SDS () TaskId ()
currentTopTask = mapRead (\currentInstance -> TaskId currentInstance 0) currentInstanceShare
		
applicationName :: SDS () String ()
applicationName = createReadOnlySDS appName
where
	appName () iworld=:{IWorld|server={serverName}} = (serverName,iworld)

applicationBuild :: SDS () String ()
applicationBuild = createReadOnlySDS appBuild
where
	appBuild () iworld=:{IWorld|server={buildID}} = (buildID,iworld)

applicationDirectory :: SDS () FilePath ()
applicationDirectory = createReadOnlySDS appDir
where
	appDir () iworld=:{IWorld|server={paths={appDirectory}}} = (appDirectory,iworld)

applicationConfig :: SDS () Config ()
applicationConfig = createReadOnlySDS config
where
	config () iworld=:{IWorld|config} = (config,iworld)

