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

import iTasks.Internal.TaskStore
import StdTuple, StdList, StdString
from iTasks.Internal.TaskEval  import currentInstanceShare
from StdFunc import id, o, const

NS_SYSTEM_DATA :== "SystemData"

currentDateTime :: SDSLens () DateTime ()
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
currentTimestamp = toReadOnly (sdsFocus {start=Timestamp 0,interval=Timestamp 1} iworldTimestamp) id

currentTimespec :: SDSLens () Timespec ()
currentTimespec = toReadOnly (sdsFocus {start=zero,interval=zero} iworldTimespec) id

// Workflow processes
topLevelTasks :: SharedTaskList ()
topLevelTasks = topLevelTaskList

currentSessions :: SDSLens () [TaskListItem ()] ()
currentSessions
    = mapRead (map toTaskListItem) (toReadOnly (sdsFocus filter filteredInstanceIndex) id)
where
    filter = {InstanceFilter|onlyInstanceNo=Nothing,notInstanceNo=Nothing,onlySession=Just True,matchAttribute=Nothing
             ,includeConstants=True,includeProgress=True,includeAttributes=True}

currentProcesses :: SDSLens () [TaskListItem ()] ()
currentProcesses
    = mapRead (map toTaskListItem) (toReadOnly (sdsFocus filter filteredInstanceIndex) id)
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

currentTaskInstanceNo :: SDSSource () InstanceNo ()
currentTaskInstanceNo = createReadOnlySDS (\() iworld=:{current={taskInstance}} -> (taskInstance,iworld))

currentTaskInstanceAttributes :: SDSSequence () TaskAttributes TaskAttributes
currentTaskInstanceAttributes
	= sdsSequence "currentTaskInstanceAttributes" 
		id
		(\_ no -> no) 
		(\_ _ -> Right snd)
		(SDSWriteConst (\_ _ -> Ok (DoNotWrite ())))  
    (SDSWrite (\no w _ -> (Ok (DoWrite w))))
    (\p ws -> Ok (snd ws)) 
		currentTaskInstanceNo
		taskInstanceAttributes

allTaskInstances :: SDSLens () [TaskInstance] ()
allTaskInstances
    = (sdsProject (SDSLensRead readInstances) (SDSLensWrite \ws _. Ok (DoNotWrite ws)) reducer
       (sdsFocus {InstanceFilter|onlyInstanceNo=Nothing,notInstanceNo=Nothing,onlySession=Nothing,matchAttribute=Nothing,includeConstants=True,includeProgress=True,includeAttributes=True} filteredInstanceIndex))
where
    readInstances is = Ok (map taskInstanceFromInstanceData is)

    reducer _ _ = Ok ()

detachedTaskInstances :: SDSLens () [TaskInstance] ()
detachedTaskInstances
    =  (sdsProject (SDSLensRead readInstances) (SDSLensWrite \ws _. Ok (DoNotWrite ws)) reducer
       (sdsFocus {InstanceFilter|onlyInstanceNo=Nothing,notInstanceNo=Nothing,onlySession=Just False,matchAttribute=Nothing,includeConstants=True,includeProgress=True,includeAttributes=True} filteredInstanceIndex))
where
    readInstances is = Ok (map taskInstanceFromInstanceData is)

    reducer _ _ = Ok ()

taskInstanceByNo :: SDSLens InstanceNo TaskInstance TaskAttributes
taskInstanceByNo
    = sdsProject (SDSLensRead readItem) (SDSLensWrite writeItem) reducer
      (sdsTranslate "taskInstanceByNo" filter filteredInstanceIndex)
where
    filter no = {InstanceFilter|onlyInstanceNo=Just [no],notInstanceNo=Nothing,onlySession=Nothing,matchAttribute=Nothing,includeConstants=True,includeProgress=True,includeAttributes=True}

    readItem [i]    = Ok (taskInstanceFromInstanceData i)
    readItem _      = Error (exception "Task instance not found")

    writeItem [(n,c,p,_)] a = Ok (DoWrite [(n,c,p,Just a)])
    writeItem _ _   = Error (exception "Task instance not found")

    reducer :: p [InstanceData] -> MaybeError TaskException TaskAttributes
    reducer _ [(_, _, _, (Just attributes))] = Ok (attributes)
    reducer _ _                              = Error (exception "Task instance not found")

taskInstanceAttributesByNo :: SDSLens InstanceNo TaskAttributes TaskAttributes
taskInstanceAttributesByNo
    = sdsProject (SDSLensRead readItem) (SDSLensWrite writeItem) reducer
      (sdsTranslate "taskInstanceAttributesByNo" filter filteredInstanceIndex)
where
    filter no = {InstanceFilter|onlyInstanceNo=Just [no],notInstanceNo=Nothing,onlySession=Nothing,matchAttribute=Nothing,includeConstants=False,includeProgress=False,includeAttributes=True}

    readItem [(_,_,_,Just a)]    = Ok a
    readItem _      = Error (exception "Task instance not found")

    writeItem [(n,c,p,_)] a = Ok (DoWrite [(n,c,p,Just a)])
    writeItem _ _   = Error (exception "Task instance not found")

    reducer :: p [InstanceData] -> MaybeError TaskException TaskAttributes
    reducer _ [(_, _, _, (Just attributes))] = Ok (attributes)
    reducer _ _                              = Error (exception "Task instance not found")

taskInstancesByAttribute :: SDSLens (!String,!String) [TaskInstance] ()
taskInstancesByAttribute 
    = 
      (sdsProject (SDSLensRead readInstances) (SDSLensWrite \ws _. Ok (DoNotWrite ws)) reducer
       (sdsTranslate "taskInstancesByAttribute" (\p -> {InstanceFilter|onlyInstanceNo=Nothing,notInstanceNo=Nothing,onlySession=Nothing,matchAttribute=Just p,includeConstants=True,includeProgress=True,includeAttributes=True}) filteredInstanceIndex))
where
    readInstances is = Ok (map taskInstanceFromInstanceData is)

    reducer _ _ = Ok ()

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


