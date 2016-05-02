implementation module iTasks.API.Core.SDSs

import StdList, StdBool, StdFile, StdTuple
import System.Time, Text, Data.Tuple, Data.Functor, Data.Error, System.File
import iTasks._Framework.Store, iTasks._Framework.TaskStore, iTasks._Framework.Util
import iTasks._Framework.Task
import iTasks._Framework.IWorld
import iTasks.API.Core.Types
import iTasks.API.Core.SDSCombinators, iTasks.API.Common.SDSCombinators

from StdFunc					import o, seq, const, id
from iTasks._Framework.Util as iFU import qualified dateToTimestamp
from iTasks._Framework.TaskEval  import currentInstanceShare

import qualified Data.Map as DM

NS_SYSTEM_DATA :== "SystemData"

sharedStore :: !String !a -> Shared a | JSONEncode{|*|}, JSONDecode{|*|}, TC a
sharedStore storeId defaultV
    = sdsFocus storeId (jsonFileStore NS_APPLICATION_SHARES True True (Just defaultV))

constShare :: !a -> ROShared p a
constShare v = createReadOnlySDS (\_ env -> (v, env))

null :: WriteOnlyShared a
null = createReadWriteSDS NS_SYSTEM_DATA "null" (\() env -> (Ok (), env)) (\() _ env -> (Ok (const False), env))
			
currentDateTime :: ReadOnlyShared DateTime
currentDateTime = mapRead (\(d,t) -> DateTime d t) (iworldLocalDate |+| iworldLocalTime)

currentTime :: ReadOnlyShared Time
currentTime = toReadOnly iworldLocalTime
		
currentDate :: ReadOnlyShared Date
currentDate = toReadOnly iworldLocalDate

currentUTCDateTime :: ReadOnlyShared DateTime
currentUTCDateTime = mapRead (\(d,t) -> DateTime d t) (iworldUTCDate |+| iworldUTCTime)

currentUTCTime :: ReadOnlyShared Time
currentUTCTime = toReadOnly iworldUTCTime

currentUTCDate :: ReadOnlyShared Date
currentUTCDate = toReadOnly iworldUTCDate

currentTimestamp :: ReadOnlyShared Timestamp
currentTimestamp = mapRead datetimeToTimestamp currentUTCDateTime

// Workflow processes
topLevelTasks :: SharedTaskList ()
topLevelTasks = topLevelTaskList

currentSessions ::ReadOnlyShared [TaskListItem ()]
currentSessions
    = mapRead (map toTaskListItem) (toReadOnly (sdsFocus filter filteredInstanceIndex))
where
    filter = {InstanceFilter|onlyInstanceNo=Nothing,notInstanceNo=Nothing,onlySession=Just True,matchAttribute=Nothing
             ,includeConstants=True,includeProgress=True,includeAttributes=True}

currentProcesses ::ReadOnlyShared [TaskListItem ()]
currentProcesses
    = mapRead (map toTaskListItem) (toReadOnly (sdsFocus filter filteredInstanceIndex))
where
    filter = {InstanceFilter|onlyInstanceNo=Nothing,notInstanceNo=Nothing,onlySession=Just False,matchAttribute=Nothing
             ,includeConstants=True,includeProgress=True,includeAttributes=True}

toTaskListItem :: !InstanceData -> TaskListItem a
toTaskListItem (instanceNo,Just {InstanceConstants|listId},Just progress, Just attributes) //TODO Set self for current evaluating instance
	= {TaskListItem|taskId = TaskId instanceNo 0, listId = listId, detached = True, self = False, value = NoValue, progress = Just progress, attributes = attributes}

taskInstanceFromInstanceData :: InstanceData -> TaskInstance
taskInstanceFromInstanceData (instanceNo,Just {InstanceConstants|instanceKey,session,listId,build,issuedAt},Just progress=:{InstanceProgress|value,firstEvent,lastEvent},Just attributes)
    = {TaskInstance|instanceNo = instanceNo, instanceKey = instanceKey, session = session, listId = listId, build = build
      ,attributes = attributes, value = value, issuedAt = issuedAt, firstEvent = firstEvent, lastEvent = lastEvent}

currentTaskInstanceNo :: ROShared () InstanceNo
currentTaskInstanceNo = createReadOnlySDS (\() iworld=:{current={taskInstance}} -> (taskInstance,iworld))

currentTaskInstanceAttributes :: RWShared () TaskAttributes TaskAttributes
currentTaskInstanceAttributes
	= sdsSequence "currentTaskInstanceAttributes" 
		(\_ no -> no) snd (SDSWriteConst (\_ _ -> Ok Nothing))  (SDSWriteConst (\no w -> (Ok (Just w))))
		currentTaskInstanceNo
		taskInstanceAttributes
//taskInstanceAttributes          :: RWShared InstanceNo TaskAttributes TaskAttributes

allTaskInstances :: ROShared () [TaskInstance]
allTaskInstances
    = toReadOnly
      (sdsProject (SDSLensRead readInstances) SDSNoWrite
       (sdsFocus {InstanceFilter|onlyInstanceNo=Nothing,notInstanceNo=Nothing,onlySession=Nothing,matchAttribute=Nothing,includeConstants=True,includeProgress=True,includeAttributes=True} filteredInstanceIndex))
where
    readInstances is = Ok (map taskInstanceFromInstanceData is)

detachedTaskInstances :: ROShared () [TaskInstance]
detachedTaskInstances
    = toReadOnly
      (sdsProject (SDSLensRead readInstances) SDSNoWrite
       (sdsFocus {InstanceFilter|onlyInstanceNo=Nothing,notInstanceNo=Nothing,onlySession=Just False,matchAttribute=Nothing,includeConstants=True,includeProgress=True,includeAttributes=True} filteredInstanceIndex))
where
    readInstances is = Ok (map taskInstanceFromInstanceData is)

taskInstanceByNo :: RWShared InstanceNo TaskInstance TaskAttributes
taskInstanceByNo
    = sdsProject (SDSLensRead readItem) (SDSLensWrite writeItem)
      (sdsTranslate "taskInstanceByNo" filter filteredInstanceIndex)
where
    filter no = {InstanceFilter|onlyInstanceNo=Just [no],notInstanceNo=Nothing,onlySession=Nothing,matchAttribute=Nothing,includeConstants=True,includeProgress=True,includeAttributes=True}

    readItem [i]    = Ok (taskInstanceFromInstanceData i)
    readItem _      = Error (exception "Task instance not found")

    writeItem [(n,c,p,_)] a = Ok (Just [(n,c,p,Just a)])
    writeItem _ _   = Error (exception "Task instance not found")

taskInstanceAttributesByNo :: RWShared InstanceNo TaskAttributes TaskAttributes
taskInstanceAttributesByNo
    = sdsProject (SDSLensRead readItem) (SDSLensWrite writeItem)
      (sdsTranslate "taskInstanceAttributesByNo" filter filteredInstanceIndex)
where
    filter no = {InstanceFilter|onlyInstanceNo=Just [no],notInstanceNo=Nothing,onlySession=Nothing,matchAttribute=Nothing,includeConstants=False,includeProgress=False,includeAttributes=True}

    readItem [(_,_,_,Just a)]    = Ok a
    readItem _      = Error (exception "Task instance not found")

    writeItem [(n,c,p,_)] a = Ok (Just [(n,c,p,Just a)])
    writeItem _ _   = Error (exception "Task instance not found")

taskInstancesByAttribute :: ROShared (!String,!String) [TaskInstance]
taskInstancesByAttribute 
    = toReadOnly
      (sdsProject (SDSLensRead readInstances) SDSNoWrite
       (sdsTranslate "taskInstancesByAttribute" (\p -> {InstanceFilter|onlyInstanceNo=Nothing,notInstanceNo=Nothing,onlySession=Nothing,matchAttribute=Just p,includeConstants=True,includeProgress=True,includeAttributes=True}) filteredInstanceIndex))
where
    readInstances is = Ok (map taskInstanceFromInstanceData is)

currentTopTask :: ReadOnlyShared TaskId
currentTopTask = mapRead (\currentInstance -> TaskId currentInstance 0) currentInstanceShare
		
applicationName :: ReadOnlyShared String
applicationName = createReadOnlySDS appName
where
	appName () iworld=:{IWorld|server={serverName}} = (serverName,iworld)

applicationBuild :: ReadOnlyShared String
applicationBuild = createReadOnlySDS appBuild
where
	appBuild () iworld=:{IWorld|server={buildID}} = (buildID,iworld)

applicationDirectory :: ReadOnlyShared FilePath
applicationDirectory = createReadOnlySDS appDir
where
	appDir () iworld=:{IWorld|server={paths={appDirectory}}} = (appDirectory,iworld)

applicationConfig :: ReadOnlyShared Config
applicationConfig = createReadOnlySDS config
where
	config () iworld=:{IWorld|config} = (config,iworld)

storeNamespaces :: ROShared () [String]
storeNamespaces = createReadOnlySDS read
where
    read () iworld = listStoreNamespaces iworld

storeNames :: ROShared String [String]
storeNames = createReadOnlySDSError read
where
    read namespace iworld = case listStoreNames namespace iworld of
        (Ok names,iworld) = (Ok names,iworld)
        (Error e,iworld) = (Error (exception e),iworld)

// Random source
randomInt :: ROShared () Int
randomInt = createReadOnlySDS randomInt
where
	randomInt () iworld=:{IWorld|random=[i:is]}
		= (i, {IWorld|iworld & random = is})

externalFile :: RWShared FilePath String String
externalFile = createReadWriteSDS NS_SYSTEM_DATA "externalFile" read write
where
	read path iworld=:{world}
		# (ok,file,world)			= fopen path FReadData iworld.world
		| not ok					= (Ok "", {IWorld|iworld & world = world}) // empty string if file doesn't exist
		# (res,file)				= readAll file
		# (ok,world)				= fclose file world
		| not ok					= (Error (exception CannotClose) ,{IWorld|iworld & world = world})
        = case res of
            Error e                 = (Error (exception e), {IWorld|iworld & world = world})
            Ok content              = (Ok content, {IWorld|iworld & world = world})

	write path content iworld=:{world}
		# (ok,file,world)			= fopen path FWriteText world
		| not ok					= (Error (exception CannotOpen), {IWorld|iworld & world = world})
		# file						= fwrites content file
		# (ok,world)				= fclose file world
		| not ok					= (Error (exception CannotClose) ,{IWorld|iworld & world = world})
		= (Ok ((==) path), {IWorld|iworld & world = world})

