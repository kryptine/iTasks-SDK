implementation module iTasks.API.Core.SDSs

import StdList, StdBool, StdFile, StdTuple
import System.Time, Text, Data.Tuple, Data.Functor, Data.Error, System.File
import iTasks.Framework.Store, iTasks.Framework.TaskStore, iTasks.Framework.Util
import iTasks.Framework.Task
import iTasks.Framework.IWorld
import iTasks.API.Core.Types
import iTasks.API.Core.SDSCombinators, iTasks.API.Common.SDSCombinators

from StdFunc					import o, seq, const
from iTasks.Framework.Util as iFU import qualified currentTimestamp, dateToTimestamp
from iTasks.Framework.TaskEval import topListShare, currentInstanceShare

import qualified Data.Map as DM
derive JSONEncode InstanceFilter

SYSTEM_DATA_NS :== "SystemData"

sharedStore :: !String !a -> Shared a | JSONEncode{|*|}, JSONDecode{|*|}, TC a
sharedStore storeId defaultV = singleValueStoreSDS NS_APPLICATION_SHARES storeId True True (Just defaultV)

constShare :: !a -> ROShared p a
constShare v = createReadOnlySDS (\_ env -> (v, env))

null :: WriteOnlyShared a
null = createReadWriteSDS SYSTEM_DATA_NS "null" (\Void env -> (Ok Void, env)) (\Void _ env -> (Ok (const False), env))
			
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

// Workflow processes
topLevelTasks :: SharedTaskList Void
topLevelTasks = topListShare

currentSessions ::ReadOnlyShared [TaskListItem Void]
currentSessions
    = mapRead (map toTaskListItem) (toReadOnly (sdsFocus {InstanceFilter|instanceNo=Nothing,session=Just True} filteredInstanceMeta))

currentProcesses ::ReadOnlyShared [TaskListItem Void]
currentProcesses
    = mapRead (map toTaskListItem) (toReadOnly (sdsFocus {InstanceFilter|instanceNo=Nothing,session=Just False} filteredInstanceMeta))

toTaskListItem :: !TIMeta -> TaskListItem a
toTaskListItem {TIMeta|instanceNo,listId,progress,attributes}
	= {taskId = TaskId instanceNo 0, listId = listId, value = NoValue, progressMeta = Just progress, attributes = attributes}

taskInstanceFromTIMeta :: TIMeta -> TaskInstance
taskInstanceFromTIMeta {TIMeta|instanceNo,instanceKey,session,listId,build,progress={ProgressMeta|value,issuedAt,issuedBy,involvedUsers,firstEvent,lastEvent,connectedTo,lastIO},attributes}
    = {TaskInstance|instanceNo = instanceNo, instanceKey = instanceKey, session = session, listId = listId, build = build
      ,attributes = attributes, value = value, issuedAt = issuedAt, issuedBy = issuedBy, involvedUsers = involvedUsers, firstEvent = firstEvent, lastEvent = lastEvent, connectedTo = connectedTo,lastIO = lastIO}

processesForCurrentUser	:: ReadOnlyShared [TaskListItem Void]
processesForCurrentUser = mapRead readPrj (currentProcesses >+| currentUser)
where
	readPrj (items,user)	= filter (forWorker user) items

    forWorker user {TaskListItem|attributes} = case 'DM'.get "user" attributes of
        Just uid1 = case user of
            (AuthenticatedUser uid2 _ _)    = uid1 == uid2
            _                               = False
        Nothing = case 'DM'.get "role" attributes of
            Just role = case user of
                (AuthenticatedUser _ roles _)   = isMember role roles
                _                               = False
            Nothing = True

allTaskInstances :: ROShared Void [TaskInstance]
allTaskInstances
    = toReadOnly
      (sdsProject (SDSLensRead readTIItems) SDSNoWrite
       (sdsFocus {InstanceFilter|instanceNo=Nothing,session=Nothing} filteredInstanceMeta))
where
    readTIItems is = Ok (map taskInstanceFromTIMeta is)

detachedTaskInstances :: ROShared Void [TaskInstance]
detachedTaskInstances
    = toReadOnly
      (sdsProject (SDSLensRead readTIItems) SDSNoWrite
       (sdsFocus {InstanceFilter|instanceNo=Nothing,session=Just False} filteredInstanceMeta))
where
    readTIItems is = Ok (map taskInstanceFromTIMeta is)

taskInstanceByNo :: RWShared InstanceNo TaskInstance TaskAttributes
taskInstanceByNo
    = sdsProject (SDSLensRead readTIItem) (SDSLensWrite writeTIItem)
      (sdsTranslate "taskInstanceByNo" (\instanceNo -> {InstanceFilter|instanceNo=Just instanceNo,session=Nothing}) filteredInstanceMeta)
where
    readTIItem [i]    = Ok (taskInstanceFromTIMeta i)
    readTIItem _      = Error (exception "Task instance not found")

    writeTIItem [i] a = Ok (Just [{TIMeta|i &attributes = a}])
    writeTIItem _ _   = Error (exception "Task instance not found")

currentUser :: ReadOnlyShared User
currentUser = createReadOnlySDS (\Void iworld=:{current={user}} -> (user,iworld))

currentTopTask :: ReadOnlyShared TaskId
currentTopTask = mapRead (\currentInstance -> TaskId currentInstance 0) currentInstanceShare
		
applicationName :: ReadOnlyShared String
applicationName = createReadOnlySDS appName
where
	appName Void iworld=:{IWorld|server={serverName}} = (serverName,iworld)

applicationBuild :: ReadOnlyShared String
applicationBuild = createReadOnlySDS appBuild
where
	appBuild Void iworld=:{IWorld|server={buildID}} = (buildID,iworld)

applicationDirectory :: ReadOnlyShared FilePath
applicationDirectory = createReadOnlySDS appDir
where
	appDir Void iworld=:{IWorld|server={paths={appDirectory}}} = (appDirectory,iworld)

applicationConfig :: ReadOnlyShared Config
applicationConfig = createReadOnlySDS config
where
	config Void iworld=:{IWorld|config} = (config,iworld)

storeNamespaces :: ROShared Void [String]
storeNamespaces = createReadOnlySDS read
where
    read Void iworld = listStoreNamespaces iworld

storeNames :: ROShared String [String]
storeNames = createReadOnlySDSError read
where
    read namespace iworld = case listStoreNames namespace iworld of
        (Ok names,iworld) = (Ok names,iworld)
        (Error msg,iworld) = (Error (dynamic msg,msg),iworld)

// Random source

randomInt	:: ReadOnlyShared Int
randomInt = createReadOnlySDS randomInt
where
	randomInt Void iworld=:{IWorld|random=[i:is]}
		= (i, {IWorld|iworld & random = is})

externalFile :: !FilePath -> Shared String
externalFile path = createReadWriteSDS "externalFile" path read write
where
	read Void iworld=:{world}
		# (ok,file,world)			= fopen path FReadData iworld.world
		| not ok					= (Ok "", {IWorld|iworld & world = world}) // empty string if file doesn't exist
		# (res,file)				= readAll file
		# (ok,world)				= fclose file world
		| not ok					= (Error (dynamic CannotClose,toString CannotClose) ,{IWorld|iworld & world = world})
        = case res of
            Error e                 = (Error (dynamic e,toString e) ,{IWorld|iworld & world = world})
            Ok content              = (Ok content, {IWorld|iworld & world = world})

	write Void content iworld=:{world}
		# (ok,file,world)			= fopen path FWriteText world
		| not ok					= (Error (dynamic CannotOpen,toString CannotOpen), {IWorld|iworld & world = world})
		# file						= fwrites content file
		# (ok,world)				= fclose file world
		| not ok					= (Error (dynamic CannotClose,toString CannotClose) ,{IWorld|iworld & world = world})
		= (Ok (const True), {IWorld|iworld & world = world})
