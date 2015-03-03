implementation module iTasks.API.Core.SDSs

import StdList, StdBool, StdFile, StdTuple
import System.Time, Text, Data.Tuple, Data.Functor, Data.Error, System.File
import iTasks.Framework.Store, iTasks.Framework.TaskStore, iTasks.Framework.Util
import iTasks.Framework.Task
import iTasks.Framework.IWorld
import iTasks.API.Core.Types
import iTasks.API.Core.SDSCombinators, iTasks.API.Common.SDSCombinators

from StdFunc					import o, seq, const
from iTasks.Framework.Util as iFU import qualified dateToTimestamp
from iTasks.Framework.TaskEval  import currentInstanceShare

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
topLevelTasks :: SharedTaskList Void
topLevelTasks = topLevelTaskList

currentSessions ::ReadOnlyShared [TaskListItem Void]
currentSessions
    = mapRead (map toTaskListItem) (toReadOnly (sdsFocus filter filteredInstanceIndex))
where
    filter = {InstanceFilter|onlyInstanceNo=Nothing,onlySession=Just True
             ,includeConstants=True,includeProgress=True,includeAttributes=True}

currentProcesses ::ReadOnlyShared [TaskListItem Void]
currentProcesses
    = mapRead (map toTaskListItem) (toReadOnly (sdsFocus filter filteredInstanceIndex))
where
    filter = {InstanceFilter|onlyInstanceNo=Nothing,onlySession=Just False
             ,includeConstants=True,includeProgress=True,includeAttributes=True}

toTaskListItem :: !InstanceData -> TaskListItem a
toTaskListItem (instanceNo,Just {InstanceConstants|listId},Just progress, Just attributes) //TODO Set self for current evaluating instance
	= {TaskListItem|taskId = TaskId instanceNo 0, listId = listId, detached = True, self = False, value = NoValue, progress = Just progress, attributes = attributes}

taskInstanceFromInstanceData :: InstanceData -> TaskInstance
taskInstanceFromInstanceData (instanceNo,Just {InstanceConstants|instanceKey,session,listId,build,issuedAt,issuedBy},Just progress=:{InstanceProgress|value,involvedUsers,firstEvent,lastEvent,connectedTo,lastIO},Just attributes)
    = {TaskInstance|instanceNo = instanceNo, instanceKey = instanceKey, session = session, listId = listId, build = build
      ,attributes = attributes, value = value, issuedAt = issuedAt, issuedBy = issuedBy, involvedUsers = involvedUsers, firstEvent = firstEvent, lastEvent = lastEvent, connectedTo = connectedTo,lastIO = lastIO}

processesForCurrentUser	:: ReadOnlyShared [TaskListItem Void]
processesForCurrentUser = mapRead readPrj (currentProcesses >+| currentUser)
where
	readPrj (items,user)	= filter (forWorker user) items

    forWorker user {TaskListItem|attributes} = case 'DM'.get TAUser attributes of
        Just (TAUserVal (UserWithId uid1)) = case user of
            (AuthenticatedUser uid2 _ _)    = uid1 == uid2
            _                               = False
        Nothing = case 'DM'.get TARole attributes of
            Just role = case user of
                (AuthenticatedUser _ roles _)   = isMember (toString role) roles
                _                               = False
            Nothing = True

allTaskInstances :: ROShared () [TaskInstance]
allTaskInstances
    = toReadOnly
      (sdsProject (SDSLensRead readInstances) SDSNoWrite
       (sdsFocus {InstanceFilter|onlyInstanceNo=Nothing,onlySession=Nothing,includeConstants=True,includeProgress=True,includeAttributes=True} filteredInstanceIndex))
where
    readInstances is = Ok (map taskInstanceFromInstanceData is)

detachedTaskInstances :: ROShared () [TaskInstance]
detachedTaskInstances
    = toReadOnly
      (sdsProject (SDSLensRead readInstances) SDSNoWrite
       (sdsFocus {InstanceFilter|onlyInstanceNo=Nothing,onlySession=Just False,includeConstants=True,includeProgress=True,includeAttributes=True} filteredInstanceIndex))
where
    readInstances is = Ok (map taskInstanceFromInstanceData is)

taskInstanceByNo :: RWShared InstanceNo TaskInstance TaskAttributes
taskInstanceByNo
    = sdsProject (SDSLensRead readItem) (SDSLensWrite writeItem)
      (sdsTranslate "taskInstanceByNo" filter filteredInstanceIndex)
where
    filter no = {InstanceFilter|onlyInstanceNo=Just [no],onlySession=Nothing,includeConstants=True,includeProgress=True,includeAttributes=True}

    readItem [i]    = Ok (taskInstanceFromInstanceData i)
    readItem _      = Error (exception "Task instance not found")

    writeItem [(n,c,p,_)] a = Ok (Just [(n,c,p,Just a)])
    writeItem _ _   = Error (exception "Task instance not found")

currentUser :: ReadOnlyShared User
currentUser = createReadOnlySDS (\() iworld=:{current={user}} -> (user,iworld))

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

