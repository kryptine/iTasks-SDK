implementation module iTasks.API.Core.SystemData

import StdList, StdBool, StdFile
import System.Time, Text, Data.Tuple, Data.Map, Math.Random, Data.Functor, Data.Error, System.File
import iTasks.Framework.Store, iTasks.Framework.TaskStore, iTasks.Framework.Shared, iTasks.Framework.Util
import iTasks.Framework.Task
import iTasks.API.Core.SystemTypes
from StdFunc					import o, seq
from iTasks.Framework.IWorld	import :: IWorld(..)
from iTasks.Framework.Util as iFU import qualified currentDate, currentTime, currentDateTime, currentTimestamp, dateToTimestamp

SYSTEM_DATA_NS :== "SystemData"

sharedStore :: !String !a -> Shared a | JSONEncode{|*|}, JSONDecode{|*|}, TC a
sharedStore storeId defaultV = storeAccess NS_APPLICATION_SHARES storeId (Just defaultV)

currentDateTime :: ReadOnlyShared DateTime
currentDateTime = createReadOnlySDSPredictable SYSTEM_DATA_NS "currentDateTime" read
where
	read iworld
		# (dateTime, iworld)		= 'iFU'.currentDateTime iworld
		# (Timestamp ts, iworld)	= 'iFU'.currentTimestamp iworld
		= ((dateTime, Timestamp (ts + 1)), iworld)
		
currentTime :: ReadOnlyShared Time
currentTime = createReadOnlySDSPredictable SYSTEM_DATA_NS "currentTime" read
where
	read iworld
		# (time, iworld)			= 'iFU'.currentTime iworld
		# (Timestamp ts, iworld)	= 'iFU'.currentTimestamp iworld
		= ((time, Timestamp (ts + 1)), iworld)
		
currentDate :: ReadOnlyShared Date
currentDate = createReadOnlySDSPredictable SYSTEM_DATA_NS "currentDate" read
where
	read iworld
		# (DateTime date time, iworld)	= 'iFU'.currentDateTime iworld
		# (Timestamp ts, iworld)		= 'iFU'.currentTimestamp iworld
		= ((date, Timestamp (ts + secondsUntilChange time)), iworld)

	secondsUntilChange {Time|hour,min,sec} = (23-hour)*3600 + (59-min)*60 + (60-sec)

// Workflow processes
topLevelTasks :: SharedTaskList Void
topLevelTasks = mapReadWrite (readPrj,writePrj) (detachedInstances >+| currentTopTask)
where
	readPrj (instances, taskId) = {TaskList|listId = TopLevelTaskList, items = [toTaskListItem m \\ (_,m) <- (toList instances)| not (isSession m)], selfId = taskId}
    writePrj [] instances = Nothing
    writePrj updates (instances,_) = Just (foldl applyUpdate instances updates)

    applyUpdate instances (TaskId instanceNo 0,management)
        = case get instanceNo instances of
            Just meta   = put instanceNo {TIMeta|meta&management=management} instances
            _           = instances
    applyUpdate instances _ = instances

currentSessions ::ReadOnlyShared [TaskListItem Void]
currentSessions = mapRead (\instances -> [toTaskListItem m \\ (_,m) <- (toList instances)]) (toReadOnly sessionInstances)

currentProcesses ::ReadOnlyShared [TaskListItem Void]
currentProcesses = mapRead (\instances -> [toTaskListItem m \\ (_,m) <- (toList instances)]) (toReadOnly detachedInstances)

processesForCurrentUser	:: ReadOnlyShared [TaskListItem Void]
processesForCurrentUser = mapRead readPrj (currentProcesses >+| currentUser)
where
	readPrj (items,user)	= filter (forWorker user) items

	forWorker user {managementMeta=Just {ManagementMeta|worker=AnyUser}}									= True
	forWorker (AuthenticatedUser uid1 _ _) {managementMeta=Just {ManagementMeta|worker=UserWithId uid2}}	= uid1 == uid2
	forWorker (AuthenticatedUser _ roles _) {managementMeta=Just {ManagementMeta|worker=UserWithRole role}}	= isMember role roles
	forWorker _ _																							= False

isSession :: !TIMeta -> Bool
isSession {TIMeta|session=Just _}	= True
isSession _						 	= False

toTaskListItem :: !TIMeta -> TaskListItem a 
toTaskListItem {TIMeta|instanceNo,listId,progress,management}
	= {taskId = TaskId instanceNo 0, listId = listId, name = Nothing, value = NoValue, progressMeta = Just progress, managementMeta = Just management}

currentUser :: ReadOnlyShared User
currentUser = createReadOnlySDS (\iworld=:{currentUser} -> (currentUser,iworld))

currentTopTask :: ReadOnlyShared TaskId
currentTopTask = createReadOnlySDS (\iworld=:{currentInstance} -> (TaskId currentInstance 0,iworld))
		
applicationName :: ReadOnlyShared String
applicationName = createReadOnlySDS appName
where
	appName iworld=:{IWorld|application} = (application,iworld)

applicationBuild:: ReadOnlyShared String
applicationBuild  = createReadOnlySDS appBuild
where
	appBuild iworld=:{IWorld|build} = (build,iworld)

applicationDirectory :: ReadOnlyShared FilePath
applicationDirectory = createReadOnlySDS appDir
where
	appDir iworld=:{IWorld|appDirectory} = (appDirectory,iworld)

applicationConfig :: ReadOnlyShared Config
applicationConfig = createReadOnlySDS config
where
	config iworld=:{IWorld|config} = (config,iworld)

// Random source
randomInt	:: ReadOnlyShared Int
randomInt = createReadOnlySDS randomInt
where
	randomInt iworld=:{IWorld|world}
		# (Clock seed, world)	= clock world
		= (hd (genRandInt seed), {IWorld|iworld & world = world})

EXTERNAL_FILE_POLLING_RATE :== 10

externalFile :: !FilePath -> Shared String
externalFile path = createPollingSDS "externalFile" path read write
where
	read iworld
		# (Timestamp ts, iworld)	= 'iFU'.currentTimestamp iworld
		# (res, iworld)				= read` iworld
		= (fmap (\r -> (r, Timestamp (ts + EXTERNAL_FILE_POLLING_RATE), checkF r)) res, iworld)
	
	read` iworld=:{world}
		# (ok,file,world)			= fopen path FReadData iworld.world
		| not ok					= (Ok "", {IWorld|iworld & world = world}) // empty string if file doesn't exist
		# (res,file)				= readAll file
		# (ok,world)				= fclose file world
		| not ok					= (Error (toString CannotClose) ,{IWorld|iworld & world = world})
		| isError res				= (Error (toString (fromError res)) ,{IWorld|iworld & world = world})
		= (Ok (fromOk res), {IWorld|iworld & world = world})
		
	checkF old iworld
		# (res,iworld)= read` iworld
		| isOk res && (fromOk res) <> old = (Changed, iworld)
		# (Timestamp ts, iworld) = 'iFU'.currentTimestamp iworld
		= (CheckAgain (Timestamp (ts + EXTERNAL_FILE_POLLING_RATE)), iworld)
		
	write content iworld=:{world}
		# (ok,file,world)			= fopen path FWriteText world
		| not ok					= (Error (toString CannotOpen), {IWorld|iworld & world = world})
		# file						= fwrites content file
		# (ok,world)				= fclose file world
		| not ok					= (Error (toString CannotClose) ,{IWorld|iworld & world = world})
		= (Ok Void, {IWorld|iworld & world = world})
