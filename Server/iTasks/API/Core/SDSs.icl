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

from Data.Map import toList
derive gEq TIType

SYSTEM_DATA_NS :== "SystemData"

sharedStore :: !String !a -> Shared a | JSONEncode{|*|}, JSONDecode{|*|}, TC a
sharedStore storeId defaultV = storeAccess NS_APPLICATION_SHARES storeId (Just defaultV)

constShare :: !a -> ROShared p a
constShare v = createReadOnlySDS (\_ env -> (v, env))

null :: WriteOnlyShared a
null = createSDS Nothing (\Void env -> (Ok (Void, OnWrite), env)) (\Void _ env -> (Ok (const False), env))
			
currentDateTime :: ReadOnlyShared DateTime
currentDateTime = createReadOnlySDSPredictable SYSTEM_DATA_NS "currentDateTime" read
where
	read Void iworld=:{current={localDateTime,timestamp=Timestamp ts}}
		= ((localDateTime, Timestamp (ts + 1)), iworld)
		
currentTime :: ReadOnlyShared Time
currentTime = createReadOnlySDSPredictable SYSTEM_DATA_NS "currentTime" read
where
	read Void iworld=:{current={localDateTime=DateTime _ time,timestamp=Timestamp ts}}
		= ((time, Timestamp (ts + 1)), iworld)
		
currentDate :: ReadOnlyShared Date
currentDate = createReadOnlySDSPredictable SYSTEM_DATA_NS "currentDate" read
where
	read Void iworld=:{current={localDateTime=DateTime date time,timestamp=Timestamp ts}}
		= ((date, Timestamp (ts + secondsUntilChange time)), iworld)

	secondsUntilChange {Time|hour,min,sec} = (23-hour)*3600 + (59-min)*60 + (60-sec)

currentUTCDateTime :: ReadOnlyShared DateTime
currentUTCDateTime = createReadOnlySDSPredictable SYSTEM_DATA_NS "currentUTCDateTime" read
where
	read Void iworld=:{current={utcDateTime,timestamp=Timestamp ts}}
		= ((utcDateTime, Timestamp (ts + 1)), iworld)

currentUTCTime :: ReadOnlyShared Time
currentUTCTime = createReadOnlySDSPredictable SYSTEM_DATA_NS "currentUTCTime" read
where
	read Void iworld=:{current={utcDateTime=DateTime _ time,timestamp=Timestamp ts}}
		= ((time, Timestamp (ts + 1)), iworld)

currentUTCDate :: ReadOnlyShared Date
currentUTCDate = createReadOnlySDSPredictable SYSTEM_DATA_NS "currentUTCDate" read
where
	read Void iworld=:{current={utcDateTime=DateTime date time,timestamp=Timestamp ts}}
		= ((date, Timestamp (ts + secondsUntilChange time)), iworld)

	secondsUntilChange {Time|hour,min,sec} = (23-hour)*3600 + (59-min)*60 + (60-sec)

// Workflow processes
topLevelTasks :: SharedTaskList Void
topLevelTasks = topListShare

currentSessions ::ReadOnlyShared [TaskListItem Void]
currentSessions = mapRead (\instances -> [toTaskListItem m \\ (_,m) <- (toList instances) | m.instanceType === SessionInstance]) (toReadOnly fullInstanceMeta)

currentProcesses ::ReadOnlyShared [TaskListItem Void]
currentProcesses = mapRead (\instances -> [toTaskListItem m \\ (_,m) <- (toList instances) | m.instanceType =!= SessionInstance]) (toReadOnly fullInstanceMeta)

toTaskListItem :: !TIMeta -> TaskListItem a 
toTaskListItem {TIMeta|instanceNo,listId,progress,management}
	= {taskId = TaskId instanceNo 0, listId = listId, name = Nothing, value = NoValue, progressMeta = Just progress, managementMeta = Just management}

processesForCurrentUser	:: ReadOnlyShared [TaskListItem Void]
processesForCurrentUser = mapRead readPrj (currentProcesses >+| currentUser)
where
	readPrj (items,user)	= filter (forWorker user) items

	forWorker user {managementMeta=Just {ManagementMeta|worker=AnyUser}}									= True
	forWorker (AuthenticatedUser uid1 _ _) {managementMeta=Just {ManagementMeta|worker=UserWithId uid2}}	= uid1 == uid2
	forWorker (AuthenticatedUser _ roles _) {managementMeta=Just {ManagementMeta|worker=UserWithRole role}}	= isMember role roles
	forWorker _ _																							= False

isSession :: !TIMeta -> Bool
isSession {TIMeta|instanceType=SessionInstance}	= True
isSession _						 	            = False

allTaskInstances :: ReadOnlyShared [TaskListItem Void]
allTaskInstances = createReadOnlySDS (\Void iworld=:{ti} -> (map (toTaskListItem o snd) (toList ti),iworld))

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

// Random source
randomInt	:: ReadOnlyShared Int
randomInt = createReadOnlySDS randomInt
where
	randomInt Void iworld=:{IWorld|random=[i:is]}
		= (i, {IWorld|iworld & random = is})

EXTERNAL_FILE_POLLING_RATE :== 10

externalFile :: !FilePath -> Shared String
externalFile path = createPollingSDS "externalFile" path read write
where
	read Void iworld
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
		
	write Void content iworld=:{world}
		# (ok,file,world)			= fopen path FWriteText world
		| not ok					= (Error (toString CannotOpen), {IWorld|iworld & world = world})
		# file						= fwrites content file
		# (ok,world)				= fclose file world
		| not ok					= (Error (toString CannotClose) ,{IWorld|iworld & world = world})
		= (Ok (const True), {IWorld|iworld & world = world})
