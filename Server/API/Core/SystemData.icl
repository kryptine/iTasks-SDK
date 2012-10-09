implementation module SystemData

import SystemTypes, TaskStore, Time, Shared, Util, Text, Task, Tuple
import Random
import StdList, StdBool
from StdFunc		import o, seq
from IWorld			import :: IWorld(..)
from Util			import qualified currentDate, currentTime, currentDateTime, currentTimestamp, dateToTimestamp

sharedStore :: !String !a -> Shared a | JSONEncode{|*|}, JSONDecode{|*|}, TC a
sharedStore storeId defaultV = createChangeOnWriteSDS
	"sharedStore" storeId
	(get (loadValue NS_APPLICATION_SHARES) defaultV)
	write
where	
	get f defaultV iworld
		# (mbV,iworld) = f storeId iworld
		# res = case mbV of
			Nothing	= Ok defaultV
			Just v	= Ok v
		= (res,iworld)
		
	write v iworld = (Ok Void,storeValue NS_APPLICATION_SHARES storeId v iworld)
	
currentDateTime :: ReadOnlyShared DateTime
currentDateTime = createReadOnlySDSPredictable read
where
	read iworld
		# (dateTime, iworld)		= 'Util'.currentDateTime iworld
		# (Timestamp ts, iworld)	= 'Util'.currentTimestamp iworld
		= ((dateTime, Timestamp (ts + 1)), iworld)
		
currentTime :: ReadOnlyShared Time
currentTime = createReadOnlySDSPredictable read
where
	read iworld
		# (time, iworld)			= 'Util'.currentTime iworld
		# (Timestamp ts, iworld)	= 'Util'.currentTimestamp iworld
		= ((time, Timestamp (ts + 1)), iworld)
		
currentDate :: ReadOnlyShared Date
currentDate = createReadOnlySDSPredictable read
where
	read iworld
		# (DateTime date time, iworld)	= 'Util'.currentDateTime iworld
		# (Timestamp ts, iworld)		= 'Util'.currentTimestamp iworld
		= ((date, Timestamp (ts + secondsUntilChange time)), iworld)

	secondsUntilChange {Time|hour,min,sec} = (23-hour)*3600 + (59-min)*60 + (60-sec)

// Workflow processes
topLevelTasks :: SharedTaskList Void
topLevelTasks = createReadOnlySDS read
where
	read iworld
		# (list, iworld) = loadValue NS_TASK_INSTANCES "persistent-index" iworld
		= ({TaskList|listId = TopLevelTaskList, items = fromMaybe [] list}, iworld)
		
currentProcesses ::ReadOnlyShared [TaskListItem Void]
currentProcesses = createReadOnlySDS read
where
	read iworld
		# (list, iworld) = loadValue NS_TASK_INSTANCES "persistent-index" iworld
		= (fromMaybe [] list, iworld)

processesForCurrentUser	:: ReadOnlyShared [TaskListItem Void]
processesForCurrentUser = createReadOnlySDS read
where
	read iworld=:{currentUser}
		# (list, iworld) = loadValue NS_TASK_INSTANCES "persistent-index" iworld
		= (maybe [] (\l -> [ p \\ p <- l | forWorker currentUser p]) list, iworld)
		
	forWorker user {managementMeta=Just {ManagementMeta|worker=AnyUser}}									= True
	forWorker (AuthenticatedUser uid1 _ _) {managementMeta=Just {ManagementMeta|worker=UserWithId uid2}}	= uid1 == uid2
	forWorker (AuthenticatedUser _ roles _) {managementMeta=Just {ManagementMeta|worker=UserWithRole role}}	= isMember role roles
	forWorker _ _																							= False

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
