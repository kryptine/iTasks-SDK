implementation module SystemData

import SystemTypes, Time, Shared, Util, Text, Task, Tuple
import Random
import StdList, StdBool
from StdFunc		import o, seq
from IWorld			import :: IWorld(..)
from Util			import qualified currentDate, currentTime, currentDateTime, currentTimestamp, dateToTimestamp

sharedStore :: !String !a -> Shared a | JSONEncode{|*|}, JSONDecode{|*|}, TC a
sharedStore storeId defaultV = createBasicSDS
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
currentDateTime = createReadOnlySDS "SystemData" "currentDateTime" 'Util'.currentDateTime
		
currentTime :: ReadOnlyShared Time
currentTime = createReadOnlySDS "SystemData" "currentTime" 'Util'.currentTime 
		
currentDate :: ReadOnlyShared Date
currentDate = createReadOnlySDS "SystemData" "currentDate" 'Util'.currentDate

// Workflow processes
topLevelTasks :: SharedTaskList Void
topLevelTasks = createReadOnlySDS "taskList" "tasklist-top" read
where
	read iworld
		# (list, iworld) = loadValue NS_TASK_INSTANCES "persistent-index" iworld
		= ({TaskList|listId = TopLevelTaskList, items = fromMaybe [] list}, iworld)
		
currentProcesses ::ReadOnlyShared [TaskListItem Void]
currentProcesses = createReadOnlySDS "SystemData" "processes" read
where
	read iworld
		# (list, iworld) = loadValue NS_TASK_INSTANCES "persistent-index" iworld
		= (fromMaybe [] list, iworld)

processesForCurrentUser	:: ReadOnlyShared [TaskListItem Void]
processesForCurrentUser = createReadOnlySDS "SystemData" "processesForCurrentUser" read
where
	read iworld=:{currentUser}
		# (list, iworld) = loadValue NS_TASK_INSTANCES "persistent-index" iworld
		= (maybe [] (\l -> [ p \\ p <- l | forWorker currentUser p]) list, iworld)
		
	forWorker user {managementMeta=Just {worker=AnyUser}}										= True
	forWorker (AuthenticatedUser uid1 _ _) {managementMeta=Just {worker=UserWithId uid2}}		= uid1 == uid2
	forWorker (AuthenticatedUser _ roles _) {managementMeta=Just {worker=UserWithRole role}}	= isMember role roles
	forWorker _ _																				= False

currentUser :: ReadOnlyShared User
currentUser = createReadOnlySDS "SystemData" "currentUser" (\iworld=:{currentUser} -> (currentUser,iworld))

currentTopTask :: ReadOnlyShared TaskId
currentTopTask = createReadOnlySDS "SystemData" "currentTopTask" (\iworld=:{currentInstance} -> (TaskId currentInstance 0,iworld))
		
applicationName :: ReadOnlyShared String
applicationName = createReadOnlySDS "SystemData" "applicationName" appName
where
	appName iworld=:{IWorld|application} = (application,iworld)

applicationBuild:: ReadOnlyShared String
applicationBuild  = createReadOnlySDS "SystemData" "applicationBuild" appBuild
where
	appBuild iworld=:{IWorld|build} = (build,iworld)

applicationDirectory :: ReadOnlyShared FilePath
applicationDirectory = createReadOnlySDS "SystemData" "applicationDirectory" appDir
where
	appDir iworld=:{IWorld|appDirectory} = (appDirectory,iworld)

applicationConfig :: ReadOnlyShared Config
applicationConfig = createReadOnlySDS "SystemData" "config" config
where
	config iworld=:{IWorld|config} = (config,iworld)

// Random source
randomInt	:: ReadOnlyShared Int
randomInt = createReadOnlySDS "SystemData" "randomInt" randomInt
where
	randomInt iworld=:{IWorld|world}
		# (Clock seed, world)	= clock world
		= (hd (genRandInt seed), {IWorld|iworld & world = world})
