implementation module SystemData

import SystemTypes, Time, Shared, Util, Text, Task, Tuple
import Random
import StdList, StdBool
from StdFunc		import o, seq
from IWorld			import :: IWorld(..)
from TaskState	 	import :: ParallelControl
from Util			import qualified currentDate, currentTime, currentDateTime, currentTimestamp, dateToTimestamp

sharedStore :: !String !a -> Shared a | JSONEncode{|*|}, JSONDecode{|*|}, TC a
sharedStore storeId defaultV = makeUnsafeShare
	"sharedStore" storeId
	(get (loadValue NS_APPLICATION_SHARES) defaultV)
	write
	(get (getStoreVersion NS_APPLICATION_SHARES) 0)
where	
	get f defaultV iworld
		# (mbV,iworld) = f storeId iworld
		# res = case mbV of
			Nothing	= Ok defaultV
			Just v	= Ok v
		= (res,iworld)
		
	write v iworld = (Ok Void,storeValue NS_APPLICATION_SHARES storeId v iworld)
	
currentDateTime :: ReadOnlyShared DateTime
currentDateTime = makeReadOnlyShared "SystemData" "currentDateTime" 'Util'.currentDateTime timeVersion
		
currentTime :: ReadOnlyShared Time
currentTime = makeReadOnlyShared "SystemData" "currentTime" 'Util'.currentTime timeVersion 
		
currentDate :: ReadOnlyShared Date
currentDate = makeReadOnlyShared "SystemData" "currentDate" 'Util'.currentDate dateVersion

// Workflow processes
topLevelTasks :: SharedTaskList Void
topLevelTasks = makeReadOnlyShared "taskList" "tasklist-top" read getVersion
where
	read iworld
		# (list, iworld) = loadValue NS_PERSISTENT_INSTANCES "index" iworld
		= ({TaskList|listId = TopLevelTaskList, state = [], items = fromMaybe [] list}, iworld)
	getVersion  iworld
		# (version, iworld) = getStoreVersion NS_PERSISTENT_INSTANCES "index" iworld
		= (fromMaybe 0 version, iworld)
		
currentProcesses ::ReadOnlyShared [TaskListItem]
currentProcesses = makeReadOnlyShared "SystemData" "processes" read getVersion
where
	read iworld
		# (list, iworld) = loadValue NS_PERSISTENT_INSTANCES "index" iworld
		= (fromMaybe [] list, iworld) 
	getVersion  iworld
		# (version, iworld) = getStoreVersion NS_PERSISTENT_INSTANCES "index" iworld
		= (fromMaybe 0 version, iworld)

processesForCurrentUser	:: ReadOnlyShared [TaskListItem]
processesForCurrentUser = makeReadOnlyShared "SystemData" "processesForCurrentUser" read getVersion
where
	read iworld=:{currentUser}
		# (list, iworld) = loadValue NS_PERSISTENT_INSTANCES "index" iworld
		= (maybe [] (\l -> find currentUser l) list, iworld)
	getVersion iworld
		# (version, iworld) = getStoreVersion NS_PERSISTENT_INSTANCES "index" iworld
		= (fromMaybe 0 version, iworld)

	find user procs
		= flatten [if (forWorker user p) [{p & subItems = find user p.subItems}] (find user p.subItems) \\ p <- procs]

	forWorker user {managementMeta=Just {worker=Nothing}}		= True
	forWorker user {managementMeta=Just {worker=Just worker}}	= worker == user
	forWorker _ _												= False

currentUser :: ReadOnlyShared User
currentUser = makeReadOnlyShared "SystemData" "currentUser" (\iworld=:{currentUser} -> (currentUser,iworld)) (\iworld -> (0,iworld))

currentTopTask :: ReadOnlyShared TaskId
currentTopTask = makeReadOnlyShared "SystemData" "currentTopTask" (\iworld=:{evalStack=[taskId:_]} -> (taskId,iworld)) (\iworld -> (0,iworld))
		
applicationName :: ReadOnlyShared String
applicationName = makeReadOnlyShared "SystemData" "applicationName" appName (\iworld -> (0,iworld))
where
	appName iworld=:{IWorld|application} = (application,iworld)

applicationBuild:: ReadOnlyShared String
applicationBuild  = makeReadOnlyShared "SystemData" "applicationBuild" appBuild (\iworld -> (0,iworld))
where
	appBuild iworld=:{IWorld|build} = (build,iworld)

applicationDirectory :: ReadOnlyShared FilePath
applicationDirectory = makeReadOnlyShared "SystemData" "applicationDirectory" appDir (\iworld -> (0,iworld))
where
	appDir iworld=:{IWorld|appDirectory} = (appDirectory,iworld)

applicationConfig :: ReadOnlyShared Config
applicationConfig = makeReadOnlyShared "SystemData" "config" config (\iworld -> (0,iworld))
where
	config iworld=:{IWorld|config} = (config,iworld)

// Random source
randomInt	:: ReadOnlyShared Int
randomInt = makeReadOnlyShared "SystemData" "randomInt" randomInt timeVersion
where
	randomInt iworld=:{IWorld|world}
		# (Clock seed, world)	= clock world
		= (hd (genRandInt seed), {IWorld|iworld & world = world})

dateVersion iworld
	# (date,iworld) 	= 'Util'.currentDate iworld 
	# (Timestamp ts)	= 'Util'.dateToTimestamp date
	= (ts,iworld)

timeVersion iworld
	# (Timestamp ts,iworld)	= 'Util'.currentTimestamp iworld
	= (ts,iworld)
