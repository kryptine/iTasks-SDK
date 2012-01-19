implementation module SystemData

import SystemTypes, Time, Shared, SharedCombinators, Util, Text, Task, Tuple
import Random
import StdList, StdBool
from StdFunc	import o, seq
from IWorld		import :: IWorld(..), :: Control
from Util		import qualified currentDate, currentTime, currentDateTime, currentTimestamp, dateToTimestamp

sharedStore :: !String !a -> Shared a | JSONEncode{|*|}, JSONDecode{|*|}, TC a
sharedStore storeId defaultV = ReadWriteShared
	["sharedStore_" +++ storeId]
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
currentDateTime = makeReadOnlyShared "SystemData_currentDateTime" 'Util'.currentDateTime timeVersion
		
currentTime :: ReadOnlyShared Time
currentTime = makeReadOnlyShared "SystemData_currentTime" 'Util'.currentTime timeVersion 
		
currentDate :: ReadOnlyShared Date
currentDate = makeReadOnlyShared "SystemData_currentDate" 'Util'.currentDate dateVersion

// Workflow processes
topLevelTasks :: (TaskList Void)
topLevelTasks = TopLevelTaskList

currentProcesses ::ReadOnlyShared [TaskInstanceMeta]
currentProcesses = makeReadOnlyShared "SystemData_processes" read getVersion
where
	read iworld
		# (list, iworld) = loadValue NS_PERSISTENT_INSTANCES "index" iworld
		= (fromMaybe [] list, iworld) 
	getVersion  iworld
		# (version, iworld) = getStoreVersion NS_PERSISTENT_INSTANCES "index" iworld
		= (fromMaybe 0 version, iworld)

processesForCurrentUser	:: ReadOnlyShared [TaskInstanceMeta]
processesForCurrentUser = makeReadOnlyShared "SystemData_processesForCurrentUser" read getVersion
where
	read iworld=:{currentUser}
		# (list, iworld) = loadValue NS_PERSISTENT_INSTANCES "index" iworld
		= (maybe [] (\l -> find currentUser l) list, iworld)
	getVersion iworld
		# (version, iworld) = getStoreVersion NS_PERSISTENT_INSTANCES "index" iworld
		= (fromMaybe 0 version, iworld)

	find user procs
		= flatten [if (p.TaskInstanceMeta.managementMeta.worker === Just user || p.TaskInstanceMeta.managementMeta.worker === Nothing) [{p & subInstances = find user p.subInstances}] (find user p.subInstances) \\ p <- procs]

currentUser :: ReadOnlyShared User
currentUser = makeReadOnlyShared "SystemData_currentUser" (\iworld=:{currentUser} -> (currentUser,iworld)) (\iworld -> (0,iworld))

currentTopTask :: ReadOnlyShared TaskId
currentTopTask = makeReadOnlyShared "SystemData_currentTopTask" (\iworld=:{evalStack=[taskId:_]} -> (taskId,iworld)) (\iworld -> (0,iworld))
		
applicationName :: ReadOnlyShared String
applicationName = makeReadOnlyShared "SystemData_applicationName" appName (\iworld -> (0,iworld))
where
	appName iworld=:{IWorld|application} = (application,iworld)

applicationBuild:: ReadOnlyShared String
applicationBuild  = makeReadOnlyShared "SystemData_applicationBuild" appBuild (\iworld -> (0,iworld))
where
	appBuild iworld=:{IWorld|build} = (build,iworld)

applicationDirectory :: ReadOnlyShared FilePath
applicationDirectory = makeReadOnlyShared "SystemData_applicationDirectory" appDir (\iworld -> (0,iworld))
where
	appDir iworld=:{IWorld|appDirectory} = (appDirectory,iworld)

applicationConfig :: ReadOnlyShared Config
applicationConfig = makeReadOnlyShared "SystemData_config" config (\iworld -> (0,iworld))
where
	config iworld=:{IWorld|config} = (config,iworld)

// Random source
randomInt	:: ReadOnlyShared Int
randomInt = makeReadOnlyShared "SystemData_randomInt" randomInt timeVersion
where
	randomInt iworld=:{IWorld|world}
		# (Clock seed, world)	= clock world
		= (hd (genRandInt seed), {IWorld|iworld & world = world})
		
null :: ReadWriteShared Void a
null = ReadWriteShared ["SystemData_null"] read write (\iworld -> (Ok 0,iworld))
where
	read iworld			= (Ok Void,iworld)
	write _ iworld		= (Ok Void,iworld)

dateVersion iworld
	# (date,iworld) 	= 'Util'.currentDate iworld 
	# (Timestamp ts)	= 'Util'.dateToTimestamp date
	= (ts,iworld)

timeVersion iworld
	# (Timestamp ts,iworld)	= 'Util'.currentTimestamp iworld
	= (ts,iworld)
