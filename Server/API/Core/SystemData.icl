implementation module SystemData

import SystemTypes, Time, Shared, SharedCombinators, Util, Text, Task, Tuple
import Random
import StdList, StdBool
from StdFunc	import o, seq
from IWorld		import :: IWorld(..), :: Control
from Util		import qualified currentDate, currentTime, currentDateTime, currentTimestamp

currentDateTime :: ReadOnlyShared DateTime
currentDateTime = makeReadOnlyShared "SystemData_currentDateTime" 'Util'.currentDateTime 'Util'.currentTimestamp
		
currentTime :: ReadOnlyShared Time
currentTime = makeReadOnlyShared "SystemData_currentTime" 'Util'.currentTime 'Util'.currentTimestamp
		
currentDate :: ReadOnlyShared Date
currentDate = makeReadOnlyShared "SystemData_currentDate" 'Util'.currentDate 'Util'.currentTimestamp

// Workflow processes
topLevelTasks :: (TaskList Void)
topLevelTasks = GlobalTaskList

currentProcesses ::ReadOnlyShared [TaskInstanceMeta]
currentProcesses = makeReadOnlyShared "SystemData_processes" read timestamp
where
	read iworld
		# (list, iworld) = loadValue NS_WORKFLOW_INSTANCES "index" iworld
		= (fromMaybe [] list, iworld) 
	timestamp iworld
		# (ts, iworld) = getStoreTimestamp NS_WORKFLOW_INSTANCES "index" iworld
		= (fromMaybe (Timestamp 0) ts, iworld)
		
processesForCurrentUser	:: ReadOnlyShared [TaskInstanceMeta]
processesForCurrentUser = makeReadOnlyShared "SystemData_processesForCurrentUser" read timestamp
where
	read iworld=:{currentUser}
		# (list, iworld) = loadValue NS_WORKFLOW_INSTANCES "index" iworld
		= (maybe [] (\l -> find currentUser l) list, iworld)
	timestamp iworld
		# (ts, iworld) = getStoreTimestamp NS_WORKFLOW_INSTANCES "index" iworld
		= (fromMaybe (Timestamp 0) ts, iworld)

	find user procs
		= flatten [if (p.managementMeta.worker === Just user || p.managementMeta.worker === Nothing) [p] (find user p.subInstances) \\ p <- procs]

//TODO: Figure out pattern match bug
currentProcessId :: ReadOnlyShared ProcessId
//currentProcessId = makeReadOnlyShared "SystemData_currentProcess" (\iworld=:{evalStack=[currentProcess:_]} -> (currentProcess, iworld)) ('ProcessDB'.lastChange)
currentProcessId = makeReadOnlyShared "SystemData_currentProcess" (\iworld=:{evalStack} -> (hd evalStack, iworld)) 'Util'.currentTimestamp

currentUser :: ReadOnlyShared User
currentUser = makeReadOnlyShared "SystemData_currentUser" (\iworld=:{currentUser} -> (currentUser,iworld)) (\iworld -> (Timestamp 0, iworld))
		
applicationName :: ReadOnlyShared String
applicationName = makeReadOnlyShared "SystemData_applicationName" appName (\iworld -> (Timestamp 0, iworld))
where
	appName iworld=:{IWorld|application} = (application,iworld)

applicationBuild:: ReadOnlyShared String
applicationBuild  = makeReadOnlyShared "SystemData_applicationBuild" appBuild (\iworld -> (Timestamp 0, iworld))
where
	appBuild iworld=:{IWorld|build} = (build,iworld)

applicationDirectory :: ReadOnlyShared FilePath
applicationDirectory = makeReadOnlyShared "SystemData_applicationDirectory" appDir (\iworld -> (Timestamp 0, iworld))
where
	appDir iworld=:{IWorld|appDirectory} = (appDirectory,iworld)

applicationConfig :: ReadOnlyShared Config
applicationConfig = makeReadOnlyShared "SystemData_config" config (\iworld -> (Timestamp 0, iworld))
where
	config iworld=:{IWorld|config} = (config,iworld)

// Random source
randomInt	:: ReadOnlyShared Int
randomInt = makeReadOnlyShared "SystemData_randomInt" randomInt 'Util'.currentTimestamp
where
	randomInt iworld=:{IWorld|world}
		# (Clock seed, world)	= clock world
		= (hd (genRandInt seed), {IWorld|iworld & world = world})
		
null :: ReadWriteShared Void a
null = ReadWriteShared ["SystemData_null"] read write getTimestamp
where
	read iworld			= (Ok Void,iworld)
	write _ iworld		= (Ok Void,iworld)
	getTimestamp iworld	= (Ok (Timestamp 0),iworld)
