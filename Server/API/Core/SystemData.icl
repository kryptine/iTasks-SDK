implementation module SystemData

import SystemTypes, Time, Shared, SharedCombinators, Util, Text, Task, Tuple
import Random
import StdList
from StdFunc	import o, seq
from IWorld		import :: IWorld(..), :: Control
from Util		import qualified currentDate, currentTime, currentDateTime, currentTimestamp
from UserDB		import qualified class UserDB(..), instance UserDB IWorld
from WorkflowDB	import qualified class WorkflowDB(..), instance WorkflowDB IWorld
from WorkflowDB	import :: WorkflowDescription

currentDateTime :: ReadOnlyShared DateTime
currentDateTime = makeReadOnlyShared "SystemData_currentDateTime" 'Util'.currentDateTime 'Util'.currentTimestamp
		
currentTime :: ReadOnlyShared Time
currentTime = makeReadOnlyShared "SystemData_currentTime" 'Util'.currentTime 'Util'.currentTimestamp
		
currentDate :: ReadOnlyShared Date
currentDate = makeReadOnlyShared "SystemData_currentDate" 'Util'.currentDate 'Util'.currentTimestamp

// Users
users :: ReadOnlyShared [User]
users = makeReadOnlyShared "SystemData_users" 'UserDB'.getUsers 'UserDB'.lastChange

usersWithRole	:: !Role -> ReadOnlyShared [User]
usersWithRole role = makeReadOnlyShared ("SystemData_usersWithRole-" +++ toString role) ('UserDB'.getUsersWithRole role) 'UserDB'.lastChange

userDetails :: !User -> Shared UserDetails
userDetails user = ReadWriteShared ["userDetails-" +++ toString user] read write (appFst Ok o 'UserDB'.lastChange)
where
	read iworld	= appFst (mb2error "user not in database") ('UserDB'.getUserDetails user iworld)
	write details iworld
		# (_,iworld) = 'UserDB'.updateUser user details iworld
		= (Ok Void,iworld)

currentUser :: ReadOnlyShared User
currentUser = makeReadOnlyShared "SystemData_currentUser" (\iworld=:{currentUser} -> (currentUser,iworld)) (\iworld -> (Timestamp 0, iworld))
	
currentUserDetails :: ReadOnlyShared (Maybe UserDetails)
currentUserDetails = makeReadOnlyShared "SystemData_currentUserDetails" (\iworld=:{currentUser} -> 'UserDB'.getUserDetails currentUser iworld) (\iworld -> (Timestamp 0, iworld))
	
// Workflow processes
topLevelTasks :: (TaskList Void)
topLevelTasks = GlobalTaskList

//TODO: Figure out pattern match bug
currentProcessId :: ReadOnlyShared ProcessId
//currentProcessId = makeReadOnlyShared "SystemData_currentProcess" (\iworld=:{evalStack=[currentProcess:_]} -> (currentProcess, iworld)) ('ProcessDB'.lastChange)
currentProcessId = makeReadOnlyShared "SystemData_currentProcess" (\iworld=:{evalStack} -> (hd evalStack, iworld)) 'Util'.currentTimestamp

currentProcesses ::ReadOnlyShared [TaskInstanceMeta]
currentProcesses = makeReadOnlyShared "SystemData_processes" read timestamp
where
	read iworld
		# (list, iworld) = loadValue NS_WORKFLOW_INSTANCES "ProcessDB" iworld
		= (fromMaybe [] list, iworld) 
	timestamp iworld
		# (ts, iworld) = getStoreTimestamp NS_WORKFLOW_INSTANCES "ProcessDB" iworld
		= (fromMaybe (Timestamp 0) ts, iworld)
		
processesForCurrentUser	:: ReadOnlyShared [TaskInstanceMeta]
processesForCurrentUser = makeReadOnlyShared "SystemData_processesForCurrentUser" read timestamp
where
	read iworld=:{currentUser}
		# (list, iworld) = loadValue NS_WORKFLOW_INSTANCES "ProcessDB" iworld
		= (maybe [] (\l -> [p \\ p <- l | p.managementMeta.worker === Just currentUser ]) list, iworld)
	timestamp iworld
		# (ts, iworld) = getStoreTimestamp NS_WORKFLOW_INSTANCES "ProcessDB" iworld
		= (fromMaybe (Timestamp 0) ts, iworld)
		
applicationName :: ReadOnlyShared String
applicationName = makeReadOnlyShared "SystemData_applicationName" appName (\iworld -> (Timestamp 0, iworld))
where
	appName iworld=:{IWorld|application} = (application,iworld)

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