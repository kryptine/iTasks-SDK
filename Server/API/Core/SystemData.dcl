definition module SystemData
/**
* This module provides access to the iTask framework data by means of
* a set of shared data structures.
*/
import Maybe
from SharedCombinators	import :: ReadOnlyShared, :: Shared, :: ReadWriteShared
from SystemTypes		import :: DateTime, :: Date, :: Time, :: User, :: Role, :: Session, :: UserDetails, :: TaskList, :: Tree
from Void				import :: Void
from ProcessDB			import :: Process, :: ProcessId

// Date & time
currentDateTime			:: ReadOnlyShared DateTime
currentTime				:: ReadOnlyShared Time
currentDate				:: ReadOnlyShared Date

// Users
users					:: 			ReadOnlyShared [User]
usersWithRole			:: !Role ->	ReadOnlyShared [User]
userDetails				:: !User ->	Shared UserDetails
currentUser				::			ReadOnlyShared User
currentUserDetails		::			ReadOnlyShared (Maybe UserDetails)

// Processes
topLevelTasks 			:: (TaskList Void)

currentProcessId		:: ReadOnlyShared ProcessId
currentProcesses		:: ReadOnlyShared [Process]
processesForCurrentUser	:: ReadOnlyShared [Process]

// Application name
applicationName			:: ReadOnlyShared String

// Random source
randomInt				:: ReadOnlyShared Int

// Null data source (writing has no effect)
null					:: ReadWriteShared Void a