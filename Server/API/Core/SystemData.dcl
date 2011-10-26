definition module SystemData
/**
* This module provides access to the iTask framework data by means of
* a set of shared data structures.
*/
import Maybe
from SharedCombinators	import :: ReadWriteShared, :: ReadOnlyShared, :: Shared
from SystemTypes		import :: DateTime, :: Date, :: Time, :: User, :: Role, :: UserDetails, :: TaskList, :: Tree, :: ProcessId, :: TaskInstanceMeta
from Void				import :: Void
from Config				import :: Config

// Date & time
currentDateTime			:: ReadOnlyShared DateTime
currentTime				:: ReadOnlyShared Time
currentDate				:: ReadOnlyShared Date

// Processes
topLevelTasks 			:: (TaskList Void)

currentProcesses		:: ReadOnlyShared [TaskInstanceMeta]
processesForCurrentUser	:: ReadOnlyShared [TaskInstanceMeta]

// Session
currentProcessId		:: ReadOnlyShared ProcessId
currentUser				:: ReadOnlyShared User

// Application name
applicationName			:: ReadOnlyShared String

// Server config
applicationConfig		:: ReadOnlyShared Config

// Random source
randomInt				:: ReadOnlyShared Int

// Null data source (writing has no effect)
null					:: ReadWriteShared Void a