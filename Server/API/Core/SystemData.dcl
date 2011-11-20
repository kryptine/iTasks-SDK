definition module SystemData
/**
* This module provides access to the iTask framework data by means of
* a set of shared data structures.
*/
import Maybe, SharedCombinators
from SystemTypes		import :: DateTime, :: Date, :: Time, :: User, :: Role, :: UserDetails, :: TaskList, :: Tree
from SystemTypes		import :: ProcessId, :: TaskInstanceMeta, :: Config
from Void				import :: Void

// Date & time
/*currentDateTime			:: ReadOnlyShared DateTime
currentTime				:: ReadOnlyShared Time
currentDate				:: ReadOnlyShared Date*/

// Processes
topLevelTasks 			:: (TaskList Void)

/*currentProcesses		:: ReadOnlyShared [TaskInstanceMeta]
processesForCurrentUser	:: ReadOnlyShared [TaskInstanceMeta]

// Session
currentProcessId		:: ReadOnlyShared ProcessId
currentUser				:: ReadOnlyShared User

// Application name
applicationName			:: ReadOnlyShared String
// Application build identifier
applicationBuild		:: ReadOnlyShared String

// Server config
applicationConfig		:: ReadOnlyShared Config

// Random source
randomInt				:: ReadOnlyShared Int*/

// Null data source (writing has no effect)
from SharedDataSource	import null
from SharedDataSource	import qualified :: WOShared