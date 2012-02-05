definition module SystemData
/**
* This module provides access to the iTask framework data by means of
* a set of shared data structures.
*/
import Maybe, JSON, Shared
from SystemTypes		import :: DateTime, :: Date, :: Time, :: User, :: Role, :: UserDetails, :: TaskList, :: Tree
from SystemTypes		import :: TaskListItem, :: Config, :: TaskId, :: TaskNo, :: TopNo
from Void				import :: Void
from FilePath			import :: FilePath

//USER-DEFINED SHARES

/*
* Creates a reference to a store identified by a string identifier.
* If no data is store the default value given as second argument is given as result.
*/
sharedStore 			:: !String !a -> Shared a | JSONEncode{|*|}, JSONDecode{|*|}, TC a

//PREDEFINED SHARES 

// Date & time
currentDateTime			:: ReadOnlyShared DateTime
currentTime				:: ReadOnlyShared Time
currentDate				:: ReadOnlyShared Date

// Processes
topLevelTasks 			:: ReadWriteShared (TaskList Void) Void

currentProcesses		:: ReadOnlyShared [TaskListItem]
processesForCurrentUser	:: ReadOnlyShared [TaskListItem]

// Session
currentUser				:: ReadOnlyShared User
currentTopTask			:: ReadOnlyShared TaskId

// Application
applicationName			:: ReadOnlyShared String	// Application name
applicationBuild		:: ReadOnlyShared String	// Application build identifier
applicationDirectory	:: ReadOnlyShared FilePath	// Directory in which the applicaton resides
applicationConfig		:: ReadOnlyShared Config	// Server config

// Random source
randomInt				:: ReadOnlyShared Int
