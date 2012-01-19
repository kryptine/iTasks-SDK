definition module SystemData
/**
* This module provides access to the iTask framework data by means of
* a set of shared data structures.
*/
import Maybe, JSON
from SharedCombinators	import :: ReadWriteShared, :: ReadOnlyShared, :: Shared
from SystemTypes		import :: DateTime, :: Date, :: Time, :: User, :: Role, :: UserDetails, :: TaskList, :: Tree
from SystemTypes		import :: TaskInstanceMeta, :: Config, :: TaskId, :: TaskNo, :: TopNo
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
topLevelTasks 			:: (TaskList Void)

currentProcesses		:: ReadOnlyShared [TaskInstanceMeta]
processesForCurrentUser	:: ReadOnlyShared [TaskInstanceMeta]

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

// Null data source (writing has no effect)
null					:: ReadWriteShared Void a
