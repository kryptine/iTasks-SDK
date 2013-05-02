definition module iTasks.API.Core.SystemData
/**
* This module provides access to the iTask framework data by means of
* a set of shared data structures.
*/
import Data.Maybe, Text.JSON
import iTasks.Framework.Shared
from iTasks.API.Core.SystemTypes	import :: DateTime, :: Date, :: Time, :: User, :: Role, :: TaskList, :: Tree
from iTasks.API.Core.SystemTypes	import :: TaskListItem, :: Config, :: TaskId, :: TaskNo, :: InstanceNo, :: SharedTaskList
from Data.Void							import :: Void
from System.FilePath						import :: FilePath

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
topLevelTasks 			:: SharedTaskList Void

currentSessions 		:: ReadOnlyShared [TaskListItem Void]
currentProcesses		:: ReadOnlyShared [TaskListItem Void]
processesForCurrentUser	:: ReadOnlyShared [TaskListItem Void]

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

// External file
externalFile :: !FilePath -> Shared String
