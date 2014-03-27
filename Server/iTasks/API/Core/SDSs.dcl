definition module iTasks.API.Core.SDSs
/**
* This module provides access to the iTask framework data by means of
* a set of shared data structures.
*/
from Text.JSON import generic JSONEncode, generic JSONDecode
import iTasks.Framework.SDS
from iTasks.API.Core.Types	    import :: DateTime, :: Date, :: Time, :: User, :: Role, :: TaskList, :: TaskAttributes
from iTasks.API.Core.Types	    import :: TaskListItem, :: Config, :: TaskId, :: TaskNo, :: InstanceNo, :: SharedTaskList
from Data.Void					import :: Void
from Data.Map                   import :: Map
from System.FilePath			import :: FilePath

//USER-DEFINED SHARES

/*
* Creates a reference to a store identified by a string identifier.
* If no data is store the default value given as second argument is given as result.
*/
sharedStore 			:: !String !a -> Shared a | JSONEncode{|*|}, JSONDecode{|*|}, TC a

// constant share, value does never change
constShare	            :: !a -> ROShared p a

//PREDEFINED SHARES

// null share
null		            :: WriteOnlyShared a

// Date & time (in task server's local timezone)
currentDateTime			:: ReadOnlyShared DateTime
currentTime				:: ReadOnlyShared Time
currentDate				:: ReadOnlyShared Date

// Date & time (in UTC)
currentUTCDateTime      :: ReadOnlyShared DateTime
currentUTCTime          :: ReadOnlyShared Time
currentUTCDate          :: ReadOnlyShared Date

// Processes
topLevelTasks 			:: SharedTaskList Void

currentSessions 		:: ReadOnlyShared [TaskListItem Void]
currentProcesses		:: ReadOnlyShared [TaskListItem Void]
processesForCurrentUser	:: ReadOnlyShared [TaskListItem Void]



// Session
currentUser				:: ReadOnlyShared User
currentTopTask			:: ReadOnlyShared TaskId

//Task instances
allTaskInstances        :: ReadOnlyShared [TaskListItem Void]
taskInstanceByNo        :: RWShared InstanceNo (TaskListItem Void) TaskAttributes

// Application
applicationName			:: ReadOnlyShared String	// Application name
applicationBuild		:: ReadOnlyShared String	// Application build identifier
applicationDirectory	:: ReadOnlyShared FilePath	// Directory in which the applicaton resides
applicationConfig		:: ReadOnlyShared Config	// Server config

// Random source
randomInt				:: ReadOnlyShared Int

// External file
externalFile :: !FilePath -> Shared String
