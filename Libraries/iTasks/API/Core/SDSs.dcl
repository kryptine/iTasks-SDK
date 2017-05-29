definition module iTasks.API.Core.SDSs
/**
* This module provides access to the iTask framework data by means of
* a set of shared data structures.
*/
from Text.JSON import generic JSONEncode, generic JSONDecode
import iTasks._Framework.SDS
from iTasks.API.Core.Types	    import :: DateTime, :: Date, :: Time, :: TaskList, :: TaskAttributes
from iTasks.API.Core.Types	    import :: TaskListFilter, :: TaskListItem, :: TaskInstance, :: Config, :: TaskId, :: TaskNo, :: InstanceNo, :: SharedTaskList
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

//Unix timestamp
currentTimestamp 		:: ReadOnlyShared Timestamp

// Processes
topLevelTasks 			:: SharedTaskList () 

currentSessions 		:: ReadOnlyShared [TaskListItem ()]
currentProcesses		:: ReadOnlyShared [TaskListItem ()]

// Session
currentTopTask			:: ReadOnlyShared TaskId

//Task instances
currentTaskInstanceNo           :: ROShared () InstanceNo
currentTaskInstanceAttributes   :: RWShared () TaskAttributes TaskAttributes
allTaskInstances                :: ROShared () [TaskInstance]
detachedTaskInstances	        :: ROShared () [TaskInstance] //Exclude sessions
taskInstanceByNo                :: RWShared InstanceNo TaskInstance TaskAttributes
taskInstanceAttributesByNo      :: RWShared InstanceNo TaskAttributes TaskAttributes
taskInstancesByAttribute		:: ROShared (!String,!String) [TaskInstance] //Parameter is (key,value)

// Application
applicationName			:: ReadOnlyShared String	// Application name
applicationBuild		:: ReadOnlyShared String	// Application build identifier
applicationDirectory	:: ReadOnlyShared FilePath	// Directory in which the applicaton resides
applicationConfig		:: ReadOnlyShared Config	// Server config

// Generic store
storeNamespaces         :: ROShared () [String]   // List the namespaces in the store
storeNames              :: ROShared String [String] // List the stores in a given namespace

// Random source
randomInt				:: ROShared () Int

// External file
externalFile            :: RWShared FilePath String String

// External directory
externalDirectory       :: ROShared FilePath [FilePath]
