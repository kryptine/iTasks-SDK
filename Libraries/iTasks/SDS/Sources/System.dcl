definition module iTasks.SDS.Sources.System
/**
* This module exposes system information from an itask application
*/

from iTasks.SDS.Definition import :: SDS
from iTasks.WF.Definition  import :: TaskId, :: TaskNo, :: InstanceNo
from iTasks.API.Core.Types import :: DateTime, :: Date, :: Time
from iTasks.API.Core.Types import :: TaskList, :: TaskAttributes
from iTasks.API.Core.Types import :: TaskListFilter, :: TaskListItem, :: TaskInstance, :: Config, :: SharedTaskList

from System.Time import :: Timestamp
from System.FilePath import :: FilePath
from Data.Map import :: Map

// Date & time (in task server's local timezone)
currentDateTime			:: SDS () DateTime ()
currentTime				:: SDS () Time ()
currentDate				:: SDS () Date ()

// Date & time (in UTC)
currentUTCDateTime      :: SDS () DateTime ()
currentUTCTime          :: SDS () Time ()
currentUTCDate          :: SDS () Date ()

//Unix timestamp
currentTimestamp 		:: SDS () Timestamp ()

// Processes
topLevelTasks 			:: SharedTaskList () 

currentSessions 		:: SDS () [TaskListItem ()] ()
currentProcesses		:: SDS () [TaskListItem ()] ()

// Session
currentTopTask			:: SDS () TaskId ()

//Task instances
currentTaskInstanceNo           :: SDS () InstanceNo ()
currentTaskInstanceAttributes   :: SDS () TaskAttributes TaskAttributes
allTaskInstances                :: SDS () [TaskInstance] ()
detachedTaskInstances	        :: SDS () [TaskInstance] () //Exclude sessions
taskInstanceByNo                :: SDS InstanceNo TaskInstance TaskAttributes
taskInstanceAttributesByNo      :: SDS InstanceNo TaskAttributes TaskAttributes
taskInstancesByAttribute		:: SDS (!String,!String) [TaskInstance] () //Parameter is (key,value)

// Application
applicationName			:: SDS () String ()   // Application name
applicationBuild		:: SDS () String ()   // Application build identifier
applicationDirectory	:: SDS () FilePath () // Directory in which the applicaton resides
applicationConfig		:: SDS () Config ()   // Server config

