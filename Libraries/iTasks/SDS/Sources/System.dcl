definition module iTasks.SDS.Sources.System
/**
* This module exposes system information from an itask application
*/

import iTasks.SDS.Definition
from iTasks.WF.Definition import :: TaskId, :: TaskNo, :: InstanceNo, :: InstanceKey, :: TaskAttributes, :: Cookies
from iTasks.WF.Combinators.Core import :: TaskList, :: SharedTaskList, :: TaskListFilter, :: TaskListItem 
from iTasks.Extensions.DateTime import :: DateTime, :: Date, :: Time 
from iTasks.Engine import :: EngineOptions

from System.Time import :: Timespec, :: Timestamp
from System.FilePath import :: FilePath
from Data.Map import :: Map
from Data.Maybe import :: Maybe

//* Types to view the server's internal table of running task instances
:: TaskInstance =
	{ instanceNo	       :: !InstanceNo        //* Unique global identification
    , instanceKey          :: !Maybe InstanceKey //* Random string that a client needs to provide to access the task instance
	, value                :: !ValueStatus       //* Status of the task value
    , session              :: !Bool              //* Is this a session
	, listId               :: !TaskId            //* Reference to parent tasklist
    , build                :: !String            //* Application build version when the instance was created
    , issuedAt			   :: !Timespec          //* When was the task created
	, taskAttributes       :: !TaskAttributes    //* Computed task meta-data
	, managementAttributes :: !TaskAttributes    //* Arbitrary meta-data
	, firstEvent		   :: !Maybe Timespec    //* When was the first work done on this task
	, lastEvent		       :: !Maybe Timespec    //* When was the last event on this task	
	}

:: ValueStatus = Stable | Unstable | Exception !String

// Date & time (in task server's local timezone)
currentDateTime 		:: SDSParallel () DateTime ()
currentTime				:: SDSLens () Time ()
currentDate				:: SDSLens () Date ()

// Date & time (in UTC)
currentUTCDateTime      :: SDSLens () DateTime ()
currentUTCTime          :: SDSLens () Time ()
currentUTCDate          :: SDSLens () Date ()

//Unix timestamp
currentTimestamp 		:: SDSLens () Timestamp ()
currentTimespec 		:: SDSLens () Timespec ()

// Processes
topLevelTasks 			:: SharedTaskList ()

currentSessions 		:: SDSLens () [TaskListItem ()] ()
currentProcesses		:: SDSLens () [TaskListItem ()] ()

// Session
currentTopTask :: SDSLens () TaskId ()

//Task instances
currentTaskInstanceNo           :: SDSSource () InstanceNo ()
currentTaskInstanceAttributes   :: SDSSequence () TaskAttributes TaskAttributes
currentTaskInstanceCookies      :: SDSSequence () Cookies (String,String,Maybe Int)
allTaskInstances                :: SDSSequence () [TaskInstance] ()
detachedTaskInstances           :: SDSSequence () [TaskInstance] () //Exclude sessions
taskInstanceByNo                :: SDSSequence InstanceNo TaskInstance TaskAttributes
taskInstanceAttributesByNo      :: SDSSequence InstanceNo TaskAttributes TaskAttributes
taskInstancesByAttribute		:: SDSLens (!String,!JSONNode) [TaskInstance] () //Parameter is (key,value)

// Application
applicationName			:: SDSSource () String ()         // Application name
applicationVersion      :: SDSSource () String ()          // Application build identifier
applicationDirectory	:: SDSSource () FilePath ()       // Directory in which the applicaton resides
applicationOptions      :: SDSSource () EngineOptions ()   //Full engine options
