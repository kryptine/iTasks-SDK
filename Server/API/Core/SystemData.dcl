definition module SystemData
/**
* This module provides access to the iTask framework data by means of
* a set of shared data structures.
*/
import Maybe
from Shared		import :: ReadOnlyShared, :: Shared, :: SymmetricShared
from Types		import :: DateTime, :: Date, :: Time, :: User, :: Role, :: Session, :: UserDetails, :: Workflow
from Void		import :: Void
from ProcessDB	import :: Process
from WorkflowDB	import :: WorkflowDescription

// Date & time
currentDateTime			:: ReadOnlyShared DateTime
currentTime				:: ReadOnlyShared Time
currentDate				:: ReadOnlyShared Date

// Users
users					:: 			ReadOnlyShared [User]
usersWithRole			:: !Role ->	ReadOnlyShared [User]
userDetails				:: !User ->	SymmetricShared UserDetails
currentUser				::			ReadOnlyShared User
currentUserDetails		::			ReadOnlyShared (Maybe UserDetails)
				
// Sessions
sessions				:: ReadOnlyShared [Session]

// Available workflows
workflows				:: ReadOnlyShared [WorkflowDescription]
allowedWorkflows		:: ReadOnlyShared [WorkflowDescription]

// Workflow processes
currentProcesses		::			ReadOnlyShared [Process]
currentProcessesForUser	:: !User ->	ReadOnlyShared [Process]

// Random source
randomInt				:: ReadOnlyShared Int

// Null data source (writing has no effect)
null					:: Shared Void a