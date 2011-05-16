definition module SystemData
/**
* This module provides access to the iTask framework data by means of
* a set of shared data structures.
*/
from Shared import :: ReadOnlyShared, :: Shared
from Types import :: DateTime, :: Date, :: Time, :: User, :: Role, :: Session
from Void import :: Void

// Date & time
sharedCurrentDateTime	:: ReadOnlyShared DateTime
sharedCurrentTime		:: ReadOnlyShared Time
sharedCurrentDate		:: ReadOnlyShared Date

// Users
sharedUsers				:: 			ReadOnlyShared [User]
sharedUsersWithRole		:: !Role -> ReadOnlyShared [User]
				
// Sessions
sharedSessions			:: ReadOnlyShared [Session]

// Available workflows

// Workflow processes

// Random source
sharedRandomInt			:: ReadOnlyShared Int