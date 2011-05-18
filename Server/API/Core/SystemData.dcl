definition module SystemData
/**
* This module provides access to the iTask framework data by means of
* a set of shared data structures.
*/
from Shared import :: ReadOnlyShared, :: Shared, :: SymmetricShared
from Types import :: DateTime, :: Date, :: Time, :: User, :: Role, :: Session, :: UserDetails
from Void import :: Void

// Date & time
currentDateTime	:: ReadOnlyShared DateTime
currentTime		:: ReadOnlyShared Time
currentDate		:: ReadOnlyShared Date

// Users
users			:: 			ReadOnlyShared [User]
usersWithRole	:: !Role ->	ReadOnlyShared [User]
userDetails		:: !User ->	SymmetricShared UserDetails
currentUser		::			ReadOnlyShared User
				
// Sessions
sessions		:: ReadOnlyShared [Session]

// Available workflows

// Workflow processes

// Random source
randomInt		:: ReadOnlyShared Int