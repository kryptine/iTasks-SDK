implementation module SystemData

import Types, Time, Shared, Util, UserDB, SessionDB
import Random
import StdList

sharedCurrentDateTime :: ReadOnlyShared DateTime
sharedCurrentDateTime = makeReadOnlyShared currentDateTime
		
sharedCurrentTime :: ReadOnlyShared Time
sharedCurrentTime = makeReadOnlyShared currentTime
		
sharedCurrentDate :: ReadOnlyShared Date
sharedCurrentDate = makeReadOnlyShared currentDate

// Users
sharedUsers :: ReadOnlyShared [User]
sharedUsers = makeReadOnlyShared getUsers

sharedUsersWithRole	:: !Role -> ReadOnlyShared [User]
sharedUsersWithRole role = makeReadOnlyShared (getUsersWithRole role)

// Sessions
sharedSessions :: ReadOnlyShared [Session]
sharedSessions = makeReadOnlyShared getSessions

// Available workflows

// Workflow processes

// Random source
sharedRandomInt	:: ReadOnlyShared Int
sharedRandomInt = makeReadOnlyShared randomInt
where
	randomInt iworld=:{IWorld|world}
		# (Clock seed, world)	= clock world
		= (hd (genRandInt seed), {IWorld|iworld & world = world})