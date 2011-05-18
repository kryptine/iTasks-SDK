implementation module SystemData

import Types, Time, Shared, Util, UserDB, SessionDB
import Random
import StdList
from Util import qualified currentDate, currentTime, currentDateTime

currentDateTime :: ReadOnlyShared DateTime
currentDateTime = makeReadOnlyShared 'Util'.currentDateTime
		
currentTime :: ReadOnlyShared Time
currentTime = makeReadOnlyShared 'Util'.currentTime
		
currentDate :: ReadOnlyShared Date
currentDate = makeReadOnlyShared 'Util'.currentDate

// Users
users :: ReadOnlyShared [User]
users = makeReadOnlyShared getUsers

usersWithRole	:: !Role -> ReadOnlyShared [User]
usersWithRole role = makeReadOnlyShared (getUsersWithRole role)

userDetails :: !User -> SymmetricShared UserDetails
userDetails user = Shared read write getTimestamp
where
	read iworld	= appFst (mb2error "user not in database") (getUserDetails user iworld)
	write details iworld
		# (_,iworld) = updateUser user details iworld
		= (Ok Void,iworld)
	getTimestamp iworld=:{IWorld|timestamp} = (Ok timestamp,iworld)
	
currentUser :: ReadOnlyShared User
currentUser = makeReadOnlyShared (\iworld=:{currentUser} -> (currentUser,iworld))

// Sessions
sessions :: ReadOnlyShared [Session]
sessions = makeReadOnlyShared getSessions

// Available workflows

// Workflow processes

// Random source
randomInt	:: ReadOnlyShared Int
randomInt = makeReadOnlyShared randomInt
where
	randomInt iworld=:{IWorld|world}
		# (Clock seed, world)	= clock world
		= (hd (genRandInt seed), {IWorld|iworld & world = world})