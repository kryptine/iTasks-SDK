implementation module SystemData

import Types, Time, Shared, Util
import Random
import StdList
from StdFunc	import o
from Util		import qualified currentDate, currentTime, currentDateTime, currentTimestamp
from UserDB		import qualified class UserDB(..), instance UserDB IWorld
from ProcessDB	import qualified class ProcessDB(..), instance ProcessDB IWorld
from ProcessDB	import :: Process
from SessionDB	import qualified class SessionDB(..), instance SessionDB IWorld

currentDateTime :: ReadOnlyShared DateTime
currentDateTime = makeReadOnlyShared 'Util'.currentDateTime 'Util'.currentTimestamp
		
currentTime :: ReadOnlyShared Time
currentTime = makeReadOnlyShared 'Util'.currentTime 'Util'.currentTimestamp
		
currentDate :: ReadOnlyShared Date
currentDate = makeReadOnlyShared 'Util'.currentDate 'Util'.currentTimestamp

// Users
users :: ReadOnlyShared [User]
users = makeReadOnlyShared 'UserDB'.getUsers 'UserDB'.lastChange

usersWithRole	:: !Role -> ReadOnlyShared [User]
usersWithRole role = makeReadOnlyShared ('UserDB'.getUsersWithRole role) 'UserDB'.lastChange

userDetails :: !User -> SymmetricShared UserDetails
userDetails user = Shared read write (appFst Ok o 'UserDB'.lastChange)
where
	read iworld	= appFst (mb2error "user not in database") ('UserDB'.getUserDetails user iworld)
	write details iworld
		# (_,iworld) = 'UserDB'.updateUser user details iworld
		= (Ok Void,iworld)
	
currentUser :: ReadOnlyShared User
currentUser = makeReadOnlyShared (\iworld=:{currentUser} -> (currentUser,iworld)) 'Util'.currentTimestamp

// Sessions
sessions :: ReadOnlyShared [Session]
sessions = makeReadOnlyShared 'SessionDB'.getSessions 'SessionDB'.lastChange

// Available workflows
workflows :: ReadOnlyShared [Workflow]
workflows = makeReadOnlyShared (\iworld=:{staticWorkflows} -> (staticWorkflows,iworld)) 'Util'.currentTimestamp

// Workflow processes
currentProcesses ::ReadOnlyShared [Process]
currentProcesses = makeReadOnlyShared ('ProcessDB'.getProcesses [Running] [Active]) 'Util'.currentTimestamp

currentProcessesForUser :: !User -> ReadOnlyShared [Process]
currentProcessesForUser user = makeReadOnlyShared ('ProcessDB'.getProcessesForUser user [Running] [Active]) 'Util'.currentTimestamp

// Random source
randomInt	:: ReadOnlyShared Int
randomInt = makeReadOnlyShared randomInt 'Util'.currentTimestamp
where
	randomInt iworld=:{IWorld|world}
		# (Clock seed, world)	= clock world
		= (hd (genRandInt seed), {IWorld|iworld & world = world})
		
null :: Shared Void a
null = Shared read write getTimestamp
where
	read iworld			= (Ok Void,iworld)
	write _ iworld		= (Ok Void,iworld)
	getTimestamp iworld	= (Ok (Timestamp 0),iworld)