implementation module UserDBTasks

import StdList, Maybe, HTML, Map, Either, JSON
import Task
from TaskContext import :: TaskContext(..), :: TopTaskContext, :: SubTaskContext, :: ParallelMeta
from StdFunc import id
from SystemTypes import :: User, :: UserId, :: UserDetails, :: Role, :: ProcessProperties
from UserDB import qualified class UserDB(..)
from UserDB import qualified instance UserDB IWorld

authenticateUser :: !String !String	-> Task (Maybe User)
authenticateUser username password = mkInstantTask ("Authenticate user", "Verify if there is a user with the supplied credentials.") eval
where
	eval taskNr iworld
		# (mbUser,iworld) = 'UserDB'.authenticateUser username password iworld
		= (TaskFinished mbUser,iworld)

createUser :: !UserDetails -> Task User
createUser user = mkInstantTask ("Create user", "Create a new user in the database.") eval
where
	eval taskNr iworld
		# (user,iworld) = 'UserDB'.createUser user iworld
		= (TaskFinished user,iworld)

deleteUser :: !User -> Task User
deleteUser user = mkInstantTask ("Delete user", "Delete a user from the database.") eval
where
	eval taskNr iworld
		# (user,iworld) = 'UserDB'.deleteUser user iworld
		= (TaskFinished user,iworld)