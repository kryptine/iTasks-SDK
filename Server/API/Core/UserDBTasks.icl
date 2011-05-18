implementation module UserDBTasks

from Task import class descr, instance descr (String,String)
import TSt, StdList, Maybe, HTML
from Types import :: User, :: UserId, :: UserDetails, :: Role
from UserDB import qualified class UserDB(..)
from UserDB import qualified instance UserDB TSt

authenticateUser :: !String !String	-> Task (Maybe User)
authenticateUser username password = mkInstantTask ("Authenticate user", "Verify if there is a user with the supplied credentials.") (mkTaskFunction ('UserDB'.authenticateUser username password))

createUser :: !UserDetails -> Task User
createUser user = mkInstantTask ("Create user", "Create a new user in the database.") (mkTaskFunction ('UserDB'.createUser user))

deleteUser :: !User -> Task User
deleteUser user = mkInstantTask ("Delete user", "Delete a user from the database.") (mkTaskFunction ('UserDB'.deleteUser user))
