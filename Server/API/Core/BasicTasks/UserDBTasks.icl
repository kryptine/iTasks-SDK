implementation module UserDBTasks

from TSt import :: Task, :: TSt, :: TaskResult(..)
from TSt import mkInstantTask, mkTaskFunction
import StdList, StdMaybe

from Types import :: User, :: UserId, :: UserDetails, :: Role
from UserDB import qualified class UserDB(..)
from UserDB import qualified instance UserDB TSt

getUser :: !UserId -> Task (Maybe User)
getUser username = mkInstantTask "Get user" "Read a user from the database." (mkTaskFunction ('UserDB'.getUser username))

getUsers :: Task [User]
getUsers = mkInstantTask "Get users" "Read all users from the database." (mkTaskFunction 'UserDB'.getUsers)

getUserDetails :: !User -> Task (Maybe UserDetails)
getUserDetails user = mkInstantTask "Get user details" "Obtain the details of a user from the database" (mkTaskFunction ('UserDB'.getUserDetails user))

getUsersWithRole :: !Role	-> Task [User]
getUsersWithRole role = mkInstantTask "Get users with role" "Get all users with a specific role." (mkTaskFunction ('UserDB'.getUsersWithRole role))

authenticateUser :: !String !String	-> Task (Maybe User)
authenticateUser username password = mkInstantTask "Authenticate user" "Verify if there is a user with the supplied credentials." (mkTaskFunction ('UserDB'.authenticateUser username password))

createUser :: !UserDetails -> Task User
createUser user = mkInstantTask "Create user" "Create a new user in the database." (mkTaskFunction ('UserDB'.createUser user))

updateUser :: !User !UserDetails -> Task User
updateUser user details = mkInstantTask "Update user" "Update a user's details in the database." (mkTaskFunction ('UserDB'.updateUser user details))

deleteUser :: !User -> Task User
deleteUser user = mkInstantTask "Delete user" "Delete a user from the database." (mkTaskFunction ('UserDB'.deleteUser user))
