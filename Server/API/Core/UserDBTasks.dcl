definition module UserDBTasks
/**
* This modules provides tasks to retrieve information from the user database
* of the iTask system. These tasks are useful when work is to be delegated to
* other users.
*/
import Maybe, Void

from Task	import :: Task
from Types	import :: UserId, :: User, :: UserDetails, :: Role

/**
* Authenticates a user by username and password
*
* @param The username
* @param The password
*
* @return A single user who matches the given credentials, or nothing of none or more than one exists.
*/
authenticateUser	:: !String !String	-> Task (Maybe User)
/**
* Add a new user
*
* @param The user-infromation which needs to be stored
*
* @return The stored user
*/
createUser			:: !UserDetails -> Task User
/**
* Delete an existing user
*
* @param The user who needs to be deleted
*
* @return The deleted user
*/
deleteUser			:: !User -> Task User
