definition module UserDBTasks
/**
* This modules provides tasks to retrieve information from the user database
* of the iTask system. These tasks are useful when work is to be delegated to
* other users.
*/
import Maybe, Void

from Task			import :: Task
from SystemTypes	import :: User, :: UserDetails, :: Role

/**
* Authenticates a user by username and password
*
* @param Username: The username
* @param Password: The password
*
* @return A single user who matches the given credentials, or nothing of none or more than one exists.

* @gin-icon key
*/
authenticateUser	:: !String !String	-> Task (Maybe User)
/**
* Add a new user
*
* @param User details: The user-information which needs to be stored
*
* @return The stored user
* 
* @gin-icon user_add
*/
createUser			:: !UserDetails -> Task User
/**
* Delete an existing user
*
* @param User: The user who needs to be deleted
*
* @return The deleted user
* 
* @gin-icon user_delete
*/
deleteUser			:: !User -> Task User
