definition module UserDBTasks
/**
* This modules provides tasks to retrieve information from the user database
* of the iTask system. These tasks are useful when work is to be delegated to
* other users.
*/
import StdMaybe, Void

from TSt	import :: Task
from Types	import :: UserId, :: User, :: UserDetails, :: Role

from InteractionTasks 	import class html

/**
* Finds a specific user
* 
* @param The username
*
* @return All information about the requested user
*/
getUser 			:: !UserId	-> Task (Maybe User)
/**
* Cross-checks a user with the database and retrieves all user
* details, if available
*
* @param The user description which needs to be checked
*
* @return Details of the user, if they exist
*/
getUserDetails 		:: !User -> Task (Maybe UserDetails)
/**
* Finds all registered users
*
* @return All registered users in the database
*/
getUsers			:: Task [User]
/**
* Finds all registered users ho have the given role
* 
* @param The required role
*
* @return All registered users from the database fulfilling the requirement
*/
getUsersWithRole	:: !Role	-> Task [User]
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
* Update an existing user
*
* @param The user who needs to be updated
* @param The new information
*
* @return The updated user
*/
updateUser			:: !User !UserDetails -> Task User
/**
* Delete an existing user
*
* @param The user who needs to be deleted
*
* @return The deleted user
*/
deleteUser			:: !User -> Task User
