definition module UserAdmin
/**
* This extension provides workflows for managing the users of an iTask system.
*/
import iTasks

// Shares

//* All users
users					:: 			ReadOnlyShared [User]
//* Users with a specific role
usersWithRole			:: !Role ->	ReadOnlyShared [User]
//* User details (name,credentials etc)
userDetails				:: !User ->	Shared (Maybe UserDetails)
//* Details of the current user
currentUserDetails		::			ReadOnlyShared (Maybe UserDetails)

/**
* Authenticates a user by username and password
*
* @param Username: The username
* @param Password: The password
*
* @return A single user who matches the given credentials, or nothing of none or more than one exists.

* @gin-icon key
*/
authenticateUser	:: !Username !Password	-> Task (Maybe User)

/**
* Wraps a task with an authentication task
*
* @param	the task to wrap
*
* @gin-icon key
*/
doAuthenticated :: (Task a) -> Task a | iTask a

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
/**
* Browse and manage the existing users
*/
manageUsers			:: Task Void