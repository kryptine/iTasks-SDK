definition module UserDB
/**
* This module provides the iTasks user database. It provides
* functions for accessing information about system users.
*/
import StdMaybe
import TSt

/**
* Fetches the id and display name from the user database for a given user id.
*
* @param A user id
* @param A unique user database handle
* @return The user
* @return The database handle
*/
getUser				::	!UserName			!*TSt -> (!User						, !*TSt)
/**
* Fetches the id and display name of all users from the from the user database.
*
* @param A unique user database handle
* @return The list of users
* @return The database handle
*/
getUsers 			:: 					!*TSt -> (![User]					, !*TSt)
/**
* Finds a list of users that have a certain role.
*
* @param The role to look for
* @param A unique user database handle
* @return A list of users
* @return The database handle
*/
getUsersWithRole	:: !String			!*TSt -> (![User]				, !*TSt)
/**
* Maps a list of usernames to a list of display names. Display names are strings which
* are suited for displaying directly in a user interface. For example "John Doe".
*
* @param A list of usernames
* @param A unique user database handle
* @return A list of display names
* @return The database handle
*/
getDisplayNames		:: ![UserName]			!*TSt -> (![DisplayName]			, !*TSt)
/**
* Maps a list of usernames to a list of a list of roles. Roles are strings that identify a certain
* role a user may play in a workflow scenario and can be used to determine what a user is allowed
* to do.
*
* @param A list of usernames
* @param A unique user database handle
* @return A list of a list of roles (a list of roles for each user)
* @return The database handle 
*/
getRoles			:: ![UserName]			!*TSt -> (![[Role]]					, !*TSt)
/**
* Authenticate a user based on a user name or password
*
* @param A user name
* @param A password
* @param A unique database handle
* @return When successful, A triple of user id/ display name/ list of roles.
* @return The database handle
*/
authenticateUser	:: !String !String	!*TSt -> (!Maybe User, !*TSt)
/**
* Create a new user
*
* @param A new user
* @param A unique database handle
*
* @return A user
* @return The database handle
*/
createUser :: !User !*TSt -> (!User,!*TSt)
/**
* Update an existing user
* 
* @param An existing user
* @param A unique database handle
*
* @return The existing user
* @retrun The database handle 
*/
updateUser :: !User !*TSt -> (!User,!*TSt)
/**
* Delete an existing user
*
* @param An existing user
* @param A unique database handle
*
* @return The existing user
* @retrun The database handle 
*/
deleteUser :: !User !*TSt -> (!User,!*TSt)