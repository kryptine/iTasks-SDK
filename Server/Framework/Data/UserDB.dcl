definition module UserDB
/**
* This module provides the iTasks user database. It provides
* functions for accessing information about system users.
*/
import Maybe
import TSt

class UserDB st
where
	/**
	* Fetches the id and display name from the user database for a given user id.
	*
	* @param A user id
	* @param A unique user database handle
	* @return The user if found
	* @return The database handle
	*/
	getUser				::	!UserId			!*st -> (!Maybe User				, !*st)
	/** 
	* Looks up the details of any user in the database.
	*
	* @param A user
	* @param A unique user database handle
	* @return The user-details, if found
	* @return The database handle
	*/	
	getUserDetails 		:: !User 			!*st -> (!Maybe UserDetails			,!*st)
	/**
	* Fetches the id and details of all users from the from the user database.
	*
	* @param A unique user database handle
	* @return The list of users
	* @return The database handle
	*/	
	getUsers 			:: 					!*st -> (![User]					, !*st)
	/**
	* Finds a list of users that have a certain role.
	*
	* @param The role to look for
	* @param A unique user database handle
	* @return A list of users
	* @return The database handle
	*/
	getUsersWithRole	:: !String			!*st -> (![User]				, !*st)
	/**
	* Authenticate a user based on a user name or password
	*
	* @param A user name
	* @param A password
	* @param A unique database handle
	* @return When successful, A triple of user id/ display name/ list of roles.
	* @return The database handle
	*/
	authenticateUser	:: !String !String	!*st -> (!Maybe User, !*st)
	/**
	* Create a new user
	*
	* @param A new user
	* @param A unique database handle
	*
	* @return A user
	* @return The database handle
	*/
	createUser :: !UserDetails !*st -> (!User,!*st)
	/**
	* Update an existing user
	* 
	* @param An existing user
	* @param The new user details to store
	* @param A unique database handle
	*
	* @return The existing user
	* @retrun The database handle 
	*/
	updateUser :: !User !UserDetails !*st -> (!User,!*st)
	/**
	* Delete an existing user
	*
	* @param An existing user
	* @param A unique database handle
	*
	* @return The existing user
	* @retrun The database handle 
	*/
	deleteUser :: !User !*st -> (!User,!*st)
	/**
	* Gets the timestamp of the last change of the user database.
	*
	* @param A unique database handle
	*
	* @return The timestamp
	* @retrun The database handle 
	*/
	lastChange :: !*st -> (!Timestamp,!*st)

instance UserDB IWorld
instance UserDB TSt
