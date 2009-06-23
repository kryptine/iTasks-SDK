definition module UserDB
/**
* This module provides the iTasks user database. It provides
* functions for accessing information about system users.
*/
import StdMaybe
import HSt, TSt

:: User 	= { userId		:: !Int
			  , username	:: !String
			  , password	:: !String
			  , displayname	:: !String
			  , roles		:: ![String]
			  }

class UserDB st
where
	/**
	* Fetches the id and display name from the user database for a given user id.
	*
	* @param A user id
	* @param A unique user database handle
	* @return The user id and display name combination
	* @return The database handle
	*/
	getUser				::	!Int			!*st -> (!(Int,String)					, !*st)
	/**
	* Fetches the id and display name of all users from the from the user database.
	*
	* @param A unique user database handle
	* @return The list of all user id and display name combinations
	* @return The database handle
	*/
	getUsers 			:: 					!*st -> (![(Int,String)]				, !*st)
	/**
	* Finds a list of users that have a certain role.
	*
	* @param The role to look for
	* @param A unique user database handle
	* @return A list of user id/ display name tuples
	* @return The database handle
	*/
	getUsersWithRole	:: !String			!*st -> (![(Int,String)]				, !*st)
	/**
	* Maps a list of user ids to a list of display names. Display names are strings which
	* are suited for displaying directly in a user interface. For example "John Doe".
	*
	* @param A list of user ids
	* @param A unique user database handle
	* @return A list of display names
	* @return The database handle
	*/
	getDisplayNames		:: ![Int]			!*st -> (![String]						, !*st)
	/**
	* Maps a list of user ids to a list user names. These user names are typically strings with
	* certain limitations that are used to uniquely identify  users.
	*
	* @param A list of user ids
	* @param A unique user database handle
	* @return A list of user names
	* @return The database handle
	*/
	getUserNames		:: ![Int]			!*st -> (![String]						, !*st)
	/**
	* Maps a list of user ids to a list of a list of roles. Roles are strings that identify a certain
	* role a user may play in a workflow scenario and can be used to determine what a user is allowed
	* to do.
	*
	* @param A list of user ids
	* @param A unique user database handle
	* @return A list of a list of roles (a list of roles for each user)
	* @return The database handle 
	*/
	getRoles			:: ![Int]			!*st -> (![[String]]					, !*st)
	/**
	* Authenticate a user based on a user name or password
	*
	* @param A user name
	* @param A password
	* @param A unique database handle
	* @return When successful, A triple of user id/ display name/ list of roles.
	* @return The database handle
	*/
	authenticateUser	:: !String !String	!*st -> (!Maybe (Int,String,[String])	, !*st)
	
instance UserDB HSt
instance UserDB TSt