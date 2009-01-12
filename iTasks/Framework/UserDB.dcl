definition module UserDB
/**
* This module provides the abstract iTasks user database. It provides
* functions for accessing information about system users.
*/

import StdMaybe

/**
* A user database handle
*/
:: UserDB 

/**
* Opens the user database
*
* @param The world
* @return A user database handle
* @return The world
*/
openUserDB			:: 					!*World	->	(!*UserDB						, !*World)
/**
* Maps a list of user ids to a list of display names. Display names are strings which
* are suited for displaying directly in a user interface. For example "John Doe".
*
* @param A list of user ids
* @param A unique user database handle
* @return A list of display names
* @return The database handle
*/
getDisplayNames		:: ![Int]			!*UserDB -> (![String]						, !*UserDB)
/**
* Maps a list of user ids to a list user names. These user names are typically strings with
* certain limitations that are used to uniquely identify  users.
*
* @param A list of user ids
* @param A unique user database handle
* @return A list of user names
* @return The database handle
*/
getUserNames		:: ![Int]			!*UserDB -> (![String]						, !*UserDB)
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
getRoles			:: ![Int]			!*UserDB -> (![[String]]					, !*UserDB)
/**
* Finds a list of users that have a certain role.
*
* @param The role to look for
* @param A unique user database handle
* @return A list of user id/ display name tuples
* @return The database handle
*/
getUsersWithRole	:: !String			!*UserDB -> (![(Int,String)]				, !*UserDB)
/**
* Authenticate a user based on a user name or password
*
* @param A user name
* @param A password
* @param A unique database handle
* @return When successful, A triple of user id/ display name/ list of roles.
* @return The database handle
*/
authenticateUser	:: !String !String	!*UserDB -> (!Maybe (Int,String,[String])	, !*UserDB)