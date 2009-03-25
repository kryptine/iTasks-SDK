definition module UserTasks
/**
* This modules provides tasks to retrieve information about other the users
* of the iTask system. These tasks are useful when work is to be delegated to
* other users.
*/
from TSt	import :: Task
from Types	import :: UserId

//User database access

/**
* Returns user id of user logged in the iTask system
*/
getCurrentUser		::  Task (UserId, String)
/**
* Finds a specific user
*/
getUser 			:: !UserId		-> Task (UserId, String)
/**
* Finds all users
*/
getUsers			:: Task [(UserId, String)]
/**
* Finds all users (user id + display name) who have the given role
*/
getUsersWithRole	:: !String	-> Task [(UserId, String)]
/**
* Looks up the corresponding display names for a list of user ids
*/
getDisplayNames 	:: ![UserId] 	-> Task [String]
/**
* Looks up the corresponding user names for a list of user ids
*/
getUserNames		:: ![UserId]	-> Task [String]
/**
* Looks up the corresponding roles for a list of user ids
*/
getRoles			:: ![UserId]	-> Task [[String]]

//Interactively choose a user
chooseUser			:: 				Task (UserId, String)
chooseUserWithRole	:: !String	-> 	Task (UserId, String)
