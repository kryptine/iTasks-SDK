definition module UserTasks
/**
* This modules provides tasks to retrieve information about other the users
* of the iTask system. These tasks are useful when work is to be delegated to
* other users.
*/
from TSt import :: Task

/**
* Looks up the corresponding display names for a list of user ids
*/
getDisplayNamesTask 	:: ![Int] 	-> Task [String]
/**
* Looks up the corresponding user names for a list of user ids
*/
getUserNamesTask		:: ![Int]	-> Task [String]
/**
* Looks up the corresponding roles for a list of user ids
*/
getRolesTask			:: ![Int]	-> Task [[String]]
/**
* Finds all users (user id + display name) who have the given role
*/
getUsersWithRoleTask	:: !String	-> Task [(Int,String)]
/**
* Finds all users ids
*/
getUsersIds	::  (Task [Int])