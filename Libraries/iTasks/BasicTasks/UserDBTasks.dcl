definition module UserDBTasks
/**
* This modules provides tasks to retrieve information from the user database
* of the iTask system. These tasks are useful when work is to be delegated to
* other users.
*/
import StdMaybe

from TSt	import :: Task
from Types	import :: UserId
from UserDB	import :: User

from InteractionTasks 	import class html

/**
* Finds a specific user
*/
getUser 			:: !UserId		-> Task User
/**
* Finds a specific user by username
*/
getUserByName		:: !String		-> Task User
/**
* Finds all users
*/
getUsers			:: Task [User]
/**
* Finds all users (user id + display name) who have the given role
*/
getUsersWithRole	:: !String	-> Task [User]
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
/**
* Authenticates a user by username and password
*/
authenticateUser	:: !String !String	-> Task (Maybe User)

/**
* Add a new user
*/
createUser			:: !String !String !String ![String] -> Task User



//Interactively choose a user
chooseUser			:: !question			-> Task User | html question
chooseUserWithRole	:: !question !String	-> Task User | html question