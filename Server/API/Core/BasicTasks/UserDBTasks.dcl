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
*/
getUser 			:: !UserId	-> Task (Maybe User)
/**
* Finds all users
*/
getUsers			:: Task [User]
/**
* Finds all users (user id + display name) who have the given role
*/
getUsersWithRole	:: !Role	-> Task [User]
/**
* Authenticates a user by username and password
*/
authenticateUser	:: !String !String	-> Task (Maybe User)
/**
* Add a new user
*/
createUser			:: !UserDetails -> Task User
/**
* Update an existing user
*/
updateUser			:: !User !UserDetails -> Task User
/**
* Delete an existing user
*/
deleteUser			:: !User -> Task User

//Interactively choose a user
chooseUser			:: !question			-> Task User | html question
chooseUserWithRole	:: !question !String	-> Task User | html question