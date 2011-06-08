definition module UserService
/**
* This module provides the user service that can be used to query
* information about users of the application.
*/
from HTTP	import :: HTTPRequest, :: HTTPResponse
from IWorld	import :: IWorld

userService :: !String !String ![String] !HTTPRequest !*IWorld -> (!HTTPResponse, !*IWorld)