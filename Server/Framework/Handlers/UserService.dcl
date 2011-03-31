definition module UserService
/**
* This module provides the user service that can be used to query
* information about users of the application.
*/
import HTTP, TSt

userService :: !String !String ![String] !HTTPRequest !*TSt -> (!HTTPResponse, !*TSt)