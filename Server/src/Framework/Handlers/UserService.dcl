definition module UserService
/**
* This module provides the user service that can be used to query
* information about users of the application.
*/
import Http, TSt

userService :: !String !Bool ![String] !HTTPRequest *TSt -> (!HTTPResponse, !*TSt)