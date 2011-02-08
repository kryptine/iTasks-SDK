definition module SessionService
/**
* This module provides the session service. It provides an authentication framework that
* creates temporary access codes with which the other services can be accessed.
*/
import HTTP, TSt

sessionService :: !String !Bool ![String] !HTTPRequest *TSt -> (!HTTPResponse, !*TSt)
