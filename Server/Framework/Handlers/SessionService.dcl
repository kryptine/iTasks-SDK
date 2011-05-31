definition module SessionService
/**
* This module provides the session service. It provides an authentication framework that
* creates temporary access codes with which the other services can be accessed.
*/
from HTTP	import :: HTTPRequest, :: HTTPResponse
from Types	import :: IWorld

sessionService :: !String !String ![String] !HTTPRequest !*IWorld -> (!HTTPResponse, !*IWorld)
