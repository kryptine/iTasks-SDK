definition module DeauthenticationHandler //iTasks.Framework.Handlers.DeauthenticationHandler
/**
* Handles the ajax requests to terminate the current session
*/
from TSt 	import :: TSt
from Http	import :: HTTPRequest, :: HTTPResponse

handleDeauthenticationRequest :: !HTTPRequest !*TSt -> (!HTTPResponse, !*TSt)