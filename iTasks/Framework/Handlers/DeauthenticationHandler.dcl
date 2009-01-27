definition module DeauthenticationHandler //iTasks.Framework.Handlers.DeauthenticationHandler

import Http, TSt
/**
* Handles the ajax requests to terminate the current session
*/
handleDeauthenticationRequest :: !HTTPRequest !*TSt -> (!HTTPResponse, !*TSt)