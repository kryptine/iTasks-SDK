definition module DeauthenticationHandler //iTasks.Framework.Handlers.DeauthenticationHandler

import Http, Session
/**
* Handles the ajax requests to terminate the current session
*/
handleDeauthenticationRequest :: !HTTPRequest !Session *HSt -> (!HTTPResponse, !*HSt)