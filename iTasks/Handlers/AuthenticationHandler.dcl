definition module AuthenticationHandler //iTasks.Handlers.AuthenticationHandler

import Http

/**
* Handles the ajax request from the login window. It validates the username and
* password and returns session information to the client.
*/
handleAuthenticationRequest :: !HTTPRequest *World -> (!HTTPResponse, !*World)