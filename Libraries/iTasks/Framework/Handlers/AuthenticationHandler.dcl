definition module AuthenticationHandler

import Http
import TSt

/**
* Handles the ajax request from the login window. It validates the username and
* password and returns session information to the client.
*/
handleAuthenticationRequest :: !HTTPRequest *TSt -> (!HTTPResponse, !*TSt)