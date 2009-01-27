definition module AuthenticationHandler

import Http
import HSt

/**
* Handles the ajax request from the login window. It validates the username and
* password and returns session information to the client.
*/
handleAuthenticationRequest :: !HTTPRequest *HSt -> (!HTTPResponse, !*HSt)