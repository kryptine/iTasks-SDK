definition module NewStartHandler

import Http, TSt

/**
* Handles the ajax requests from the 'start new work' panel.
*/
handleNewStartRequest :: !HTTPRequest !*TSt -> (!HTTPResponse, !*TSt)