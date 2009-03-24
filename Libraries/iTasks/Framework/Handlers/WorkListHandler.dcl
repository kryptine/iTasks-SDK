definition module WorkListHandler //iTasks.Handlers.WorkListHandler

import Http, TSt

/**
* Handles the ajax requests from the current work filter panel.
*/
handleWorkListRequest :: !HTTPRequest !*TSt -> (!HTTPResponse, !*TSt)