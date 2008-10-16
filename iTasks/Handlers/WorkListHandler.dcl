definition module WorkListHandler //iTasks.Handlers.WorkListHandler

import Http

/**
* Handles the ajax requests from the current work filter panel.
*/
handleWorkListRequest :: !HTTPRequest *World -> (!HTTPResponse, !*World)