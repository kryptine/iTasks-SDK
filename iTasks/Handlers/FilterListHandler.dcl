definition module FilterListHandler //iTasks.Handlers.FilterListHandler

import Http

/**
* Handles the ajax requests from the current work filter panel.
*/
handleFilterListRequest :: !HTTPRequest *World -> (!HTTPResponse, !*World)