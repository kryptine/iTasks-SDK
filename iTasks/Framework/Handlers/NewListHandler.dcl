definition module NewListHandler //iTasks.Handlers.NewListHandler

import Http, TSt

/**
* Handles the ajax requests from the 'start new work' panel.
*/
handleNewListRequest :: !HTTPRequest !*TSt -> (!HTTPResponse, !*TSt)