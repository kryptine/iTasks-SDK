definition module IndexHandler //iTasks.Handlers.IndexHandler

import Http

/**
* Handles the 'index' request. This generates a minimal page which loads the required
* javascript code to jumpstart the client driven boot process.
*/
handleIndexRequest :: !HTTPRequest *World -> (!HTTPResponse, !*World)