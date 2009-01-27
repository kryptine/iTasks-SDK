definition module ProcessTableHandler
/**
* This provides a handler which visualizes all processes in the
* the iTasks process database.
*/
import Http, TSt

/**
* Handles the ajax requests for a ProcessTable tab panel.
*/
handleProcessTableRequest :: !HTTPRequest !*TSt -> (!HTTPResponse, !*TSt)