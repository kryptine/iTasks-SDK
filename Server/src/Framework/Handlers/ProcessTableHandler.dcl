definition module ProcessTableHandler
/**
* This provides a handler which visualizes all processes in the
* the iTasks process database.
*/
from TSt 	import :: TSt
from Http	import :: HTTPRequest, :: HTTPResponse
/**
* Handles the ajax requests for a ProcessTable tab panel.
*/
handleProcessTableRequest :: !HTTPRequest !*TSt -> (!HTTPResponse, !*TSt)