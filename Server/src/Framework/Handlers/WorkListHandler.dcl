definition module WorkListHandler //iTasks.Handlers.WorkListHandler
/**
* Handles the ajax requests from the current work filter panel.
*/
from TSt 	import :: TSt
from Http	import :: HTTPRequest, :: HTTPResponse

handleWorkListRequest :: !HTTPRequest !*TSt -> (!HTTPResponse, !*TSt)