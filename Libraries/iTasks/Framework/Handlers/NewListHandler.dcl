definition module NewListHandler //iTasks.Handlers.NewListHandler
/**
* Handles the ajax requests from the 'start new work' panel.
*/
from TSt 	import :: TSt
from Http	import :: HTTPRequest, :: HTTPResponse

handleNewListRequest :: !HTTPRequest !*TSt -> (!HTTPResponse, !*TSt)