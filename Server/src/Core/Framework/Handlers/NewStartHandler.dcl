definition module NewStartHandler
/**
* Handles the ajax requests from the 'start new work' panel.
*/
from TSt 	import :: TSt
from Http	import :: HTTPRequest, :: HTTPResponse

handleNewStartRequest :: !HTTPRequest !*TSt -> (!HTTPResponse, !*TSt)