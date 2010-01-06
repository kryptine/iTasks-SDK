definition module WorkTabHandler
/**
* This module provides a handler function that generates the
* content of the "work tabs". These are the collections of tasks
* that are combined as a single unit of work in the user interface.
*/
from TSt 	import :: TSt
from Http	import :: HTTPRequest, :: HTTPResponse

handleWorkTabRequest :: !HTTPRequest !*TSt -> (!HTTPResponse, !*TSt)