definition module TaskTreeForestHandler
/**
* This module provides a handler that computes and visualizes the complete! task forest
* of all relevant processes for the current user.
*/
from TSt 	import :: TSt
from Http	import :: HTTPRequest, :: HTTPResponse

handleTaskForestRequest :: !HTTPRequest *TSt -> (!HTTPResponse, !*TSt)