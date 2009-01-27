definition module TaskTreeForestHandler
/**
* This module provides a handler that computes and visualizes the complete! task forest
* of all relevant processes for the current user.
*/
import Http, TSt

handleTaskTreeForestRequest :: !HTTPRequest *TSt -> (!HTTPResponse, !*TSt)