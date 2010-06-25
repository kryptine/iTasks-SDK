definition module TaskService
/**
* This module provides the task service.
* It provides all information about currently running workflow instances (processes)
* and the active tasks belonging to them.
*/
import Http, TSt

taskService :: !String !Bool ![String] !HTTPRequest *TSt -> (!HTTPResponse, !*TSt)