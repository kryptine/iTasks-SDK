definition module TaskService
/**
* This module provides the task service.
* It provides all information about currently running workflow instances (processes)
* and the active tasks belonging to them.
*/
from HTTP	import :: HTTPRequest, :: HTTPResponse
from IWorld	import :: IWorld
import iTaskClass

taskService :: !(Task a) !HTTPRequest !*IWorld -> (!HTTPResponse, !*IWorld) | iTask a