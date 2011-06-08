definition module WorkflowService
/**
* This module provides the workflow sevice. It lists the possible workflows that are available
* for execution.
* The workflows are organized in a folder hierarchy.
*/
from HTTP	import :: HTTPRequest, :: HTTPResponse
from IWorld	import :: IWorld

workflowService :: !String !String ![String] !HTTPRequest !*IWorld -> (!HTTPResponse, !*IWorld)