definition module WorkflowService
/**
* This module provides the workflow sevice. It lists the possible workflows that are available
* for execution.
* The workflows are organized in a folder hierarchy.
*/
import HTTP, TSt

workflowService :: !String !String ![String] !HTTPRequest !*TSt -> (!HTTPResponse, !*TSt)