definition module DocumentService
/**
* This module provides the document service.
* It provides uploading and downloading of (large) documents that are further
* processed by workflows.
*/
from HTTP	import :: HTTPRequest, :: HTTPResponse
from Types	import :: IWorld

documentService :: !String !String ![String] !HTTPRequest !*IWorld -> (!HTTPResponse, !*IWorld)