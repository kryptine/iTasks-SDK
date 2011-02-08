definition module DocumentService
/**
* This module provides the document service.
* It provides uploading and downloading of (large) documents that are further
* processed by workflows.
*/
import HTTP, TSt

documentService :: !String !Bool ![String] !HTTPRequest *TSt -> (!HTTPResponse, !*TSt)