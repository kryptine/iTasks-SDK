definition module ApplicationService
/**
* This module provides the application service.
* It serves basic meta data about the application such as name and build.
*/
import Http, TSt

applicationService :: !String !Bool ![String] !HTTPRequest *TSt -> (!HTTPResponse, !*TSt)