definition module ApplicationService
/**
* This module provides the application service.
* It serves basic meta data about the application such as name and build.
*/
import HTTP, TSt

applicationService :: !String !String ![String] !HTTPRequest !*TSt -> (!HTTPResponse, !*TSt)