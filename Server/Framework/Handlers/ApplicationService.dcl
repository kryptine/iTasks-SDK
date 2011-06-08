definition module ApplicationService
/**
* This module provides the application service.
* It serves basic meta data about the application such as name and build.
*/
from HTTP import :: HTTPRequest, :: HTTPResponse
from IWorld import :: IWorld

applicationService :: !String !String ![String] !HTTPRequest !*IWorld -> (!HTTPResponse, !*IWorld)