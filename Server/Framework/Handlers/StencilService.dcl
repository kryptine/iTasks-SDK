definition module StencilService
/**
* This module provides the Stencil service. It lists the possible stencils that are available
* for placement in graphical workflow diagrams.
*/
from HTTP	import :: HTTPRequest, :: HTTPResponse
from IWorld	import :: IWorld

stencilService :: !String !String ![String] !HTTPRequest !*IWorld -> (!HTTPResponse, !*IWorld)