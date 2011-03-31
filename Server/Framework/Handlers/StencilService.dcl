definition module StencilService
/**
* This module provides the Stencil service. It lists the possible stencils that are available
* for placement in graphical workflow diagrams.
*/
import HTTP, TSt

stencilService :: !String !String ![String] !HTTPRequest !*TSt -> (!HTTPResponse, !*TSt)