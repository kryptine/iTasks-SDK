implementation module StencilService

import HTTP, TSt, UserDB
import HtmlUtil, Text
import StdArray, StdString, StdInt, StdList, StdBool, StdClass
import GenEq

import GinBindings
import GinFlowLibrary
import GinORYXStencil

from Util import mb2list

stencilService :: !String !Bool ![String] !HTTPRequest *TSt -> (!HTTPResponse, !*TSt)
stencilService url html path req tst
	= case path of
		["gin"]
			// Restore the session if supplied
			# (mbErr,tst)		= if ( sessionParam <> "") (initSession sessionParam tst) (Nothing,tst)	
			| isJust mbErr
				# json	= JSONObject [("success",JSONBool False),("error", JSONString (fromJust mbErr))]
				= (serviceResponse html "stencils" description url params json, tst)
			# declarations = flatten ((map getModuleDeclarations) flowLibrary)
			# components		= makeStencilSet declarations
			# json 				= toJSON components
			= (serviceResponse html "workflows" description url params json, tst)
		_
			= (notFoundResponse req, tst)

where
	sessionParam= paramValue "session" req
	params 		= [("session", sessionParam, False)]

description :== "This service provides a list of stencils that are available for placement in graphical workflow diagrams."
