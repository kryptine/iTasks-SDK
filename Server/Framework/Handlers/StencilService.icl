implementation module StencilService

import HTTP, TSt, UserDB
import HtmlUtil, Text
import StdArray, StdString, StdInt, StdList, StdBool, StdClass
import GenEq

import GinConfig
import GinStorage
import GinFlowLibrary
import GinORYXStencil
import GinORYXExtensions

from Util import mb2list

stencilService :: !String !Bool ![String] !HTTPRequest *TSt -> (!HTTPResponse, !*TSt)
stencilService url html path req tst
	// Restore the session if supplied
	# (mbErr,tst) = if ( sessionParam <> "") (initSession sessionParam tst) (Nothing,tst)	
	| isJust mbErr = (errorResponse (fromJust mbErr), tst)
	# (mConfig,tst) = accWorldTSt ginLoadConfig tst
	| isNothing mConfig = (errorResponse "Failed to load configuration", tst)
	= case path of
		[]
			# (modules, tst) = accWorldTSt (searchPathModules (fromJust mConfig)) tst  
			= (okResponse (makeORYXExtensionsFile modules), tst)
		["gin"] = (okResponse predefinedStencilSet, tst)
		["gin", name]
			# (mbContents, tst) = accWorldTSt (readModule (fromJust mConfig) name) tst  
			| isError mbContents = (errorResponse (fromError mbContents), tst)
			= (okResponse (makeStencilSet (fromOk mbContents)), tst)
		_
			= (notFoundResponse req, tst)
where
	sessionParam= paramValue "session" req
	params 		= [("session", sessionParam, False)]
	
	errorResponse message
		# json	= JSONObject [("success",JSONBool False),("error", JSONString message)]
		= serviceResponse html "stencils" description url params json
	
	okResponse stencilset
		# json	= toJSON stencilset
		= serviceResponse html "stencils" description url params json

description :== "This service provides a list of stencils that are available for placement in graphical workflow diagrams."

