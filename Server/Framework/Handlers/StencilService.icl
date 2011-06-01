implementation module StencilService

import StdArray, StdString, StdInt, StdList, StdBool, StdClass
import Types, SessionDB, HtmlUtil
import HTTP, Text, Error
import GenEq

import GinConfig
import GinStorage
import GinFlowLibrary
import GinORYXStencil
import GinORYXExtensions

from Util import mb2list

stencilService :: !String !String ![String] !HTTPRequest !*IWorld -> (!HTTPResponse, !*IWorld)
stencilService url format path req iworld
	# iworld=:{world}		= iworld
	# (mConfig,world)		= ginLoadConfig world	
	| isNothing mConfig
		= (errorResponse "Failed to load configuration", {iworld & world = world})
	| otherwise
		= case path of
		[]
			# (modules, world) = searchPathModules (fromJust mConfig) world  
			= (okResponse (makeORYXExtensionsFile modules), {iworld & world = world})
		["gin"]
			= (okResponse predefinedStencilSet, iworld)
		["gin", name]
			# (mbContents, world) =  readModule (fromJust mConfig) name world  
			| isError mbContents = (errorResponse (fromError mbContents), {iworld & world = world})
			= (okResponse (makeStencilSet (fromOk mbContents)), {iworld & world = world})
		_
			= (notFoundResponse req, {iworld & world = world})
where
	html			= format == "html"
	sessionParam	= paramValue "session" req
	params			= [("session", sessionParam, False)]
	
	errorResponse message
		# json	= JSONObject [("success",JSONBool False),("error", JSONString message)]
		= serviceResponse html "stencils" description url params json
	
	okResponse stencilset
		# json	= toJSON stencilset
		= serviceResponse html "stencils" description url params json

description :== "This service provides a list of stencils that are available for placement in graphical workflow diagrams."

