definition module GinORYX

import JSON

import GinSyntax

::ORYXEditor = { diagram	:: ORYXDiagram
			   , stencilset	:: ORYXStencilSet
			   , toString	:: (ORYXEditor -> String)
			   }

:: ORYXBound =	
	{ x :: !Real
	, y :: !Real
	}

:: ORYXBounds =	
	{ lowerRight	:: !ORYXBound
	, upperLeft		:: !ORYXBound
	}

:: ORYXChildShape =	
	{ resourceId	:: !ORYXResourceId
	, properties	:: !ORYXProperties
	, stencil		:: !ORYXStencil
	, childShapes	:: ![ORYXChildShape]
	, outgoing		:: ![ORYXOutgoing]
	, bounds		:: !ORYXBounds
	, dockers		:: ![ORYXDocker]
	, target		:: Maybe ORYXTarget
	}
	
:: ORYXProperties = ORYXProperties [ORYXProperty]

:: ORYXProperty =
	{ key	:: !String
	, value	:: !JSONNode
	}
	
:: ORYXDiagram = 
	{ resourceId	:: !ORYXResourceId
	, properties	:: !ORYXProperties
	, stencil	 	:: !ORYXStencil
	, childShapes	:: ![ORYXChildShape]
	, bounds		:: !ORYXBounds
	, stencilset	:: !ORYXStencilSet
	, ssextensions	:: ![ORYXStencilSetExtension]
	}
	
:: ORYXDocker = 
	{ x	:: !Real
	, y :: !Real
	}
	
:: ORYXOutgoing =
	{ resourceId	:: !ORYXResourceId
	}

:: ORYXResourceId :== String

:: ORYXStencil = 
	{ id			:: !ORYXResourceId
	}

:: ORYXStencilSet =
	{ url		:: !String
	, namespace	:: !String
	}

:: ORYXStencilSetExtension :== String

:: ORYXTarget =
	{ resourceId	:: !ORYXResourceId
	}

derive gEq		 	ORYXBound, ORYXBounds, ORYXChildShape, ORYXDiagram, ORYXDocker, ORYXOutgoing, ORYXProperty,   ORYXStencil, ORYXStencilSet, ORYXTarget
derive JSONEncode	ORYXBound, ORYXBounds, ORYXChildShape, ORYXDiagram, ORYXDocker, ORYXOutgoing, ORYXProperties, ORYXStencil, ORYXStencilSet, ORYXTarget
derive JSONDecode 	ORYXBound, ORYXBounds, ORYXChildShape, ORYXDiagram, ORYXDocker, ORYXOutgoing, ORYXProperties, ORYXStencil, ORYXStencilSet, ORYXTarget


petriNetORYXEditor :: ORYXEditor

workflowNetORYXEditor :: ORYXEditor

ginORYXEditor :: ORYXEditor

newORYXDiagram :: ORYXStencilSet -> ORYXDiagram
