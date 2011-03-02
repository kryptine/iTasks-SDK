implementation module GinORYX

from StdFunc import o
import StdList
import StdMisc

import Maybe
import JSON

import GinSyntax
import GinORYX

derive gEq		 	ORYXBound, ORYXBounds, ORYXChildShape, ORYXDiagram, ORYXDocker, ORYXOutgoing, ORYXProperties, ORYXProperty, ORYXStencilReference, ORYXStencilSetReference, ORYXTarget
derive JSONEncode	ORYXBound, ORYXBounds, ORYXDiagram, ORYXDocker, ORYXOutgoing, ORYXStencilReference, ORYXStencilSetReference, ORYXTarget
derive JSONDecode 	ORYXBound, ORYXBounds, ORYXDiagram, ORYXDocker, ORYXOutgoing, ORYXStencilReference, ORYXStencilSetReference, ORYXTarget

JSONEncode{|ORYXChildShape|} {resourceId, properties, stencil, childShapes, outgoing, bounds, dockers, target}
	# target` = case target of
		Just t	= [("target", toJSON t)]
		Nothing	= []
	# fields =
		[ ("resourceId"	, toJSON resourceId)
		, ("properties"	, toJSON properties)
		, ("stencil"	, toJSON stencil)
		, ("childShapes", toJSON childShapes)
		, ("outgoing"	, toJSON outgoing)
		, ("bounds"		, toJSON bounds)
		, ("dockers"	, toJSON dockers)
		: target` ]
	= [JSONObject fields]

JSONDecode{|ORYXChildShape|} [node:nodes]
	# mResourceId	= jsonQuery "resourceId"	node
	# mProperties	= jsonQuery "properties"	node
	# mStencil		= jsonQuery "stencil"		node
	# mChildShapes	= jsonQuery "childShapes"	node
	# mOutgoing		= jsonQuery "outgoing"		node
	# mBounds		= jsonQuery "bounds"		node
	# mDockers		= jsonQuery "dockers"		node
	# mTarget		= jsonQuery "target"		node
	| isNothing mResourceId		= (Nothing, nodes)
	| isNothing mProperties		= (Nothing, nodes)
	| isNothing mStencil		= (Nothing, nodes)
	| isNothing mChildShapes	= (Nothing, nodes)
	| isNothing mOutgoing		= (Nothing, nodes)
	| isNothing mBounds			= (Nothing, nodes)
	| isNothing mDockers		= (Nothing, nodes)
	=	(Just	{ ORYXChildShape
				| resourceId	= fromJust mResourceId
				, properties	= fromJust mProperties
				, stencil		= fromJust mStencil
				, childShapes	= fromJust mChildShapes
				, outgoing		= fromJust mOutgoing
				, bounds		= fromJust mBounds
				, dockers		= fromJust mDockers
				, target		= mTarget
				}
		, nodes)

JSONEncode{|ORYXProperties|} (ORYXProperties properties)
	=	[JSONObject (map (\{ORYXProperty | key, value} -> (key, value)) properties)]

JSONDecode{|ORYXProperties|} [JSONObject fields:nodes]
	# properties = ORYXProperties (map (\(key, value) -> {ORYXProperty | key = key, value = value}) fields)
	= (Just properties, nodes)
JSONDecode{|ORYXProperties|} nodes = (Nothing, nodes)

petriNetORYXEditor :: ORYXEditor
petriNetORYXEditor = newORYXEditor petriNetStencilSet
where
	petriNetStencilSet :: ORYXStencilSetReference
	petriNetStencilSet 
		=	{ ORYXStencilSetReference 
	   		| url = "petrinets/petrinet.json"
			, namespace = "http://b3mn.org/stencilset/petrinet#"
			}
			
workflowNetORYXEditor :: ORYXEditor
workflowNetORYXEditor = newORYXEditor workflowNetStencilSet
where
	workflowNetStencilSet :: ORYXStencilSetReference
	workflowNetStencilSet 
		=	{ ORYXStencilSetReference 
	   		| url = "workflownets/workflownets.json"
			, namespace = "http://www.example.org/workflownets#"
			}

ginORYXEditor :: ORYXEditor
ginORYXEditor = newORYXEditor ginStencilSet
where
	ginStencilSet :: ORYXStencilSetReference
	ginStencilSet
		=	{ ORYXStencilSetReference 
	   		| url = "/services/json/stencils/gin"
			, namespace = "http://mbsd.icis.ru.nl/itasks/gin#"
			}
			
newORYXEditor :: ORYXStencilSetReference -> ORYXEditor
newORYXEditor stencilset
	=	{ ORYXEditor
		| diagram = newORYXDiagram stencilset
		, stencilset = stencilset
		, toString = \_ -> "" //TODO
		}

newORYXDiagram :: ORYXStencilSetReference -> ORYXDiagram
newORYXDiagram stencilset
	=	{ ORYXDiagram
					| resourceId = "oryx_78E522C8-943A-44FF-B391-14BE8630F198"
					, properties =	ORYXProperties []
					, stencil = { ORYXStencilReference
								| id = "Diagram"
								}
					, childShapes = []
					, bounds =	{ ORYXBounds
								| lowerRight = { ORYXBound | x = 1485.0, y = 1050.0 }
								, upperLeft = { ORYXBound | x = 0.0, y = 0.0 }
								}
					, stencilset = stencilset
					, ssextensions = []
					}

