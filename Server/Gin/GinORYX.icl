implementation module GinORYX

import StdList
import StdMisc

import Maybe
import JSON

import GinSyntax
import GinORYX

derive gEq		 	ORYXBound, ORYXBounds, ORYXChildShape, ORYXDiagram, ORYXDocker, ORYXOutgoing, ORYXProperties, ORYXProperty, ORYXStencil, ORYXStencilSet, ORYXTarget
derive JSONEncode	ORYXBound, ORYXBounds, ORYXChildShape, ORYXDiagram, ORYXDocker, ORYXOutgoing, ORYXStencil, ORYXStencilSet, ORYXTarget
derive JSONDecode 	ORYXBound, ORYXBounds, ORYXChildShape, ORYXDiagram, ORYXDocker, ORYXOutgoing, ORYXStencil, ORYXStencilSet, ORYXTarget

JSONEncode{|ORYXProperties|} (ORYXProperties properties)
	=	[JSONObject (map (\{ORYXProperty | key, value} -> (key, value)) properties)]

JSONDecode{|ORYXProperties|} [JSONObject fields:nodes]
	# properties = ORYXProperties (map (\(key, value) -> {ORYXProperty | key = key, value = value}) fields)
	= (Just properties, nodes)
JSONDecode{|ORYXProperties|} nodes = (Nothing, nodes)

petriNetORYXEditor :: ORYXEditor
petriNetORYXEditor = newORYXEditor petriNetStencilSet
where
	petriNetStencilSet :: ORYXStencilSet
	petriNetStencilSet 
		=	{ ORYXStencilSet 
	   		| url = "petrinets/petrinet.json"
			, namespace = "http://b3mn.org/stencilset/petrinet#"
			}
			
workflowNetORYXEditor :: ORYXEditor
workflowNetORYXEditor = newORYXEditor workflowNetStencilSet
where
	workflowNetStencilSet :: ORYXStencilSet
	workflowNetStencilSet 
		=	{ ORYXStencilSet 
	   		| url = "workflownets/workflownets.json"
			, namespace = "http://www.example.org/workflownets#"
			}

ginORYXEditor :: ORYXEditor
ginORYXEditor = petriNetORYXEditor
/*
ginORYXEditor = newORYXEditor ginStencilSet
where
	ginStencilSet :: ORYXStencilSet
	ginStencilSet
		=	{ ORYXStencilSet 
	   		| url = "../../lib/oryx/stencilsets/gin/gin.json"
			, namespace = "http://mbsd.icis.ru.nl/itasks/gin#"
			}
*/
			
newORYXEditor :: ORYXStencilSet -> ORYXEditor
newORYXEditor stencilset
	=	{ ORYXEditor
		| diagram = newORYXDiagram stencilset
		, stencilset = stencilset
		, toString = \_ -> "" //TODO
		}

newORYXDiagram :: ORYXStencilSet -> ORYXDiagram
newORYXDiagram stencilset
	=	{ ORYXDiagram
					| resourceId = "oryx_78E522C8-943A-44FF-B391-14BE8630F198"
					, properties =	ORYXProperties []
					, stencil = { ORYXStencil
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


