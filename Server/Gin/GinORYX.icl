implementation module GinORYX

import StdEnum
from StdFunc import const,o
import StdList
import StdMisc
import StdTuple

import Maybe
import JSON
import Text

import GinBindings
import GinFlowLibrary
import GinSyntax
import GinAbstractSyntax
import GinORYX

import Map

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

newORYXEditor :: ORYXStencilSetReference -> ORYXEditor
newORYXEditor stencilset
	=	{ ORYXEditor
		| diagram = newORYXDiagram stencilset
		, stencilset = stencilset
		, toString = const "newORYXEditor: toString is undefined"
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
					
petriNetORYXEditor :: ORYXEditor
petriNetORYXEditor = newORYXEditor petriNetStencilSet
where
	petriNetStencilSet :: ORYXStencilSetReference
	petriNetStencilSet 
		=	{ ORYXStencilSetReference 
	   		| url = "petrinets/petrinet.json"
			, namespace = "http://b3mn.org/stencilset/petrinet#"
			}


bpmnORYXEditor :: ORYXEditor
bpmnORYXEditor = newORYXEditor bpmnStencilSet
where
	bpmnStencilSet :: ORYXStencilSetReference
	bpmnStencilSet 
		=	{ ORYXStencilSetReference 
	   		| url = "bpmn2.0/bpmn2.0.json"
			, namespace = "http://b3mn.org/stencilset/bpmn2.0#"
			}

//Gin specific:

ginORYXEditor :: ORYXEditor
ginORYXEditor = 
	{ ORYXEditor
	| newORYXEditor ginStencilSet
	& toString = \editor -> tryRender (simpleGModule editor.ORYXEditor.diagram)
	}
where
	ginStencilSet :: ORYXStencilSetReference
	ginStencilSet
		=	{ ORYXStencilSetReference 
	   		| url = "/services/json/stencils/gin"
			, namespace = "http://mbsd.icis.ru.nl/itasks/gin#"
			}
	
	tryRender :: GModule -> String
	tryRender gMod = 
	    case runParse (gToAModule gMod) of
	        GSuccess aMod -> renderAModule [] aMod
	        GError errors -> "Parse error:\n" +++ ((join "\n" (map (\(path,msg) = toString path +++ ":" +++ msg) errors)))
					
simpleGModule :: !ORYXDiagram -> GModule
simpleGModule diagram = 
	{ GModule 
	| newModule
	& definitions = [ { GDefinition
					  | newWorkflow
					  & body = GGraphExpression (oryxDiagramToGraph diagram)
					  }
					]

	}

oryxDiagramToGraph :: !ORYXDiagram -> GGraph
oryxDiagramToGraph diagram
	# declMap = (fromList o map (\(bt,decl) -> (decl.GDeclaration.name, (bt,decl))) o flatten o map getModuleDeclarations) flowLibrary
	# shapes = diagram.ORYXDiagram.childShapes
	# shapeMap = (fromList o map (\shape -> (shapeId shape, shape)))  shapes
	# nodes =  (zip2 [0..] o filter (not o isEdge)) shapes
	# nodeMap = (fromList o map (\(index,node) -> (shapeId node, index))) nodes
	=	{ GGraph
		| nodes = map (oryxChildShapeToNode declMap o snd) nodes
		, edges = (flatten o map (oryxChildShapesToEdge shapeMap nodeMap)) nodes
		, size = Nothing
		}

oryxChildShapeToNode :: (Map String (BranchType,GDeclaration)) !ORYXChildShape -> GNode
oryxChildShapeToNode declMap shape
	# mDecl = get (shapeName shape) declMap
	| isNothing mDecl = abort ("oryxChildShapeToNode: Invalid shape " +++ shapeName shape)
    =	{ GNode
		| name = shapeName shape
		, position =	{ GPosition 
						| x = shape.ORYXChildShape.bounds.ORYXBounds.upperLeft.ORYXBound.x
						, y = shape.ORYXChildShape.bounds.ORYXBounds.upperLeft.ORYXBound.y
						}
		, actualParams = oryxPropertiesToGExpressions (snd (fromJust mDecl)) shape.ORYXChildShape.properties 
		}

oryxPropertiesToGExpressions :: GDeclaration !ORYXProperties -> [GExpression]
oryxPropertiesToGExpressions decl properties
	# propMap = propertyMap properties
 	= [ maybe GUndefinedExpression jsonNodetoGExpression (get param.GFormalParameter.name propMap)
 	    \\ param <- decl.GDeclaration.formalParams 
 	  ]
 	  
jsonNodetoGExpression :: JSONNode -> GExpression
jsonNodetoGExpression (JSONString s)	= GCleanExpression s
jsonNodetoGExpression _				= GUndefinedExpression
	
oryxChildShapesToEdge :: (Map ORYXResourceId ORYXChildShape) (Map ORYXResourceId Int) (!Int,!ORYXChildShape) -> [GEdge]
oryxChildShapesToEdge shapeMap nodeMap (fromIndex,fromNode) = 
	catMaybes (map (oryxChildShapeToEdge shapeMap nodeMap fromIndex) fromNode.ORYXChildShape.outgoing)

oryxChildShapeToEdge :: (Map ORYXResourceId ORYXChildShape) (Map ORYXResourceId Int) !Int !ORYXOutgoing -> Maybe GEdge
oryxChildShapeToEdge shapeMap nodeMap fromIndex arcres =
	case get arcres.ORYXOutgoing.resourceId shapeMap of
		Just arc 
				 = case arc.ORYXChildShape.outgoing of
			[toRes]	= case get toRes.ORYXOutgoing.resourceId nodeMap of
						  Just toIndex = Just
						  	{ GEdge
						  	| fromNode = fromIndex
						  	, pattern = oryxPropertiesToPattern arc.ORYXChildShape.properties
						  	, toNode = toIndex
						    }
					  	  Nothing = abort "oryxChildShapeToEdge: Arc outgoing resourceId not found"
			[]		= Nothing //Arc not connected to node
			_		= abort "oryxChildShapeToEdge: arc cannot point to multiple nodes"
		Nothing = abort "oryxChildShapeToEdge: Node outgoing resourceId not found"
	
oryxPropertiesToPattern :: !ORYXProperties -> Maybe GPattern
oryxPropertiesToPattern properties
	= case get "pattern" (propertyMap properties) of
		Just (JSONString s)  = case trim s of
								   "" = Nothing
							       s` = Just s`
		_					 = Nothing

propertyMap :: !ORYXProperties -> Map String JSONNode
propertyMap (ORYXProperties properties) = fromList [ (p.ORYXProperty.key, p.ORYXProperty.value) \\ p <- properties ]

shapeId :: !ORYXChildShape -> String
shapeId shape = shape.ORYXChildShape.resourceId
	
shapeName :: !ORYXChildShape -> String
shapeName shape = shape.ORYXChildShape.stencil.ORYXStencilReference.id

isEdge :: !ORYXChildShape -> Bool
isEdge shape = shapeName shape == "Arc"

