implementation module GinORYX

import StdBool
import StdEnum
from StdFunc import const,o,flip
import StdList
import StdMisc
import StdTuple

import JSON
import Map
import Maybe
import Text

import iTasks

import GinAbstractSyntax
import GinFlowLibrary
import GinParser
import GinSyntax

derive gEq		 		ORYXBound, ORYXBounds, ORYXChildShape, ORYXDiagram, ORYXDocker, ORYXOutgoing, ORYXProperties, ORYXProperty, ORYXStencilReference, ORYXStencilSetReference, ORYXTarget, ORYXError
derive JSONEncode		ORYXBound, ORYXBounds, ORYXDiagram, ORYXDocker, ORYXOutgoing, ORYXStencilReference, ORYXStencilSetReference, ORYXTarget, ORYXError
derive JSONDecode 		ORYXBound, ORYXBounds, ORYXDiagram, ORYXDocker, ORYXOutgoing, ORYXStencilReference, ORYXStencilSetReference, ORYXTarget, ORYXError
derive gVisualize  	 	ORYXBound, ORYXBounds, ORYXChildShape, ORYXDiagram, ORYXDocker, ORYXOutgoing, ORYXProperties, ORYXProperty, ORYXStencilReference, ORYXStencilSetReference, ORYXTarget, ORYXError, JSONNode
derive gUpdate	    	ORYXBound, ORYXBounds, ORYXChildShape, ORYXDiagram, ORYXDocker, ORYXOutgoing, ORYXProperties, ORYXProperty, ORYXStencilReference, ORYXStencilSetReference, ORYXTarget, ORYXError, JSONNode
derive gDefaultMask		ORYXBound, ORYXBounds, ORYXChildShape, ORYXDiagram, ORYXDocker, ORYXOutgoing, ORYXProperties, ORYXProperty, ORYXStencilReference, ORYXStencilSetReference, ORYXTarget, ORYXError, JSONNode
derive gVerify  		ORYXBound, ORYXBounds, ORYXChildShape, ORYXDiagram, ORYXDocker, ORYXOutgoing, ORYXProperties, ORYXProperty, ORYXStencilReference, ORYXStencilSetReference, ORYXTarget, ORYXError, JSONNode

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
		, verify = oryxAlwaysValid
		}
where
	oryxAlwaysValid :: !ORYXEditor *IWorld -> (!WorldPredicateResult,!*IWorld)
	oryxAlwaysValid _ iworld = (WPRValid Nothing, iworld)
		
emptyORYXEditor :: ORYXEditor
emptyORYXEditor = newORYXEditor emptyStencilSet
where
	emptyStencilSet :: ORYXStencilSetReference
	emptyStencilSet 
		=	{ ORYXStencilSetReference 
			| url = ""
			, namespace = ""
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
					
bpmnORYXEditor :: ORYXEditor
bpmnORYXEditor = newORYXEditor bpmnStencilSet
where
	bpmnStencilSet :: ORYXStencilSetReference
	bpmnStencilSet 
		=	{ ORYXStencilSetReference 
	   		| url = "bpmn2.0/bpmn2.0.json"
			, namespace = "http://b3mn.org/stencilset/bpmn2.0#"
			}
			
ginORYXDiagram :: ORYXDiagram
ginORYXDiagram = newORYXDiagram ginStencilSet

//Gin specific:
ginORYXEditor :: !ORYXDiagram !(ORYXEditor *IWorld -> *(WorldPredicateResult,*IWorld))-> ORYXEditor
ginORYXEditor diagram verify = 
	{ ORYXEditor
	| newORYXEditor ginStencilSet
	& diagram = diagram
	, verify = verify
	}
	
ginStencilSet :: ORYXStencilSetReference
ginStencilSet 
	=	{ ORYXStencilSetReference 
   		| url = "/services/json/stencils/gin"
		, namespace = "http://mbsd.icis.ru.nl/itasks/gin#"
		}
					
oryxDiagramToGraph :: !Bindings !ORYXDiagram -> GGraph
oryxDiagramToGraph bindings diagram 
	= oryxChildShapesToGraph bindings diagram.ORYXDiagram.childShapes

oryxChildShapesToGraph :: !Bindings ![ORYXChildShape] -> GGraph
oryxChildShapesToGraph bindings shapes
	// shapeMap :: Map ORYXResourceId ORYXChildShape
	# shapeMap = (fromList o map (\shape -> (shapeId shape, shape)))  shapes
	// nodes :: [(NodeIndex, ORYXChildShape)]
	# (nodes, graph) = addShapes (filter (not o isEdge) shapes) emptyGraph
	// nodeMap :: Map ORYXResourceId NodeIndex
	# nodeMap = (fromList o map (\(index,node) -> (shapeId node, index))) nodes
	//find outgoing edges for each node
	# edges = (flatten o map (oryxChildShapeToEdges shapeMap nodeMap)) nodes
	= GGraph (addEdges edges graph)
	where
		addShapes :: ![ORYXChildShape] !(Graph GNode GEdge) -> ([(NodeIndex,ORYXChildShape)], Graph GNode GEdge)
		addShapes [] graph = ([], graph)
		addShapes [shape:shapes] graph
			# (index, graph) = addNode (oryxChildShapeToNode bindings shape) graph
			# (indexedShapes, graph) = addShapes shapes graph
			= ([(index,shape):indexedShapes], graph)
		
		addEdges :: ![(EdgeIndex,GEdge)] !(Graph GNode GEdge) -> Graph GNode GEdge
		addEdges []                       graph = graph
		addEdges [(edgeIndex,edge):edges] graph = addEdges edges (addEdge edge edgeIndex graph)

oryxChildShapeToNode :: !Bindings !ORYXChildShape -> GNode
oryxChildShapeToNode bindings shape
	# mDecl = runParse (getDeclaration (shapeName shape) bindings)
	= case mDecl of 
		GError [(_, err)] = abort ("oryxChildShapeToNode: Invalid shape " +++ shapeName shape)
		GSuccess decl = 
			{ GNode
			| name = shapeName shape
			, position =	{ GPosition 
							| x = shape.ORYXChildShape.bounds.ORYXBounds.upperLeft.ORYXBound.x
							, y = shape.ORYXChildShape.bounds.ORYXBounds.upperLeft.ORYXBound.y
							}
			, actualParams = oryxChildShapeToActualParams bindings (snd decl) shape
			}

oryxChildShapeToActualParams :: !Bindings GDeclaration !ORYXChildShape -> [GExpression]
oryxChildShapeToActualParams bindings decl shape
	# propMap = propertyMap (shape.ORYXChildShape.properties)
	= map (oryxChildShapeToActualParam bindings shape propMap) decl.GDeclaration.formalParams
 	  
oryxChildShapeToActualParam :: !Bindings !ORYXChildShape (Map String JSONNode) !GFormalParameter -> GExpression
oryxChildShapeToActualParam bindings childShape propMap formalParam
	| isHigherOrderTask formalParam.GFormalParameter.type
	  && (not o isEmpty) childShape.ORYXChildShape.childShapes
	  = GGraphExpression (oryxChildShapesToGraph bindings childShape.ORYXChildShape.childShapes)
	= case get formalParam.GFormalParameter.name propMap of
		Just (JSONString value) = GCleanExpression value
		Nothing	   				= abort ("oryxChildShapeToActualParam: " +++ formalParam.GFormalParameter.name +++ " paramter not found")
	
isHigherOrderTask :: !GTypeExpression -> Bool
isHigherOrderTask (GTypeApplication (GConstructor "Task") _) = True
isHigherOrderTask _											 = False
 
oryxChildShapeToEdges :: (Map ORYXResourceId ORYXChildShape) (Map ORYXResourceId Int) (!Int,!ORYXChildShape) -> [(EdgeIndex,GEdge)]
oryxChildShapeToEdges shapeMap nodeMap (fromIndex,fromNode) = 
	catMaybes (map (oryxOutgoingToEdge shapeMap nodeMap fromIndex) fromNode.ORYXChildShape.outgoing)

oryxOutgoingToEdge :: (Map ORYXResourceId ORYXChildShape) (Map ORYXResourceId Int) !Int !ORYXOutgoing -> Maybe (EdgeIndex,GEdge)
oryxOutgoingToEdge shapeMap nodeMap fromIndex arcres =
	case get arcres.ORYXOutgoing.resourceId shapeMap of
		Just arc 
				 = case arc.ORYXChildShape.outgoing of
			[toRes]	= case get toRes.ORYXOutgoing.resourceId nodeMap of
						  Just toIndex = Just ((fromIndex,toIndex), oryxPropertiesToPattern arc.ORYXChildShape.properties)
					  	  Nothing      = abort "oryxChildShapeToEdge: Arc outgoing resourceId not found"
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

updateDiagramExtensions :: !GModule -> GModule
updateDiagramExtensions gmod =: { moduleKind = GCleanModule _ }
	= gmod //GCleanModule does not contain diagrams
updateDiagramExtensions gmod =: { moduleKind = GGraphicalModule definitions }
	=	{ GModule
		| gmod
		& moduleKind = GGraphicalModule (map updateDefinition definitions)
		}
where
	updateDefinition :: !GDefinition -> GDefinition
	updateDefinition gdef =
		{ GDefinition | gdef & body = updateDiagram gdef.GDefinition.body }

	updateDiagram :: !ORYXDiagram -> ORYXDiagram
	updateDiagram diagram = 
		{ ORYXDiagram
		| diagram
		& ssextensions = [ "http://mbsd.icis.ru.nl/itasks/gin/" +++ imp +++ "#"
							\\ imp <- gmod.GModule.imports ]
		}

makeORYXError :: !ORYXDiagram !(GPath,String) -> Maybe ORYXError
makeORYXError diagram (p,message) = makeORYXError` (reverse p)
where
	makeORYXError` :: [GPathNode] -> Maybe ORYXError
	makeORYXError` [] = 
		Just	{ ORYXError
				| resourceId = diagram.ORYXDiagram.resourceId
				, message = message
				, paramIndex = Nothing
				}
	makeORYXError` [PNDefinition _ : path] = makeORYXError` path
	makeORYXError` [PNBody:path] = makeORYXError` path
	makeORYXError` [PNNode index:path]
		# nodes = filter (not o isEdge) diagram.ORYXDiagram.childShapes
		| index < 0 || index >= length nodes = Nothing
		= makeORYXErrorChild (nodes !! index) path message
	makeORYXError` [PNEdge index:path]
		# edges = filter isEdge diagram.ORYXDiagram.childShapes
		| index < 0 || index >= length edges = Nothing
		= makeORYXErrorChild (edges !! index) path message
	makeORYXError` path = Nothing //TODO

makeORYXErrorChild :: !ORYXChildShape ![GPathNode] !String -> Maybe ORYXError
makeORYXErrorChild shape [] message = 
	Just	{ ORYXError
			| resourceId = shape.ORYXChildShape.resourceId
			, message = message
			, paramIndex = Nothing
			}
makeORYXErrorChild shape [PNActualParam index] message = 
	Just	{ ORYXError
			| resourceId = shape.ORYXChildShape.resourceId
			, message = message
			, paramIndex = Just index
			}
makeORYXErrorChild shape [PNActualParam index: path] message
	# params = shape.ORYXChildShape.childShapes
	| index < 0 || index >= length params = Nothing
	= makeORYXErrorChild (params !! index) path message
makeORYXErrorChild _ path message = abort ("abort in makeORYXErrorChild: Path=" +++ toString path +++ ",message=" +++ message)
