implementation module GinSyntax

//import StdEnv
import iTasks
import JSON

import GinORYX
import GinTypes
import GinParser
import GinAbstractSyntax
from GinSVG import ::SVGShape

// Generic functions
derive class iTask GModule, GModuleKind, Binding, NodeBinding, NBParameterMap, ParallelBinding, PBParameter
derive class iTask GDefinition, GDeclaration, GExpression, GListComprehension, GGraph, GNode, GEdge, GPosition, GSize

getNodeBinding :: GIdentifier Bindings -> GParseState NodeBinding
getNodeBinding name [] = parseError ("Node binding " +++ name +++ " not found")
getNodeBinding name [b:bs] = case b of
	NodeBinding sb | sb.NodeBinding.declaration.GDeclaration.name == name = ret sb
	otherwise = getNodeBinding name bs

getParallelBinding :: GIdentifier GIdentifier Bindings -> GParseState ParallelBinding
getParallelBinding split merge [] = parseError ("Invalid split/merge combination (" +++ split +++ "," +++ merge +++ ")")
getParallelBinding split merge [b:bs] = case b of
	ParallelBinding pb | pb.ParallelBinding.split.GDeclaration.name == split 
						 && pb.ParallelBinding.merge.GDeclaration.name == merge = ret pb
	otherwise = getParallelBinding split merge bs
	
getDeclaration :: GIdentifier Bindings -> GParseState (BranchType, GDeclaration)
getDeclaration name [] = parseError ("Node binding " +++ name  +++ " not found")
getDeclaration name [b:bs] = case b of
	NodeBinding sb     | sb.NodeBinding.declaration.GDeclaration.name == name = ret (BTSingle, sb.NodeBinding.declaration)
	ParallelBinding pb | pb.ParallelBinding.split.GDeclaration.name   == name = ret (BTSplit,  pb.ParallelBinding.split)
	ParallelBinding pb | pb.ParallelBinding.merge.GDeclaration.name   == name = ret (BTMerge,  pb.ParallelBinding.merge)
	otherwise = getDeclaration name bs

getModuleBindings :: GModule -> Bindings
getModuleBindings gMod =: { moduleKind = GCleanModule bindings } = bindings
getModuleBindings gMod =: { moduleKind = GGraphicalModule definitions } = map getDefinitionBinding definitions

getDefinitionBinding :: GDefinition -> Binding
getDefinitionBinding gdef = 
	NodeBinding { NodeBinding 
                | declaration = gdef.GDefinition.declaration
				, parameterMap = NBPrefixApp
				}

getModuleDeclarations :: GModule -> [(BranchType,GDeclaration)]
getModuleDeclarations gMod = 
	case gMod.moduleKind of
		(GCleanModule bindings) = flatten (map get bindings)
		(GGraphicalModule defs) = [(BTSingle, decl) \\ decl <- [ def.GDefinition.declaration \\ def <- defs ] ]
where
	get :: Binding -> [(BranchType,GDeclaration)]
	get (NodeBinding nb) = [(BTSingle, nb.NodeBinding.declaration)]
	get (ParallelBinding pb) = [ (BTSplit, pb.ParallelBinding.split)
							   , (BTMerge, pb.ParallelBinding.merge) ]

// Selection functions
getPredecessors :: GGraph Int -> [Int]
getPredecessors graph node = [ e.fromNode \\ e <- graph.edges | e.toNode == node ]

getSuccessors :: GGraph Int -> [Int]
getSuccessors graph node = [ e.toNode \\ e <- graph.edges | e.fromNode == node ]

getSuccessorsEdges :: GGraph Int -> [Int]
getSuccessorsEdges graph node = [ snd e \\ e <- zip2 graph.edges [0..] | (fst e).fromNode == node ]

getNode :: GGraph Int -> GNode
getNode graph index = graph.nodes !! index

getNodePatternBefore :: GGraph Int -> Maybe String
getNodePatternBefore graph node = 
    case [ e.GEdge.pattern \\ e <- graph.edges | e.toNode == node ] of
    []        = Nothing
    [pattern] = pattern

getNodePatternAfter :: GGraph Int -> Maybe String
getNodePatternAfter graph node = 
    case [ e.GEdge.pattern \\ e <- graph.edges | e.fromNode == node ] of
    []        = Nothing
    [pattern] = pattern

//JSON Serialization and deserialization
gModuleToJSON :: GModule -> String
gModuleToJSON g = toString (toJSON g)

gModuleFromJSON :: String -> Maybe GModule
gModuleFromJSON s = fromJSON (fromString s)

//Construction
newWorkflow :: GDefinition
newWorkflow =	{ GDefinition
				| declaration = { name         = "newWorkflow"
                              , formalParams = []
                              , returnType   = gTask gVoid
                              , icon         = "task"
                              , shape        = ""
                              }
				, body = ginORYXDiagram
				}
              
newModule :: GModule
newModule = { GModule
			| name = "newModule"
			, types = []
			, moduleKind = GGraphicalModule [newWorkflow]
			, imports = []
			}