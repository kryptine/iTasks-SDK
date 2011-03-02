implementation module GinSyntax

//import StdEnv
import iTasks
import JSON
import GinTypes, GinFlowLibrary

// Generic functions
derive class iTask GModule, GImport, GDefinition, GDeclaration, GExpression, GListComprehension, GGraph, GNode, GEdge, GPosition, GSize

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
newWorkflow = { declaration = { name         = "newWorkflow"
                              , formalParams = []
                              , returnType   = gTask gVoid
                              , icon         = "task"
                              , shape        = "app:task"
                              }
              , body = GGraphExpression {GGraph | nodes=[], edges=[], size=Nothing}
              , locals = []
              }
              
newModule :: GModule
newModule = { name = "newModule"
			, types = []
			, definitions = [newWorkflow]
			, imports = []
			}
