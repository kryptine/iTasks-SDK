implementation module GinSyntax

import iTasks
import JSON

import GinAbstractSyntax
import GinORYX
import GinParser
import GinSVG
import GinTypes

// Generic functions
derive class iTask GModule, GModuleKind, Binding, NodeBinding, NBParameterMap, ParallelBinding, PBParameter, GDefinition, GDeclaration

getNodeBinding :: GIdentifier Bindings -> GParseState NodeBinding
getNodeBinding name [] = parseError ("Node binding " +++ name +++ " not found")
getNodeBinding name [b:bs] = case b of
	NodeBinding sb | sb.NodeBinding.declaration.GDeclaration.name == name = ret sb
	otherwise = getNodeBinding name bs

getParallelBinding :: GIdentifier GIdentifier Bindings -> GParseState ParallelBinding
getParallelBinding split merge bindings = case getParallelBinding` split merge bindings of
	Just pb = ret pb
	Nothing = parseError "Invalid split/merge combination"

isParallelBinding :: GIdentifier GIdentifier Bindings -> Bool
isParallelBinding split merge bindings = isJust (getParallelBinding` split merge bindings)

getParallelBinding` :: GIdentifier GIdentifier Bindings -> Maybe ParallelBinding
getParallelBinding` split merge [] = Nothing
getParallelBinding` split merge [b:bs] = case b of
	ParallelBinding pb | pb.ParallelBinding.split.GDeclaration.name == split 
						 && pb.ParallelBinding.merge.GDeclaration.name == merge = Just pb
	otherwise = getParallelBinding` split merge bs

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
                              , icon         = Nothing
                              , shape        = Nothing
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