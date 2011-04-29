definition module GinSyntax

import Graph
import Maybe

from iTasks import ::JSONNode, ::VerSt, ::UpdateMask, ::USt, ::UpdateMode, ::VSt, ::Visualization
from iTasks import class iTask, generic gVisualize, generic gUpdate, generic gDefaultMask, generic gVerify, generic JSONEncode, generic JSONDecode, generic gEq

import GinAbstractSyntax
from GinORYX import ::ORYXDiagram
from GinSVG import ::SVGShape
import GinTypes

// Graph definition
:: GModule = { name        :: GIdentifier
             , types       :: [GTypeDefinition]
             , moduleKind  :: GModuleKind
             , imports     :: [GImport]
             }
             
:: GImport :== String

:: GModuleKind	= GCleanModule Bindings
				| GGraphicalModule [GDefinition]
				
:: Bindings :== [Binding]
				
:: Binding = NodeBinding NodeBinding | ParallelBinding ParallelBinding

//For GCleanModule:

// NodeBinding: binds a single node declaration to a Clean function 
:: NodeBinding = { declaration  :: GDeclaration
                 , parameterMap :: NBParameterMap
                 }
                   
:: NBParameterMap = NBUndefined
				  | NBPrefixApp  
                  | NBInfixApp AFix APrecedence
                  | NBApply ([AExpression Void] -> GParseState (AExpression Void))

//Sequential composition is hard coded, maps to either >>= (with pattern) or >>| (without pattern).
//ParallelBinding: binds a parallel composition of nodes to a Clean function 
:: ParallelBinding = { split           :: GDeclaration
                     , merge           :: GDeclaration
                     , type            :: GTypeExpression
                     , fixedNrBranches :: Maybe Int
                     , parameterMap    :: AExpression PBParameter
                     }

:: PBParameter = PBSplitParameter ParameterPosition
               | PBMergeParameter ParameterPosition
               | PBBranch BranchPosition
               | PBBranchList

:: ParameterPosition :== Int
:: BranchPosition :== Int

getNodeBinding :: GIdentifier Bindings -> GParseState NodeBinding
getParallelBinding :: GIdentifier GIdentifier Bindings -> GParseState ParallelBinding
isParallelBinding :: GIdentifier GIdentifier Bindings -> Bool

:: BranchType = BTSingle | BTSplit | BTMerge
getDeclaration :: GIdentifier Bindings -> GParseState (BranchType, GDeclaration)

getModuleBindings :: GModule -> Bindings
getDefinitionBinding :: GDefinition -> Binding

getModuleDeclarations :: GModule -> [(BranchType,GDeclaration)]

//For GGraphicalModule:

:: GDefinition = { declaration :: GDeclaration
                 , body        :: ORYXDiagram //GExpression
                 }

:: GDeclaration = { name         :: GIdentifier
                  , formalParams :: [GFormalParameter]
                  , returnType   :: GTypeExpression
                  , icon         :: GIcon
                  , shape        :: Maybe SVGShape
                  }              
                  
:: GIcon :== String

:: GExpression = GUndefinedExpression
               | GGraphExpression GGraph
               | GListExpression [GExpression]
               | GListComprehensionExpression GListComprehension
               | GCleanExpression GCleanExpression
               
:: GListComprehension = { output :: GExpression
                        , guard :: Maybe GCleanExpression
                        , selector :: GPattern
                        , input :: GExpression
                        }

:: GGraph = GGraph (Graph GNode GEdge)

:: GNode = { actualParams :: [GExpression]
           , name         :: GIdentifier
           , position     :: GPosition
           }

:: GEdge :== Maybe GPattern
           
:: GPattern :== String

:: GCleanExpression :== String

:: GPosition = { x :: Real
               , y :: Real
               }
               
:: GSize = { height :: Real
           , width  :: Real
           }
           
:: GDescription :== String

// Generic functions
derive class iTask GModule, GModuleKind, Binding, NodeBinding, NBParameterMap, ParallelBinding, PBParameter, GDefinition, GDeclaration

//JSON Serialization and deserialization
gModuleToJSON :: GModule -> String
gModuleFromJSON :: String -> Maybe GModule

//Construction
newWorkflow :: GDefinition
newModule :: GModule
