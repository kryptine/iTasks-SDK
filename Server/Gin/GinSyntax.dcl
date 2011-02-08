definition module GinSyntax

import Maybe
import GenEq 

import GinTypes

// Graph definition
:: GModule = { name        :: GIdentifier
             , types       :: [GTypeDefinition]
             , definitions :: [GDefinition]
             , imports     :: [GImport]
             }
             
:: GImport = { name         :: GIdentifier
             , types        :: [GTypeDefinition]
             , declarations :: [GDeclaration]
             }

:: GDefinition = { declaration :: GDeclaration
                 , body        :: GExpression
                 , locals      :: [GDefinition]
                 }

:: GDeclaration = { name         :: GIdentifier
                  , formalParams :: [GFormalParameter]
                  , returnType   :: GTypeExpression
                  , icon         :: GIcon
                  , shape        :: GShape
                  }              
                  
:: GShape :== String //TODO: Algebraic datatype for shapes

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

:: GGraph = { edges :: [GEdge]
            , nodes :: [GNode]
            , size  :: Maybe GSize
            }

:: GNode = { actualParams :: [GExpression]
           , name         :: GIdentifier
           , position     :: GPosition
           }

:: GEdge = { fromNode :: Int
           , pattern  :: Maybe GPattern
           , toNode   :: Int
           }
           
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
derive class iTask GModule, GImport, GDefinition, GDeclaration, GExpression, GListComprehension, GGraph, GNode, GEdge, GPosition, GSize

// Selection functions
getPredecessors :: GGraph Int -> [Int]
getSuccessors :: GGraph Int -> [Int]
getSuccessorsEdges :: GGraph Int -> [Int]
getNode :: GGraph Int -> GNode
getNodePatternBefore :: GGraph Int -> Maybe String
getNodePatternAfter :: GGraph Int -> Maybe String
       
//JSON Serialization and deserialization
gModuleToJSON :: GModule -> String
gModuleFromJSON :: String -> Maybe GModule

//Construction
newWorkflow :: GDefinition
newModule :: GModule

