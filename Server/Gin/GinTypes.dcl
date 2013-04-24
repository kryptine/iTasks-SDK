definition module GinTypes

//import GenEq
//import HTML, UIDefinition

from Maybe import :: Maybe

from iTasks import class iTask

from StdGeneric import generic bimap, :: Bimap
from GenVisualize import generic gVisualizeText, generic gVisualizeEditor
from GenUpdate import generic gUpdate, generic gDefault, generic gHeaders, generic gGridRows
from GenVerify import generic gVerify
from JSON import generic JSONEncode, generic JSONDecode, :: JSONNode
from GenEq import generic gEq
from SystemTypes import :: InteractionMask, :: VerifyMask, :: VerifyOptions, :: ConsPos, :: StaticVisualizationMode, :: VSt, :: VisualizationResult

import GinPrinter

:: GTypeExpression = GConstructor GIdentifier
                   | GList GTypeExpression
                   | GTuple [GTypeExpression]
                   | GTypeApplication [GTypeExpression]
                   | GTypeVariable GTypeVariable
                   | GFunction GTypeExpression GTypeExpression
                   | GUndefinedTypeExpression
                   
:: GTypeVariable :== String 

:: GTypeDefinition = { name :: GIdentifier
                     , rhs  :: GTypeRhs
                     }

:: GTypeRhs = GAlgebraicTypeRhs [GDataConstructor]
            | GRecordTypeRhs [GRecordField]
            | GSynonymTypeRhs GTypeExpression
            | GAbstractTypeRhs
            
:: GDataConstructor = { name      :: GIdentifier
                      , arguments :: [GTypeExpression]
                      }

:: GRecordField = { name :: GIdentifier
                  , type :: GTypeExpression
                  }

:: GFormalParameter = { name			:: GIdentifier
					  , title			:: Maybe String
					  , description		:: Maybe String
                      , type			:: GTypeExpression
                      , defaultValue	:: Maybe String
                      , visible			:: Bool
                      } 

:: GIdentifier :== String

derive bimap (,)
derive bimap Maybe

derive class iTask      GTypeExpression, GTypeDefinition, GTypeRhs, GDataConstructor, GRecordField, GFormalParameter

typeIsDefined :: GTypeExpression -> Bool

printGTypeExpression :: Bool GTypeExpression -> a | Printer a

printGTypeDefinition :: GTypeDefinition -> a | Printer a

instance toString GTypeExpression

isTask :: !GTypeExpression -> Bool

gTask :: GTypeExpression -> GTypeExpression

gVoid :: GTypeExpression

