definition module GinTypes

import GenEq
import iTasks
from PPrint import ::Doc

:: GTypeExpression = GConstructor GIdentifier
                   | GList GTypeExpression
                   | GTuple [GTypeExpression]
                   | GTypeApplication GTypeExpression GTypeExpression
                   | GTypeVariable GTypeVariable
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

:: GFormalParameter = { name :: GIdentifier
                      , type :: GTypeExpression
                      } 

:: GIdentifier :== String

derive bimap (,)
derive bimap Maybe

derive class iTask      GTypeExpression, GTypeDefinition, GTypeRhs, GDataConstructor, GRecordField, GFormalParameter
derive gEq              GTypeExpression, GTypeDefinition, GTypeRhs, GDataConstructor, GRecordField, GFormalParameter

typeIsDefined :: GTypeExpression -> Bool

printGTypeExpression :: GTypeExpression -> Doc

printGTypeDefinition :: GTypeDefinition -> Doc

gTask :: GTypeExpression -> GTypeExpression

gVoid :: GTypeExpression
