definition module GinTypes

import GenEq, GenPrint, GenParse, GenVisualize, GenUpdate, GenMerge
import JSON
from PPrint import ::Doc

:: GTypeExpression = GUndefinedTypeExpression
                   | GBasicTypeExpression GIdentifier
                   | GAbstractTypeExpression GIdentifier
                   | GList GTypeExpression
                   | GTuple [GTypeExpression]
                   | GArray GTypeExpression
                   | GConstructor GIdentifier
                   | GTypeApplication GTypeExpression GTypeExpression
                   | GTypeVariable GTypeVariable

:: GTypeVariable :== String 

:: GTypeDefinition = { name       :: GIdentifier
                     , expression :: GTypeExpression
                     }

:: GFormalParameter = { name :: GIdentifier
                      , type :: GTypeExpression
                      } 

:: GIdentifier :== String

derive bimap (,)
derive bimap Maybe

derive class iTask      GTypeExpression, GTypeDefinition, GFormalParameter
derive gMerge           GTypeExpression, GTypeDefinition, GFormalParameter
derive gEq              GTypeExpression, GTypeDefinition, GFormalParameter

typeIsDefined :: GTypeExpression -> Bool

printGTypeExpression :: GTypeExpression -> Doc

printGTypeDefinition :: GTypeDefinition -> Doc

gTask :: GTypeExpression -> GTypeExpression

gVoid :: GTypeExpression
