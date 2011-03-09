definition module GinAbstractSyntax

import StdOverloaded
import GinTypes

from iTaskClass import class iTask, gVisualize, gUpdate, gDefaultMask, gVerify, JSONEncode, JSONDecode, gEq

from GinParser import ::GPath, ::GParseState
import Void

:: AModule = { name        :: AIdentifier
             , types       :: [GTypeDefinition]
             , definitions :: [ADefinition]
             , imports     :: [AImport]
             }
             
:: AImport :== String

:: ADefinition = { name         :: AIdentifier
                 , formalParams :: [GFormalParameter]
                 , returnType   :: GTypeExpression
                 , body         :: AExpression Void
                 , locals       :: [ADefinition]
                 }
:: AExpression ex = 
    Unparsed String
    | Lit String
    | Var AIdentifier
    | App [AExpression ex]
    | AppInfix AIdentifier AFix APrecedence (AExpression ex) (AExpression ex) 
    | Lambda APattern (AExpression ex)
    | Case (AExpression ex) [ACaseAlt ex]
    | Tuple [AExpression ex]
    | List [AExpression ex]
    | ListComprehension (AListComprehension ex)
    | PathContext GPath (AExpression ex)
    | Extension ex

:: ACaseAlt ex = CaseAlt APattern (AExpression ex)

:: AListComprehension ex = { output :: (AExpression ex)
                           , generators :: (AGeneratorList ex)
                           , guards :: [AExpression ex]
                           }

:: AGeneratorList ex = ANestedGeneratorList [AGenerator ex] | AParallelGeneratorList [AGenerator ex]

:: AGenerator ex = Generator APattern (AExpression ex)

:: APattern :== String

:: AIdentifier :== String

:: AFix = Infixl | Infixr | Infix
:: APrecedence :== Int

derive class iTask AModule, ADefinition, AExpression, ACaseAlt, AListComprehension, AGeneratorList, AGenerator, AFix

expandModule :: AModule -> AModule

//instance toString AModule

::PrintOption = PathContexts 
instance == PrintOption

renderAModule :: [PrintOption] AModule -> String


