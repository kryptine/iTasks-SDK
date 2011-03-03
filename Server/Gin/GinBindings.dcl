definition module GinBindings

import Maybe
import GinSyntax
import GinAbstractSyntax

:: ModuleBindings = { name     :: GIdentifier
                    , types    :: [GTypeDefinition]
                    , bindings :: Bindings
                    }

:: Bindings :== [Binding]

:: Binding = NodeBinding NodeBinding | ParallelBinding ParallelBinding

/**
* NodeBinding: binds a single node declaration to a Clean function 
*/

:: NodeBinding = { declaration  :: GDeclaration
                 , parameterMap :: NBParameterMap
                 }
                   
:: NBParameterMap = NBPrefixApp  
                  | NBInfixApp AFix APrecedence
                  | NBCustom (AExpression ParameterPosition)

/**
* Sequential composition is hard coded, maps to either >>= (with pattern) or >>| (without pattern).
*/

/** 
* ParallelBinding: binds a parallel composition of nodes to a Clean function 
*/

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
               | PBApply ([AExpression Void] [AExpression Void] [(Maybe APattern, AExpression Void)] -> GParseState (AExpression Void))

:: ParameterPosition :== Int
:: BranchPosition :== Int


getNodeBinding :: GIdentifier Bindings -> GParseState NodeBinding
getParallelBinding :: GIdentifier GIdentifier Bindings -> GParseState ParallelBinding

:: BranchType = BTSingle | BTSplit | BTMerge
getBranchType :: GIdentifier Bindings -> GParseState BranchType

//importDeclarations :: ModuleBindings -> GImport

mkGDefinitionBinding :: GDefinition -> Binding

getModuleDeclarations :: ModuleBindings -> [(BranchType,GDeclaration)]

getDeclarations :: Bindings -> [(BranchType,GDeclaration)]
