implementation module GinFlowLibrary

import StdEnum
from StdFunc import o
import StdTuple

import GinTypes
import GinSyntax
import GinAbstractSyntax
import GinParser

predefinedModule :: GModule
predefinedModule = 
	{ GModule
	| name = "(Clean language)"
	, imports = 
		[ "StdEnv"
		]
	, types =	
		[ { name = "Bool"   , rhs = GAbstractTypeRhs }
		, { name = "Char"   , rhs = GAbstractTypeRhs }
		, { name = "Int"    , rhs = GAbstractTypeRhs }
		, { name = "Real"   , rhs = GAbstractTypeRhs }
		, { name = "String" , rhs = GAbstractTypeRhs }
		, { name = "Task"   , rhs = GAbstractTypeRhs }
		, { name = "Void"   , rhs = GAbstractTypeRhs }				  
		]
	, moduleKind = GCleanModule
		[ bCase ]
	}

bCase :: Binding
bCase = ParallelBinding
  { split = { GDeclaration 
              | name = "case split"
              , returnType = GUndefinedTypeExpression
              , formalParams = [ { GFormalParameter 
                                 | name = "a"
                                 , type = GTypeVariable "a" 
                                 }
                               ]
              , icon = "case"
              , shape = "embed:rhombus"
              }
    , merge = { GDeclaration 
              | name = "case merge"
              , returnType = GUndefinedTypeExpression
              , formalParams = []
              , icon = "merge"
              , shape = "icon:merge"
              }
    , type = GTypeVariable "b"
    , fixedNrBranches = Nothing
    , parameterMap = Extension (PBApply mkCase)
    }
    where
      mkCase :: [AExpression Void] [AExpression Void] [(Maybe APattern, AExpression Void)] -> GParseState (AExpression Void)
      mkCase splitParams mergeParams alts = 
        if (length (filter isNothing (map fst alts)) > 1)
          (parseError "A case expression can have at most one default case")
          //"otherwise" alternative is put at the end of the list
          (ret (let sortalts = filter (isJust o fst) alts ++ filter (isNothing o fst) alts
                in  Case (splitParams !! 0) (map mkCaseAlt alts)))
      
      mkCaseAlt :: (Maybe APattern, AExpression Void) -> ACaseAlt Void
      mkCaseAlt (Just pat, exp) = CaseAlt pat exp
      mkCaseAlt (Nothing, exp)  = CaseAlt "otherwise" exp
