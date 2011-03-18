implementation module GinFlowLibrary

import StdEnum
from StdFunc import o
import StdTuple

import GinAbstractSyntax
import GinSyntax
import GinParser
import GinSVG
import GinTypes

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
	          , shape = Just caseSplitShape
	          }
	, merge = { GDeclaration 
	          | name = "case merge"
	          , returnType = GUndefinedTypeExpression
	          , formalParams = []
	          , icon = "merge"
	          , shape = Just caseMergeShape
	          }
	, type = GTypeVariable "b"
	, fixedNrBranches = Nothing
	, parameterMap = Extension (PBApply mkCase)
	}
where
	caseSplitShape :: SVGShape
	caseSplitShape = 
	  	{ SVGShape
		| width = 120
		, height = 60
		, defs = []
		, magnets = True
		, elements = 
			[ SVGPolygon Nothing 
				[ (XPct 50, YTop), (XRight, YPct 50), (XPct 50, YBottom), (XLeft, YPct 50)] [SVGAnchors "top left right bottom", SVGFill "white", SVGResize "horizontal vertical"]
			, SVGText (Just "a") (XPct 50, YPct 50) "" [SVGAlign "middle center"]
			]
		}
		
	caseMergeShape :: SVGShape
	caseMergeShape = 
	  	{ SVGShape
		| width = 20
		, height = 20
		, defs = []
		, magnets = True
		, elements = 
			[ SVGPolygon Nothing [ (XPct 50, YTop), (XRight, YPct 50), (XPct 50, YBottom), (XLeft, YPct 50)] [SVGAnchors "top left right bottom"] ]
		}
    
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
