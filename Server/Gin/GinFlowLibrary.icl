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
		[ bStartStop, bCase, bMerge, bLet ]
	}
	
bStartStop :: Binding
bStartStop = ParallelBinding
	{ split = { GDeclaration 
	          | name = "start"
	          , returnType = GUndefinedTypeExpression
	          , formalParams = []
	          , icon = "start"
	          , shape = Just startShape
	          }
	, merge = { GDeclaration 
	          | name = "stop"
	          , returnType = GUndefinedTypeExpression
	          , formalParams = []
	          , icon = "stop"
	          , shape = Just stopShape
	          }
	, type = GTypeVariable "a"
	, fixedNrBranches = Just 1
	, parameterMap = Extension (PBBranch 0)
	}
where
	startShape :: SVGShape
	startShape = 
	  	{ SVGShape
		| width = 20
		, height = 20
		, defs = []
		, magnets = True
		, elements = 
			[ SVGPolygon Nothing [ (XLeft, YTop), (XPct 66, YPct 50), (XLeft, YBottom)] []
			]
		}
		
	stopShape :: SVGShape
	stopShape = 
	  	{ SVGShape
		| width = 20
		, height = 20
		, defs = []
		, magnets = True
		, elements = 
			[ SVGRect Nothing ((XLeft, YTop), (XRight, YBottom)) 0 0 []
			]
		}

bCase :: Binding
bCase = NodeBinding
	{ NodeBinding
	| declaration = 
		{ GDeclaration 
		| name = "case split"
		, returnType = GUndefinedTypeExpression
		, formalParams = [ { GFormalParameter 
		                 | name = "a"
		                 , type = GTypeVariable "a" 
		                 }
		               ]
		, icon = "case-split"
		, shape = Just caseSplitShape
		}
	, parameterMap = NBUndefined
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

bMerge :: Binding
bMerge = NodeBinding
	{ NodeBinding
	| declaration = 
		{ GDeclaration 
		| name = "case merge"
		, returnType = GUndefinedTypeExpression
		, formalParams = []
		, icon = "case-merge"
		, shape = Just mergeShape
		}
	, parameterMap = NBUndefined
	}
where
	mergeShape :: SVGShape
	mergeShape = 
	  	{ SVGShape
		| width = 20
		, height = 20
		, defs = []
		, magnets = True
		, elements = 
			[ SVGPolygon Nothing [ (XPct 50, YTop), (XRight, YPct 50), (XPct 50, YBottom), (XLeft, YPct 50)] [SVGAnchors "top left right bottom"] ]
		}

bLet :: Binding
bLet = NodeBinding
	{NodeBinding
	| declaration = 
		{ GDeclaration 
		| name = "let"
		, returnType = GUndefinedTypeExpression
		, formalParams = [ { GFormalParameter 
		                 | name = "pattern"
		                 , type = GUndefinedTypeExpression
		                 }
		                 , { GFormalParameter 
		                 | name = "expression"
		                 , type = GTypeVariable "a" 
		                 }
		               ]
		, icon = "let"
		, shape = Just letShape
		}
	, parameterMap = NBUndefined
	}
where
	letShape :: SVGShape
	letShape = 
	  	{ SVGShape
		| width = 100
		, height = 42
		, defs = []
		, magnets = True
		, elements = 
			[ SVGRect Nothing ((XLeft, YTop), (XRight, YBottom)) 0 0 [SVGAnchors "top left right bottom", SVGFill "white", SVGResize "horizontal vertical"]
			, SVGText (Just "pattern")    (XAbs 5, YAbs 13) ""  [SVGAnchors "top left", SVGAlign "middle left"]
			, SVGText Nothing             (XAbs 5, YAbs 33) "=" [SVGAnchors "top left", SVGAlign "middle left"]
			, SVGText (Just "expression") (XAbs 18, YAbs 33) "" [SVGAnchors "top left", SVGAlign "middle left"]
			]
		}
