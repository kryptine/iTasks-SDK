implementation module GinORYXStencil

import StdEnum
import StdList
import GenEq

import JSON
import Text

import GinSyntax
import GinORYX
import GinSVG

derive gEq		 	ORYXStencilSet, ORYXStencil, ORYXRules, ORYXConnectionRule, ORYXConnect, ORYXContainmentRule, ORYXMorphingRule
derive JSONEncode	ORYXStencilSet, ORYXStencil, ORYXRules, ORYXConnectionRule, ORYXContainmentRule, ORYXMorphingRule
derive JSONDecode 	ORYXStencilSet, ORYXStencil, ORYXRules, ORYXConnectionRule, ORYXContainmentRule, ORYXMorphingRule


JSONEncode{|ORYXConnect|} {from_, to} = 
	[JSONObject [ ("from", toJSON from_)
				, ("to", toJSON to)
				]
	]

JSONDecode{|ORYXConnect|} [node:nodes]
	# mFrom	= jsonQuery "from"	node
	# mTo	= jsonQuery "to"	node
	| isNothing mFrom		= (Nothing, nodes)
	| isNothing mTo			= (Nothing, nodes)
	= (Just { ORYXConnect 
			| from_ = fromJust mFrom
			, to	= fromJust mTo
			}
	  , nodes)

makeStencilSet :: [GDeclaration] -> ORYXStencilSet
makeStencilSet decls =
	{ ORYXStencilSet
	| title = "Graphical iTask Notation"
	, namespace = "http://mbsd.icis.ru.nl/itasks/gin#"
	, description = "A graphical notation for iTask workflows"
	, baseUrl = Just "gin"
	, stencils = [diagramStencil, arcStencil] ++ map declToStencil decls
	, rules =	{ ORYXRules
				| connectionRules = 
					[	{ ORYXConnectionRule
						| role = "arc"
						, connects = 
							[	{ ORYXConnect
								| from_ = "task"
								, to	= ["task", "split", "merge"]
								}
							,	{ ORYXConnect
								| from_ = "split"
								, to	= ["task", "split"]
								}
							,	{ ORYXConnect
								| from_ = "merge"
								, to	= ["task", "split", "merge"]
								}
							]						
						}
					]
				, containmentRules = 
					[	{ ORYXContainmentRule
						| role		= "diagram"
						, contains	= ["all"]
						}
					]
				, morphingRules = 
					[	{ ORYXMorphingRule
						| role			= "arc"
						, baseMorphs	= ["Arc"]
						}
					]
				}
	}
	
diagramStencil :: ORYXStencil
diagramStencil = 
	{ ORYXStencil
	| type			= "node"
	, id			= "Diagram"
	, title			= "Diagram"
	, groups		= []
	, description	= "A Diagram"
	, view			= toString diagramView
	, icon			= "new_diagram.png"
	, mayBeRoot		= True
	, roles			= ["diagram"]
	, properties	= ORYXProperties []
	}
//where

diagramView :: SVGShape
diagramView = 
	{ SVGShape
	| width = 800
	, height = 600
	, defs = []
	, magnets = False
	, elements = 
		[ SVGRect Nothing ((XLeft, YTop),(XRight,YBottom)) 0 0
			[ SVGStroke "black"
			, SVGFill "black"
			, SVGStrokeWidth 1
			, SVGStrokeLineCap "butt"
			, SVGStrokeLineJoin "miter"
			, SVGStrokeMiterLimit 10
			]
		, SVGRect (Just "diagramcanvas") ((XLeft,YTop),(XRight, YBottom)) 0 0
			[ SVGStroke "black"
			, SVGStrokeWidth 2
			, SVGFill "white"
			]
		, SVGText (Just "diagramtext") (X 400, Y 25) "" [SVGStroke "black"]
		]
	}

arcStencil :: ORYXStencil
arcStencil = 
	{ ORYXStencil
	| type			= "edge"
	, id			= "Arc"
	, title			= "Arc"
	, groups		= []
	, description	= ""
	, view			= toString arcView
	, icon			= "new_flow.png"
	, mayBeRoot		= False
	, roles			= ["all", "arc" ]
	, properties	= ORYXProperties []
	}
where
	arcView :: SVGShape
	arcView = 
		{ SVGShape
		| width = 250
		, height = 250
		, defs = 
			[ XMLElem (uname "marker")
				[ XMLAttr (uname "id") "end"
				, XMLAttr (uname "refX") "15"
				, XMLAttr (uname "refY") "5"
				, XMLAttr (uname "markerUnits") "userSpaceOnUse"
				, XMLAttr (uname "markerWidth") "15"
				, XMLAttr (uname "markerHeight") "10"
				, XMLAttr (uname "orient") "auto"
				] 
				[ XMLElem (uname "path")
					[ XMLAttr (uname "d") "M 0 0 L 15 5 L 0 10 L 0 0"
					, XMLAttr (uname "fill") "black"
					, XMLAttr (uname "stroke") "black"
					]
					[]
				]
			]
		, magnets = False
		, elements = 
			[ SVGPath (Just "arc") "M10 50 L210 50"
			 	[ SVGStroke "black"
			 	, SVGFill "none"
			 	, SVGStrokeWidth 2
			 	, SVGStrokeLineCap "round"
			 	, SVGStrokeLineJoin "round"
			 	, SVGMarkerEnd "url(#end)"
			 	]
			]
		}

declToStencil :: GDeclaration -> ORYXStencil
declToStencil gDecl = 
	{ ORYXStencil
	| type			= "node"
	, id			= gDecl.GDeclaration.name
	, title			= gDecl.GDeclaration.name
	, groups		= []
	, description	= gDecl.GDeclaration.name
	, view			= toString (fromMaybe (taskShape gDecl) Nothing)//gDecl.GDeclaration.shape
	, icon			= gDecl.GDeclaration.icon +++ ".png"
	, mayBeRoot		= False
	, roles			= ["all", "task"] //TODO:Split/Merge
	, properties	= ORYXProperties []
	}

taskShape :: GDeclaration -> SVGShape	
taskShape gDecl = 
	{ SVGShape
	| width = 120 
	, height = 20 + 20 * length gDecl.GDeclaration.formalParams
	, defs = []
	, magnets = True
	, elements = 
		[ SVGRect (Just "taskrect") ((XLeft, YTop),(XRight, YBottom)) 5 5 [SVGStroke "black", SVGFill "white"]
		, SVGImage Nothing ((X 2, Y 2), (X 18, Y 18)) (gDecl.GDeclaration.icon +++ ".png")
		, SVGText Nothing (X 20, Y 13) gDecl.GDeclaration.name []
	    , SVGLine Nothing ((XPct 60, Y 20), (XPct 60, YBottom)) []
		] 
		++ flatten (map
			(\(nr,param) -> [ SVGLine Nothing ((XLeft, Y (20 * nr)), (XRight, Y (20 * nr))) []
					   		, SVGText Nothing (X 3, Y (13 + 20 * nr)) param.GFormalParameter.name []
							, SVGText (Just "param.GFormalParameter.name") (X 53, Y (13 + 20 * nr)) "" [] //TODO: Add support for higher order parameters
							]
			) (zip2 [1..] gDecl.GDeclaration.formalParams))
	}
