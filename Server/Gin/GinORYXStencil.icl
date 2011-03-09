implementation module GinORYXStencil

import StdEnum
import StdList
import GenEq

import JSON
import Text

import GinSyntax
import GinORYX
import GinSVG
import GinFlowLibrary

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

makeStencilSet :: GModule -> ORYXStencilSet
makeStencilSet gMod
	# decls = [ (gMod.GModule.name,bt,decl) \\ (bt,decl) <- getModuleDeclarations gMod]
	= 	{ ORYXStencilSet
		| title 		= "Graphical iTask Notation"
		, namespace		= "http://mbsd.icis.ru.nl/itasks/gin/" +++ gMod.GModule.name +++ "#"
		, description	= "A graphical notation for iTask workflows"
		, baseUrl		= Just "gin"
		, extends		= Just "http://mbsd.icis.ru.nl/itasks/gin#"
		, stencils		= map declToStencil decls
		, rules			=	{ ORYXRules 
							| connectionRules = []
							, containmentRules = []
							, morphingRules = []
							}
	}		
		
predefinedStencilSet :: ORYXStencilSet
predefinedStencilSet
	=	{ ORYXStencilSet
		| title = "Graphical iTask Notation"
		, namespace = "http://mbsd.icis.ru.nl/itasks/gin#"
		, description = "A graphical notation for iTask workflows"
		, baseUrl = Just "gin"
		, extends = Nothing
		, stencils = [diagramStencil, arcStencil]// ++ map declToStencil decls
		, rules =	{ ORYXRules
					| connectionRules = 
						[	{ ORYXConnectionRule
							| role = "arc"
							, connects = 
								[	{ ORYXConnect
									| from_ = "single"
									, to	= ["single", "split", "merge"]
									}
								,	{ ORYXConnect
									| from_ = "split"
									, to	= ["single", "split"]
									}
								,	{ ORYXConnect
									| from_ = "merge"
									, to	= ["single", "split", "merge"]
									}
								]						
							}
						]
					, containmentRules = 
						[	{ ORYXContainmentRule
							| role		= "diagram"
							, contains	= ["all"]
							}
						,	{ ORYXContainmentRule
							| role		= "higherOrderTask"
							, contains	= ["all"]
							}
						]
					, morphingRules = 
						[	{ ORYXMorphingRule
							| role			= "single"
							, baseMorphs	= ["single"]
							} 
						,	{ ORYXMorphingRule
							| role			= "split"
							, baseMorphs	= ["split"]
							} 
						,	{ ORYXMorphingRule
							| role			= "merge"
							, baseMorphs	= ["merge"]
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
	, properties	= []
	}

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
	, properties	=  
		[ ORYXProperties
			[ { ORYXProperty| key = "id"			, value = JSONString "pattern"}
			, { ORYXProperty| key = "type"			, value = JSONString "String"}
			, { ORYXProperty| key = "title"			, value = JSONString "Pattern"}
			, { ORYXProperty| key = "value"			, value = JSONString ""}
			, { ORYXProperty| key = "description"	, value = JSONString ""}
			, { ORYXProperty| key = "tooltip"		, value = JSONString ""}
			, { ORYXProperty| key = "readonly"		, value = JSONBool False}
			, { ORYXProperty| key = "optional"		, value = JSONBool True}
			, { ORYXProperty| key = "refToView"		, value = JSONString "pattern"}
			, { ORYXProperty| key = "length"		, value = JSONString ""}
			, { ORYXProperty| key = "wrapLines"		, value = JSONBool False}
			]
		]
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
			,	 SVGText (Just "pattern") (X 47, Y 7) "" [SVGEdgePosition "midtop"]
			]
		}

declToStencil :: (!String, !BranchType, GDeclaration) -> ORYXStencil
declToStencil (group,branchtype,gDecl) 
	# morphrole = case branchtype of
		BTSingle	= "single"
		BTSplit		= "split"
		BTMerge		= "merge"
	=	{ ORYXStencil
		| type			= "node"
		, id			= gDecl.GDeclaration.name
		, title			= gDecl.GDeclaration.name
		, groups		= [group]
		, description	= gDecl.GDeclaration.name
		, view			= toString (fromMaybe (taskShape gDecl) Nothing)//gDecl.GDeclaration.shape
		, icon			= gDecl.GDeclaration.icon +++ ".png"
		, mayBeRoot		= False
		, roles			= ["all", morphrole] ++ if (isHigherOrder gDecl) ["higherOrderTask"] []
		, properties	= map formalParameterToProperty gDecl.GDeclaration.formalParams
		}

formalParameterToProperty :: GFormalParameter -> ORYXProperties
formalParameterToProperty param = ORYXProperties
	[ { ORYXProperty| key = "id"			, value = JSONString param.GFormalParameter.name}
	, { ORYXProperty| key = "type"			, value = JSONString "String"}
	, { ORYXProperty| key = "title"			, value = JSONString param.GFormalParameter.name}
	, { ORYXProperty| key = "value"			, value = JSONString ""}
	, { ORYXProperty| key = "description"	, value = JSONString ""}
	, { ORYXProperty| key = "tooltip"		, value = JSONString ""}
	, { ORYXProperty| key = "readonly"		, value = JSONBool False}
	, { ORYXProperty| key = "optional"		, value = JSONBool True}
	, { ORYXProperty| key = "refToView"		, value = JSONString param.GFormalParameter.name}
	, { ORYXProperty| key = "length"		, value = JSONString ""}
	, { ORYXProperty| key = "wrapLines"		, value = JSONBool True}
	]

isHigherOrder :: GDeclaration -> Bool
isHigherOrder decl = any higherOrderParam decl.GDeclaration.formalParams 

higherOrderParam :: GFormalParameter -> Bool
higherOrderParam param = case param.GFormalParameter.type of
	(GTypeApplication (GConstructor "Task")  _)	= True
	_											= False

taskShape :: GDeclaration -> SVGShape	
taskShape gDecl = 
	{ SVGShape
	| width = if (isEmpty gDecl.GDeclaration.formalParams) 140 300
	, height = 20 + 20 * length gDecl.GDeclaration.formalParams
	, defs = []
	, magnets = True
	, elements = 
		[ SVGRect (Just "taskrect") ((XLeft, YTop),(XRight, YBottom)) 5 5 [SVGStroke "black", SVGFill "white", SVGResize "horizontal vertical"]
		, SVGImage Nothing ((X 2, Y 2), (X 18, Y 18)) (gDecl.GDeclaration.icon +++ ".png") [SVGAnchors "top left"]
		, SVGText Nothing (X 20, Y 13) gDecl.GDeclaration.name [SVGAnchors "top left"]
	    , SVGLine Nothing ((X 80, Y 20), (X 80, YBottom)) [SVGAnchors "top left bottom"]
		] 
		++ flatten (map
			(\(nr,param) -> [ SVGLine Nothing ((XLeft, Y (20 * nr)), (XRight, Y (20 * nr))) [SVGAnchors "left right"]
					   		, SVGText Nothing (X 3, Y (13 + 20 * nr)) param.GFormalParameter.name []
							, SVGText (Just param.GFormalParameter.name) (X 83, Y (13 + 20 * nr)) "" [SVGAnchors "left"]
							]
			) (zip2 [1..] gDecl.GDeclaration.formalParams))
	}



