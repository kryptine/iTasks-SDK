definition module iTasks.Gin.SVG

//import Maybe
//import XML, HTML, UIDefinition

from Maybe import :: Maybe
from XML import :: XMLNode
from iTasks import class iTask

from iTasks.Framework.GenVisualize import generic gVisualizeText, generic gVisualizeEditor
from iTasks.Framework.GenUpdate import generic gUpdate, generic gDefault, generic gHeaders, generic gGridRows
from iTasks.Framework.GenVerify import generic gVerify
from JSON import generic JSONEncode, generic JSONDecode, :: JSONNode
from GenEq import generic gEq
from iTasks.API.Core.SystemTypes import :: InteractionMask, :: VerifyMask, :: VerifyOptions, :: ConsPos, :: StaticVisualizationMode, :: VSt, :: VisualizationResult

:: SVGPosX	= XLeft
			| XRight
			| XAbs !Int
			| XPct !Int

:: SVGPosY	= YTop
			| YBottom
			| YAbs !Int
			| YPct !Int

:: SVGCoordinate :== (!SVGPosX, !SVGPosY)

:: SVGBounds :== (!SVGCoordinate, !SVGCoordinate)

:: SVGElement	= SVGRect		SVGId !SVGBounds !Int !Int ![SVGStyle]
				| SVGEllipse	SVGId !SVGBounds ![SVGStyle]
				| SVGLine		SVGId !SVGBounds ![SVGStyle]
				| SVGPolygon	SVGId [SVGCoordinate] ![SVGStyle]
				| SVGPath		SVGId !String ![SVGStyle]
				| SVGText 		SVGId !SVGCoordinate !String ![SVGStyle]
				| SVGImage		SVGId !SVGBounds !String ![SVGStyle]
				| SVGGroup		SVGId [SVGElement]
				
:: SVGStyle = 
			//Basic SVG
			  SVGStroke !String
			| SVGFill !String
			| SVGStrokeWidth Int
			| SVGStrokeDashArray !String
			| SVGStrokeLineCap !String
			| SVGStrokeLineJoin !String
			| SVGStrokeMiterLimit !Int
			| SVGMarkerEnd !String
			| SVGFontWeight !String
			// ORYX extensions
			| SVGAlign !String
			| SVGAnchors !String
			| SVGEdgePosition !String
			| SVGResize !String

:: SVGShape = 
	{ width		:: !Int
	, height	:: !Int
	, defs		:: ![XMLNode]
	, magnets	:: !Bool
	, elements	:: ![SVGElement]
	}
	
:: SVGId :== Maybe String

derive class iTask SVGShape

instance toString SVGShape
