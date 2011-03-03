definition module GinSVG

import Maybe
import XML

:: SVGPosX	= XLeft
			| XRight
			| X !Int
			| XPct !Int

:: SVGPosY	= YTop
			| YBottom
			| Y !Int
			| YPct !Int

:: SVGCoordinate :== (!SVGPosX, !SVGPosY)

:: SVGBounds :== (!SVGCoordinate, !SVGCoordinate)

:: SVGElement	= SVGRect		SVGId !SVGBounds !Int !Int ![SVGStyle]
				| SVGEllipse	SVGId !SVGBounds ![SVGStyle]
				| SVGLine		SVGId !SVGBounds ![SVGStyle]
				| SVGPolygon	SVGId [SVGCoordinate] ![SVGStyle]
				| SVGPath		SVGId !String ![SVGStyle]
				| SVGText 		SVGId !SVGCoordinate !String ![SVGStyle]
				| SVGImage		SVGId !SVGBounds !String
				| SVGGroup		SVGId [SVGElement]
				
:: SVGStyle = SVGStroke !String
			| SVGFill !String
			| SVGStrokeWidth Int
			| SVGStrokeLineCap !String
			| SVGStrokeLineJoin !String
			| SVGStrokeMiterLimit !Int
			| SVGMarkerEnd !String
			| SVGEdgePosition !String

:: SVGShape = 
	{ width		:: !Int
	, height	:: !Int
	, defs		:: ![XMLNode]
	, magnets	:: !Bool
	, elements	:: ![SVGElement]
	}
	
:: SVGId :== Maybe String


instance toString SVGShape
