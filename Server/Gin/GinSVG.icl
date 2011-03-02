implementation module GinSVG

import StdInt
import StdList
import StdString

import Maybe
import XML

instance toString SVGShape
where
	toString {width, height, defs, magnets, elements} = toString doc
	where
		doc :: XMLDoc
		doc = XMLDoc 
			"http://www.w3.org/2000/svg"
			[ ("svg","http://www.w3.org/2000/svg")
			, ("oryx","http://www.b3mn.org/oryx")
			, ("xlink", "http://www.w3.org/1999/xlink")
			]
			(XMLElem (uname "svg") 
				[ XMLAttr (uname "width")  (toString width)
				, XMLAttr (uname "height") (toString height)
				] 
				([defs`] ++ magnets` ++ [group]))
	
		defs` :: XMLNode
		defs` = XMLElem (uname "defs") [] defs
			
		magnets` :: [XMLNode]
		magnets` = if (not magnets) []
			[ XMLElem (qname "oryx" "magnets") [] 
				[ magnet 1 (height / 2) "anchor" "left"
				, magnet (width / 2) height "anchor" "bottom"
				, magnet width (height / 2) "anchor" "right"
				, magnet (width / 2) 1 "anchor" "top"
				, magnet (width / 2) (height / 2) "default" "yes"
				]
			]

		magnet :: Int Int String String -> XMLNode
		magnet cx cy type value = XMLElem (qname "oryx" "magnet")
			[ XMLAttr (qname "oryx" "cx") (toString cx)
			, XMLAttr (qname "oryx" "cy" ) (toString cy)
			, XMLAttr (qname "oryx" type) value
			]
			[]
		
		group :: XMLNode
		group = XMLElem (uname "g") [XMLAttr (uname "pointer-events") "fill"]
			(map (elementToXMLNode width height) elements)

elementToXMLNode :: Int Int SVGElement -> XMLNode
elementToXMLNode width height node = case node of
	(SVGRect sid bounds rx ry styles) = 
		XMLElem (uname "rect")
			( getID sid 
			  ++ getBoundsWidth bounds
			  ++ [ XMLAttr (uname "rx") (toString ry)
				 , XMLAttr (uname "ry") (toString ry) 
				 ]
			  ++ map styleToXMLAttr styles
			) []
	(SVGEllipse sid ((x1,y1),(x2,y2)) styles) =
		XMLElem (uname "ellipse")
			( getID sid ++ 
			  [ XMLAttr (uname "cx") (toString ((getX x1 + getX x2) / 2))
			  , XMLAttr (uname "cy") (toString ((getY y1 + getY y2) / 2))
			  , XMLAttr (uname "rx") (toString ((getX x2 - getX x1) / 2))
			  , XMLAttr (uname "ry") (toString ((getY y2 - getY y1) / 2))
			  ]
			  ++ map styleToXMLAttr styles
			) []
	(SVGLine sid ((x1,y1),(x2,y2)) styles) =
		XMLElem (uname "line")
			( getID sid ++ 
				[ XMLAttr (uname "x1") (toString (getX x1))
				, XMLAttr (uname "y1") (toString (getY y1))
				, XMLAttr (uname "x2") (toString (getX x2))
				, XMLAttr (uname "y2") (toString (getY y2))
				]
			    ++ map styleToXMLAttr styles
			) []
	(SVGPolygon sid points styles) =
		XMLElem (uname "polygon")
			( getID sid ++
			  [ XMLAttr (uname "points") 
			            (foldr (+++) "" (map (\(x,y) -> toString (getX x) +++ "," +++ toString (getY y) +++ " ") points))
			  ]
			  ++ map styleToXMLAttr styles
			) []
	(SVGPath sid d styles) =
		XMLElem (uname "path")
			( getID sid
			  ++ [ XMLAttr (uname "d") d ]
			  ++ map styleToXMLAttr styles
			) []
	(SVGText sid (x,y) text styles) = 
		XMLElem (uname "text")
			( getID sid ++ 
				[ XMLAttr (uname "x") (toString (getX x))
				, XMLAttr (uname "y") (toString (getY y))
			 	]
 			    ++ map styleToXMLAttr styles
			) [XMLText text]
	(SVGImage sid bounds image) = 
		XMLElem (uname "image")
			( getID sid 
			  ++ getBoundsWidth bounds
			  ++ [ XMLAttr (qname "xlink" "href") image ]
			) []
	(SVGGroup sid elements) = 
		XMLElem (uname "g") (getID sid) (map (elementToXMLNode width height) elements)
where
	getID :: SVGId -> [XMLAttr]
	getID Nothing    = []
	getID (Just sid) = [XMLAttr (uname "id") sid]
	
	getBoundsWidth :: SVGBounds -> [XMLAttr]
	getBoundsWidth  ((x1,y1),(x2,y2)) =
		[ XMLAttr (uname "x") (toString (getX x1))
		, XMLAttr (uname "y") (toString (getY y1))
		, XMLAttr (uname "width") (toString (getX x2 - getX x1))
		, XMLAttr (uname "height") (toString (getY y2 - getY y1))
		]
	
	getX :: SVGPosX -> Int
	getX XLeft = 0
	getX XRight = width
	getX (X x) = x
	getX (XPct p) = (p * width / 100)
	
	getY :: SVGPosY -> Int
	getY YTop = 0
	getY YBottom = height
	getY (Y y) = y
	getY (YPct p) = (p * height / 100)
	
styleToXMLAttr :: SVGStyle -> XMLAttr
styleToXMLAttr (SVGStroke s)			= XMLAttr (uname "stroke") s
styleToXMLAttr (SVGFill s)				= XMLAttr (uname "fill") s
styleToXMLAttr (SVGStrokeWidth w)		= XMLAttr (uname "stroke-width") (toString w)
styleToXMLAttr (SVGStrokeLineCap s)		= XMLAttr (uname "stroke-linecap") s
styleToXMLAttr (SVGStrokeLineJoin s)	= XMLAttr (uname "stroke-linejoin") s
styleToXMLAttr (SVGStrokeMiterLimit l)	= XMLAttr (uname "stroke-miterlimit") (toString l)
styleToXMLAttr (SVGMarkerEnd m)			= XMLAttr (uname "marker-end") m
