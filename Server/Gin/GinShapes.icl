implementation module GinShapes

import StdString
import XML

/*
?xml version="1.0" encoding="UTF-8" standalone="no"?>
<svg xmlns="http://www.w3.org/2000/svg" xmlns:svg="http://www.w3.org/2000/svg" xmlns:oryx="http://www.b3mn.org/oryx" xmlns:xlink="http://www.w3.org/1999/xlink" width="121" height="41" version="1.0">
  <defs>
  </defs>
  <oryx:magnets>
    <oryx:magnet oryx:cx="1" oryx:cy="15" oryx:anchors="left" />
    <oryx:magnet oryx:cx="60" oryx:cy="39" oryx:anchors="bottom" />
    <oryx:magnet oryx:cx="119" oryx:cy="19" oryx:anchors="right" />
    <oryx:magnet oryx:cx="60" oryx:cy="1" oryx:anchors="top" />
    <oryx:magnet oryx:cx="60" oryx:cy="19" oryx:default="yes" />
  </oryx:magnets>
  <g pointer-events="fill">
    <rect id="taskrect" x="0" y="0" width="120" height="40" rx="5" ry="5" stroke="black" stroke-width="1" fill="white" />
    <image x="2" y="2" width="16px" height="16px" xlink:href="icons/new_task.png"/>
    <text font-size="12" font-weight="bold" id="text" x="20" y="13" fill="black">showMessage</text>
    <line x1="0" y1="20" x2="120" y2="20" stroke="black"/>
    <line x1="50" y1="20" x2="50" y2="40" stroke="black"/>
    <text font-size="12" id="param1" x="3" y="33" fill="black">message</text>
    <text font-family="Courier New" font-size="12" id="param1" x="53" y="33" fill="black">text</text>
  </g>
</svg>
*/

:: GinShape = 
	{ width			:: Int
	, height		:: Int
	, definitions 	:: [XMLElement]
	}
	
instance toString GinShape
where
	toString { GinShape | width, height, definitions } = toString
		{ XMLDocument
		| defaultNamespace 	= "http://www.w3.org/2000/svg"
		, namespaces	= 	[	{ XMLNamespace | prefix = "svg", uri="http://www.w3.org/2000/svg" }
							,	{ XMLNamespace | prefix = "oryx", uri="http://www.b3mn.org/oryx" }
							,	{ XMLNamespace | prefix = "xlink", uri="http://www.w3.org/1999/xlink" }
							]
		, documentElement = { XMLElement
							| qname = def "svg"
							, attributes = 	[ { XMLAttribute | qname = def "width", value = toString width }
											, { XMLAttribute | qname = def "height", value = toString height }
											]
							, children = []
							}
		}

def :: !String -> XMLQualifiedName
def s = { XMLQualifiedName | prefix = Nothing, name = s }
	
oryx :: !String -> XMLQualifiedName
oryx s = { XMLQualifiedName | prefix = Just "oryx", name = s }
	


/*
<?xml version="1.0" standalone="no"?>
<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN"
"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd">

<svg width="100%" height="100%" version="1.1"
xmlns="http://www.w3.org/2000/svg">

<rect width="300" height="100"
style="fill:rgb(0,0,255);stroke-width:1;
stroke:rgb(0,0,0)"/>

</svg>
*/