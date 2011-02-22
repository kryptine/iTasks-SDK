definition module GinShapes

import XML

:: GinShape = 
	{ width			:: Int
	, height		:: Int
	, definitions 	:: [XMLElement]
	}
	
instance toString GinShape