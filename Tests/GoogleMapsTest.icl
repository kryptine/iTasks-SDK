module GoogleMapsTest

import iTasks
import GeoDomain
	
mkMap :: Map
mkMap = { Map
		| center 			= (20.2,5.0)
		, width 			= 500
		, height 			= 400
		, mapTypeControl	= True
		, navigationControl = True
		, scaleControl		= True
		, zoom				= 10
		, mapType			= HYBRID
		, markers			= []
		}

mapAction :: Task (Map)
mapAction = enterInformation "Do something with the map" //-&&- enterInformation " and leave a message"
	
mapsTask :: Task Void
mapsTask = mapAction >>| return Void
	
Start :: *World -> *World
Start world = startEngine [workflow "Google Maps Test" mapsTask ] world