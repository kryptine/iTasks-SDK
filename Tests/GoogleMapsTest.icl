module GoogleMapsTest

import iTasks
import GeoDomain
	
mapAction :: Task (Map)
mapAction = enterInformation "Do something with the map" //-&&- enterInformation " and leave a message"
	
mapsTask :: Task Void
mapsTask = mapAction >>= \map -> showMessageAbout "Review Your Result" (convertToStaticMap map) >>| return Void
	
Start :: *World -> *World
Start world = startEngine [workflow "Google Maps Test" mapsTask ] world