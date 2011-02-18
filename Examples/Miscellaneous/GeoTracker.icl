implementation module GeoTracker

import iTasks
import GoogleMaps

geoTrackerExamples :: [Workflow]
geoTrackerExamples = 
	[workflow "Examples/Geo tracker/Report position" "Tell us where you are..." reportPosition
	,workflow "Examples/Geo tracker/View map" "Look at the locations of users on the map." viewMap
	]
	
reportPosition :: Task GoogleMapPosition
reportPosition = enterInformation "Where are you now?"

viewMap :: Task GoogleMap
viewMap = updateInformation "Look where everyone is" mkMap <<@ FWFullWidth