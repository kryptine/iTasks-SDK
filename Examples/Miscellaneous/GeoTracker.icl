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
viewMap = updateInformation "Look where everyone is" initMap <<@ FWFullWidth
where
	initMap = {GoogleMap| mkMap & mapType = TERRAIN, markers = [uni]}
	
	uni :: GoogleMapMarker
	uni = 	{GoogleMapMarker
			| position		= {lat = 51.82, lng = 5.86}
			, title			= Just "Radboud University"
			, infoWindow	= Just {GoogleMapInfoWindow|content = "Radboud University"}
			, draggable		= False
			}