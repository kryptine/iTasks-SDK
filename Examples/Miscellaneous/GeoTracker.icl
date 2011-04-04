implementation module GeoTracker

import iTasks
import GoogleMaps

geoTrackerExamples :: [Workflow]
geoTrackerExamples = 
	[workflow "Examples/Geo tracker/Report position" "Tell us where you are..." reportPosition
	,workflow "Examples/Geo tracker/View map" "Look at the locations of users on the map." viewMap
	]
	
locationStore :: SymmetricShared [(User,GoogleMapPosition)]
locationStore = sharedStoreDefault "Locations"
	
reportPosition :: GoogleMapPosition -> Task Void
reportPosition position
	=	getCurrentUser
	>>= \user ->
		updateShared (update (user,position)) locationStore
	>>| stop
where
	update (user,position) [] = [(user,position)]
	update (user,position) [(u,p):ps]
		| u == user	= [(user,position):ps]
					= [(u,p):update (user,position) ps]
	
viewMap :: Task [(User,GoogleMapPosition)]
viewMap
	= showMessageSharedA "Look where everyone is" mapView [(ActionQuit,always)] locationStore >>= transform snd
where
	mapView :: [(User,GoogleMapPosition)] -> GoogleMap
	mapView locations = {GoogleMap| nlMap & mapType = ROADMAP, markers = map mkMarker locations}
	
	mkMarker :: (User,GoogleMapPosition) -> GoogleMapMarker
	mkMarker (user,position)
		=	{ GoogleMapMarker
			| position		= position
			, title			= Just (displayName user)
			, infoWindow	= Just {GoogleMapInfoWindow|content = "<h1>" +++ displayName user +++ "</h1>" +++ visualizeAsTextLabel position}
			, draggable		= False
			}
			
	nlMap :: GoogleMap		
	nlMap = {GoogleMap| mkMap & zoom = 7, center = {lat = 52.396, lng = 5.21}}