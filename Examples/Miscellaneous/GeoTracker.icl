implementation module GeoTracker

import iTasks
import GoogleMaps

geoTrackerExamples :: [Workflow]
geoTrackerExamples = 
	[workflow "Examples/Geo tracker/View map" "Look at the locations of users on the map." viewMap
	,workflow "Examples/Geo tracker/Report position" "Tell us where you are..." reportPosition
	]

locationStore :: SymmetricShared [(User,GoogleMapPosition)]
locationStore = sharedStore "Locations" []
	
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
	
viewMap :: Task Void
viewMap
	=	createSharedStore nlMap	//Create a new map (local to this task) to put the markers on
	>>= \map ->
		updateSharedInformationA "Look where everyone is" (toView,fromView) [(ActionQuit,alwaysShared)] (map >&< locationStore) <<@ fullWidthInteractionLayout
	>>|	stop
where
	nlMap :: GoogleMap		
	nlMap = {GoogleMap| mkMap & zoom = 7, center = {lat = 52.396, lng = 5.21}}
	
	toView :: (GoogleMap, [(User,GoogleMapPosition)]) -> GoogleMap
	toView (gmap,locations) = {GoogleMap|gmap & markers = map mkMarker locations}
	
	fromView :: GoogleMap (GoogleMap,[(User,GoogleMapPosition)]) -> (GoogleMap,[(User,GoogleMapPosition)])
	fromView view (gmap,positions)	= ({GoogleMap|view & markers = []},positions)
		
	mkMarker :: (User,GoogleMapPosition) -> GoogleMapMarker
	mkMarker (user,position)
		=	{ GoogleMapMarker
			| position		= position
			, title			= Just (displayName user)
			, infoWindow	= Just {GoogleMapInfoWindow|content = toString info}
			, draggable		= False
			}
	where
		info = SpanTag []
			[H1Tag [] [Text (displayName user)],BrTag []
			,Text "Lat:", Text (toString position.lat), BrTag []
			,Text "Lng: ", Text (toString position.lng), BrTag []
			]
