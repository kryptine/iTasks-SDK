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
	=	get currentUser
	>>= \user ->
		update (updatePos (user,position)) locationStore
	>>| stop
where
	updatePos (user,position) [] = [(user,position)]
	updatePos (user,position) [(u,p):ps]
		| u == user	= [(user,position):ps]
					= [(u,p):updatePos (user,position) ps]
	
viewMap :: Task Void
viewMap = interact
			"Look where everyone is"
			(\gmap locations _ -> [UpdateView (FormValue {GoogleMap|gmap & markers = map mkMarker locations},\mbMap -> ({GoogleMap|fromMaybe gmap mbMap & markers = []},Nothing))])
			(\_ _ _ -> UserActions [(ActionQuit,Just Void)])
			nlMap
			locationStore
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
