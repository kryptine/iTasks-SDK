implementation module GoogleMaps

import HTML, StdEnv, JSON_NG, GenUpdate, GenVisualize, GenVerify

derive JSONEncode TUIGoogleMap, TUIGoogleMapOptions
derive JSONDecode MVCUpdate, ClickUpdate, ClickSource, ClickEvent, MarkerDragUpdate

derive gVisualizeText  	GoogleMapSettings, GoogleMapPerspective, GoogleMapPosition, GoogleMapMarker, GoogleMapInfoWindow, GoogleMapType
derive gVisualizeEditor	GoogleMapSettings, GoogleMapPerspective, GoogleMapPosition, GoogleMapMarker, GoogleMapInfoWindow, GoogleMapType
derive gHeaders			GoogleMap, GoogleMapSettings, GoogleMapPerspective, GoogleMapPosition, GoogleMapMarker, GoogleMapInfoWindow, GoogleMapType
derive gGridRows		GoogleMap, GoogleMapSettings, GoogleMapPerspective, GoogleMapPosition, GoogleMapMarker, GoogleMapInfoWindow, GoogleMapType
derive gUpdate	  		GoogleMapSettings, GoogleMapPerspective, GoogleMapPosition, GoogleMapMarker, GoogleMapInfoWindow, GoogleMapType
derive gDefaultMask		GoogleMapSettings, GoogleMapPerspective, GoogleMapPosition, GoogleMapMarker, GoogleMapInfoWindow, GoogleMapType
derive gVerify			GoogleMapSettings, GoogleMapPerspective, GoogleMapPosition, GoogleMapMarker, GoogleMapInfoWindow, GoogleMapType

derive JSONEncode		GoogleMap, GoogleMapPerspective, GoogleMapSettings, GoogleMapMarker, GoogleMapInfoWindow, GoogleMapType
derive JSONDecode		GoogleMap, GoogleMapPerspective, GoogleMapSettings, GoogleMapMarker, GoogleMapInfoWindow, GoogleMapType
derive gEq				GoogleMap, GoogleMapPerspective, GoogleMapSettings, GoogleMapPosition, GoogleMapMarker, GoogleMapInfoWindow, GoogleMapType

JSONEncode{|GoogleMapPosition|} {lat,lng}										= [JSONArray [JSONReal lat,JSONReal lng]]
JSONDecode{|GoogleMapPosition|} [JSONArray [JSONReal lat,JSONReal lng]:rest]	= (Just {lat=lat,lng=lng},rest)
JSONDecode{|GoogleMapPosition|} [JSONArray [JSONInt lat,JSONReal lng]:rest]		= (Just {lat=toReal lat,lng=lng},rest)
JSONDecode{|GoogleMapPosition|} [JSONArray [JSONReal lat,JSONInt lng]:rest]		= (Just {lat=lat,lng=toReal lng},rest)
JSONDecode{|GoogleMapPosition|} [JSONArray [JSONInt lat,JSONInt lng]:rest]		= (Just {lat=toReal lat,lng=toReal lng},rest)
JSONDecode{|GoogleMapPosition|} rest											= (Nothing,rest)

:: MVCUpdate = 
	{ center			:: !GoogleMapPosition
	, zoom				:: !Int
	, type				:: !GoogleMapType
	}	
	
:: ClickUpdate = 
	{ event				:: !ClickEvent
	, source			:: !ClickSource
	, point				:: !GoogleMapPosition
	}

:: ClickEvent	= LEFTCLICK | RIGHTCLICK | DBLCLICK
:: ClickSource  = MAP | MARKER GoogleMapPosition

:: MarkerDragUpdate = 
	{ index				:: !Int
	, point				:: !GoogleMapPosition
	}

:: TUIGoogleMap = 
	{ center 			:: GoogleMapPosition
	, mapType			:: GoogleMapType
	, markers			:: [GoogleMapMarker]
	, xtype				:: String
	, taskId			:: Maybe String
	, name				:: String
	, editor			:: Bool
	, options			:: TUIGoogleMapOptions
	}
	
:: TUIGoogleMapOptions =
	{ mapTypeControl 	:: Bool
	, panControl		:: Bool
	, streetViewControl	:: Bool
	, zoomControl		:: Bool
	, scaleControl		:: Bool
	, scrollwheel		:: Bool
	, draggable			:: Bool
	, zoom				:: Int
	}

instance toString GoogleMapType
where 
	toString ROADMAP 	= "ROADMAP"
	toString SATELLITE 	= "SATELLITE"
	toString HYBRID 	= "HYBRID"
	toString TERRAIN 	= "TERRAIN"

gVisualizeText{|GoogleMap|} _ _ = ["<Google map>"]
gVisualizeEditor{|GoogleMap|} mbMap vst = visualizeCustom mkControl vst
where
	mkControl name _ _ vst=:{VSt|taskId}
		= ([defaultDef (TUICustom ((mapPanel mbMap name True)))], vst)
	where		
		mapPanel Nothing	name ed = toJSON (tuidef defaultMap name ed)
		mapPanel (Just map)	name ed = toJSON (tuidef map   name ed)
	
		tuidef map name ed =
			{ TUIGoogleMap
			| center = map.perspective.GoogleMapPerspective.center
			, mapType = map.perspective.GoogleMapPerspective.type
			, markers = map.GoogleMap.markers
			, xtype = "itasks-googlemap"
			, name = name
			, taskId = fmap toString taskId
			, editor = ed
			, options =
				{ TUIGoogleMapOptions
				| mapTypeControl = map.settings.GoogleMapSettings.mapTypeControl
				, panControl = map.settings.GoogleMapSettings.panControl
				, streetViewControl = map.settings.GoogleMapSettings.streetViewControl
				, zoomControl = map.settings.GoogleMapSettings.zoomControl
				, scaleControl = map.settings.GoogleMapSettings.scaleControl
				, scrollwheel = map.settings.GoogleMapSettings.scrollwheel
				, draggable = map.settings.GoogleMapSettings.draggable
				, zoom = map.perspective.GoogleMapPerspective.zoom
				}
			}

gUpdate{|GoogleMap|} mode ust = basicUpdate mode parseUpdate defaultMap ust
where
	parseUpdate json orig
		# mbMVC		= fromJSON json
		| isJust mbMVC
			# mvc = fromJust mbMVC
			= {GoogleMap | orig & perspective = {GoogleMapPerspective|orig.perspective & center = mvc.MVCUpdate.center, zoom = mvc.MVCUpdate.zoom, type = mvc.MVCUpdate.type}}
		# mbClick 	= fromJSON json
		| isJust mbClick
			# click = fromJust mbClick
			# marker = {GoogleMapMarker | position = click.ClickUpdate.point, title = Nothing, icon = Nothing, infoWindow = Nothing, draggable = True, selected = False} 
			= {GoogleMap | orig & markers = orig.GoogleMap.markers ++ [marker]}
		# mbMarkerDrag = fromJSON json
		| isJust mbMarkerDrag
			# {MarkerDragUpdate|index,point}	= fromJust mbMarkerDrag
			= {GoogleMap | orig & markers = [if (i == index) {GoogleMapMarker|m & position = point} m \\ m <- orig.GoogleMap.markers & i <- [0..]]}
		
		| otherwise = orig

gDefaultMask{|GoogleMap|} _ = [Touched []]

gVerify{|GoogleMap|} _ vst = alwaysValid vst //Maps are always valid

// -- Utility Functions --

defaultMapPerspective :: GoogleMapPerspective
defaultMapPerspective =
	{ GoogleMapPerspective
	| type				= ROADMAP
	, center 			= {GoogleMapPosition|lat = 51.82, lng = 5.86}
	, zoom				= 10
	}
	
defaultMapSettings :: GoogleMapSettings
defaultMapSettings =
	{ GoogleMapSettings
	| mapTypeControl	= True
	, panControl		= True
	, streetViewControl	= True
	, zoomControl		= True
	, scaleControl		= True
	, scrollwheel		= True
	, draggable			= True
	}
	
defaultMap :: GoogleMap
defaultMap = {GoogleMap|perspective = defaultMapPerspective, settings = defaultMapSettings, markers = []}

minimalMapSettings :: GoogleMapSettings
minimalMapSettings =
	{ GoogleMapSettings
	| mapTypeControl	= False
	, panControl		= False
	, streetViewControl	= False
	, zoomControl		= False
	, scaleControl		= False
	, scrollwheel		= False
	, draggable			= False
	}
minimalMap :: GoogleMap
minimalMap = {GoogleMap|perspective = defaultMapPerspective, settings = minimalMapSettings, markers = []}
	