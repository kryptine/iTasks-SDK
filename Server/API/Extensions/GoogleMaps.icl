implementation module GoogleMaps

import HTML, StdEnv, JSON, GenUpdate, GenVisualize, GenVerify

derive JSONEncode TUIGoogleMap, TUIGoogleMapOptions, TUIGoogleStaticMap
derive JSONDecode MVCUpdate, ClickUpdate, ClickSource, ClickEvent, MarkerDragUpdate

derive gVisualizeText  	GoogleMapPosition, GoogleMapMarker, GoogleMapInfoWindow, GoogleMapType
derive gVisualizeHtml  	GoogleMapPosition, GoogleMapMarker, GoogleMapInfoWindow, GoogleMapType
derive gVisualizeEditor	GoogleMapPosition, GoogleMapMarker, GoogleMapInfoWindow, GoogleMapType
derive gUpdate	  		GoogleMapPosition, GoogleMapMarker, GoogleMapInfoWindow, GoogleMapType, GoogleStaticMap
derive gDefaultMask		GoogleMapPosition, GoogleMapMarker, GoogleMapInfoWindow, GoogleMapType, GoogleStaticMap
derive gVerify			GoogleMapPosition, GoogleMapMarker, GoogleMapInfoWindow, GoogleMapType, GoogleStaticMap

derive JSONEncode		GoogleMap, GoogleMapMarker, GoogleMapInfoWindow, GoogleMapType, GoogleStaticMap
derive JSONDecode		GoogleMap, GoogleMapMarker, GoogleMapInfoWindow, GoogleMapType, GoogleStaticMap
derive gEq				GoogleMap, GoogleMapPosition, GoogleMapMarker, GoogleMapInfoWindow, GoogleMapType, GoogleStaticMap

JSONEncode{|GoogleMapPosition|} {lat,lng}										= [JSONArray [JSONReal lat,JSONReal lng]]
JSONDecode{|GoogleMapPosition|} [JSONArray [JSONReal lat,JSONReal lng]:rest]	= (Just {lat=lat,lng=lng},rest)
JSONDecode{|GoogleMapPosition|} [JSONArray [JSONInt lat,JSONReal lng]:rest]		= (Just {lat=toReal lat,lng=lng},rest)
JSONDecode{|GoogleMapPosition|} [JSONArray [JSONReal lat,JSONInt lng]:rest]		= (Just {lat=lat,lng=toReal lng},rest)
JSONDecode{|GoogleMapPosition|} [JSONArray [JSONInt lat,JSONInt lng]:rest]		= (Just {lat=toReal lat,lng=toReal lng},rest)
JSONDecode{|GoogleMapPosition|} rest											= (Nothing,rest)

derive bimap	Maybe, (,)

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
	, taskId			:: String
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
	
::TUIGoogleStaticMap =
	{ width				:: Int
	, height			:: Int
	, xtype				:: String
	, url				:: String
	}

instance toString GoogleMapType
where 
	toString ROADMAP 	= "ROADMAP"
	toString SATELLITE 	= "SATELLITE"
	toString HYBRID 	= "HYBRID"
	toString TERRAIN 	= "TERRAIN"

gVisualizeText{|GoogleMap|} _ _ = ["Map"]
gVisualizeHtml{|GoogleMap|} mode map = case mode of
	AsLabel		= [Text "Map"]
	AsDisplay	= [staticMap (convertToStaticMap map)]
gVisualizeEditor{|GoogleMap|} mbMap vst = visualizeCustom mkControl mbMap vst
where
	mkControl name mbMap _ _ static vst=:{VSt|taskId}
		| static
			//TODO
			= ([], vst)
			//= ([htmlDisplay (staticMap (convertToStaticMap (fromMaybe defaultMap mbMap)))], vst)
		| otherwise
			= ([defaultDef (TUICustom ((mapPanel mbMap name True)))], vst)
	where		
		mapPanel Nothing	name ed = toJSON (tuidef defaultMap name ed)
		mapPanel (Just map)	name ed = toJSON (tuidef map   name ed)
	
		tuidef map name ed =
			{ TUIGoogleMap
			| center = map.GoogleMap.center
			, mapType = map.GoogleMap.mapType
			, markers = map.GoogleMap.markers
			, xtype = "igooglemap"
			, name = name
			, taskId = fromMaybe "" taskId
			, editor = ed
			, options =
				{ TUIGoogleMapOptions
				| mapTypeControl = map.GoogleMap.mapTypeControl
				, panControl = map.GoogleMap.panControl
				, streetViewControl = map.GoogleMap.streetViewControl
				, zoomControl = map.GoogleMap.zoomControl
				, scaleControl = map.GoogleMap.scaleControl
				, scrollwheel = map.GoogleMap.scrollwheel
				, draggable = map.GoogleMap.draggable
				, zoom = map.GoogleMap.zoom
				}
			}

gVisualizeText{|GoogleStaticMap|} mode (GoogleStaticMap _ _ u) = case mode of
	AsLabel		= ["Static Map"]
	AsDisplay	= ["Static Map: " +++ u]
gVisualizeHtml{|GoogleStaticMap|} mode map = case mode of
	AsLabel		= [Text "Static Map"]
	AsDisplay	= [staticMap map]
gVisualizeEditor{|GoogleStaticMap|} mbMap vst = visualizeCustom mkControl mbMap vst
where
	mkControl _ mbMap _ _ _ vst = case mbMap of
		Just (GoogleStaticMap w h u)	= ([defaultDef (TUICustom (toJSON (staticMap w h u)))], vst)
		_								= ([], vst)

	staticMap w h u =
		{ TUIGoogleStaticMap
		| width 	= w
		, height 	= h	
		, xtype		= "igooglestaticmap"
		, url		= u
		}

staticMap (GoogleStaticMap w h u) = ImgTag [SrcAttr u, WidthAttr (toString w), HeightAttr (toString h)]

gUpdate{|GoogleMap|} mode ust = basicUpdate mode parseUpdate defaultMap ust
where
	parseUpdate json orig
		# mbMVC		= fromJSON json
		| isJust mbMVC
			# mvc = fromJust mbMVC
			= {GoogleMap | orig & center = mvc.MVCUpdate.center, zoom = mvc.MVCUpdate.zoom, mapType = mvc.MVCUpdate.type}
		# mbClick 	= fromJSON json
		| isJust mbClick
			# click = fromJust mbClick
			# marker = {GoogleMapMarker | position = click.ClickUpdate.point, title = Nothing, infoWindow = Nothing, draggable = True} 
			= {GoogleMap | orig & markers = orig.GoogleMap.markers ++ [marker]}
		# mbMarkerDrag = fromJSON json
		| isJust mbMarkerDrag
			# {MarkerDragUpdate|index,point}		= fromJust mbMarkerDrag
			= {GoogleMap | orig & markers = [if (i == index) {GoogleMapMarker|m & position = point} m \\ m <- orig.GoogleMap.markers & i <- [0..]]}
		
		| otherwise = orig

gDefaultMask{|GoogleMap|} _ = [Touched []]

gVerify{|GoogleMap|} _ vst = alwaysValid vst //Maps are always valid

// -- Utility Functions --
defaultMap :: GoogleMap
defaultMap = { GoogleMap
		| center 			= {GoogleMapPosition|lat = 51.82, lng = 5.86}
		, mapTypeControl	= True
		, panControl		= True
		, streetViewControl	= True
		, zoomControl		= True
		, scaleControl		= True
		, scrollwheel		= True
		, draggable			= True
		, zoom				= 10
		, mapType			= ROADMAP
		, markers			= []
		}

minimalMap :: GoogleMap
minimalMap = { GoogleMap
		| center 			= {GoogleMapPosition|lat = 51.82, lng = 5.86}
		, mapTypeControl	= False
		, panControl		= False
		, streetViewControl	= False
		, zoomControl		= False
		, scaleControl		= False
		, scrollwheel		= False
		, draggable			= False
		, zoom				= 10
		, mapType			= ROADMAP
		, markers			= []
		}

convertToStaticMap :: !GoogleMap -> GoogleStaticMap
convertToStaticMap map =:{GoogleMap | center = {lat,lng}, zoom, mapType, markers}
# url 		= "http://maps.google.com/maps/api/staticmap?"
# cntr		= "center="+++(toString lat)+++","+++(toString lng)
# zm		= "zoom="+++(toString zoom)
# sz		= "size=650x600"
# tp		= "maptype="+++(toString mapType)
# mrkrs		= "markers="+++(convertMarkers markers)
= GoogleStaticMap 650 600 (url+++cntr+++"&"+++zm+++"&"+++sz+++"&"+++tp+++"&"+++mrkrs+++"&sensor=false&key="+++GOOGLE_API_KEY)
where
	convertMarkers :: [GoogleMapMarker] -> String
	convertMarkers [] = "";
	convertMarkers [x] = convertMarker x
	convertMarkers [x:xs] = (convertMarker x)+++"|"+++(convertMarkers xs)
	
	convertMarker :: GoogleMapMarker -> String
	convertMarker mrkr =: {position ={lat,lng}, infoWindow}
	= toString lat+++","+++toString lng
