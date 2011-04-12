implementation module GoogleMaps

import HTML, StdEnv, JSON, GenUpdate, GenVisualize, GenVerify

derive JSONEncode TUIGoogleMap, TUIGoogleMapOptions, TUIGoogleStaticMap
derive JSONDecode MVCUpdate, ClickUpdate, ClickSource, ClickEvent, MarkerDragUpdate

derive gVisualize   	GoogleMapPosition, GoogleMapMarker, GoogleMapInfoWindow, GoogleMapType
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

:: TUIGoogleMap = 
	{ center 			:: GoogleMapPosition
	, mapType			:: GoogleMapType
	, markers			:: [GoogleMapMarker]
	, xtype				:: String
	, taskId			:: String
	, name				:: String
	, fieldLabel		:: Maybe String
	, hideLabel			:: Bool
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
	, name				:: String
	, url				:: String
	}
	
instance toString GoogleMapType
where 
	toString ROADMAP 	= "ROADMAP"
	toString SATELLITE 	= "SATELLITE"
	toString HYBRID 	= "HYBRID"
	toString TERRAIN 	= "TERRAIN"

gVisualize {|GoogleMap|} val vst=:{vizType, label, currentPath, optional, useLabels, verifyMask, taskId}
	# (cmv,vm) = popMask verifyMask
	= case vizType of
		VEditorDefinition = ([TUIFragment (TUICustom ((mapPanel val label (not useLabels) currentPath True)))],{VSt | vst & currentPath = stepDataPath currentPath, verifyMask = vm})
		_				  = (staticMapPanel val, {VSt | vst & currentPath = stepDataPath currentPath})
where
	mapPanel Nothing	fl hl cp ed	= toJSON (tuidef mkMap fl hl cp ed)
	mapPanel (Just map)	fl hl cp ed = toJSON (tuidef map   fl hl cp ed)

	staticMapPanel Nothing
		# (GoogleStaticMap w h u) = convertToStaticMap mkMap
		= ([HtmlFragment (ImgTag [SrcAttr u, WidthAttr (toString w), HeightAttr (toString h)])])
	staticMapPanel (Just map)
		# (GoogleStaticMap w h u) = convertToStaticMap map
		= ([HtmlFragment (ImgTag [SrcAttr u, WidthAttr (toString w), HeightAttr (toString h)])])

	tuidef map fl hl cp ed =
		{ TUIGoogleMap
		| center = map.GoogleMap.center
		, mapType = map.GoogleMap.mapType
		, markers = map.GoogleMap.markers
		, xtype = "itasks.tui.GMapControl"
		, name = dp2s cp
		, taskId = taskId
		, fieldLabel = fl
		, hideLabel = hl
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

gVisualize {|GoogleStaticMap|} Nothing vst = ([TextFragment "-"],vst)
gVisualize {|GoogleStaticMap|} (Just (GoogleStaticMap w h u)) vst=:{vizType,currentPath}
	= case vizType of
		VHtmlDisplay	= ([HtmlFragment (ImgTag [SrcAttr u, WidthAttr (toString w), HeightAttr (toString h)])],{VSt | vst & currentPath = stepDataPath currentPath})
		VTextDisplay	= ([TextFragment ("Static Map: "+++u)],{VSt | vst & currentPath = stepDataPath currentPath})
		VHtmlLabel		= ([HtmlFragment (Text "Static Map")],{VSt | vst & currentPath = stepDataPath currentPath})
		VTextLabel		= ([TextFragment "Static Map"],{VSt | vst & currentPath = stepDataPath currentPath})
		_				= ([TUIFragment (TUICustom ((toJSON staticMap)))],{VSt | vst & currentPath = stepDataPath currentPath})
where
	staticMap =
		{ TUIGoogleStaticMap
		| width 	= w
		, height 	= h	
		, xtype		= "itasks.gstaticmappanel"
		, name		= dp2s currentPath
		, url		= u
		}	

gUpdate{|GoogleMap|} mode ust = basicUpdate mode parseUpdate mkMap ust
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

mkMap :: GoogleMap
mkMap = { GoogleMap
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

convertToStaticMap :: GoogleMap -> GoogleStaticMap
convertToStaticMap map =:{GoogleMap | center = {lat,lng}, zoom, mapType, markers}
# url 		= "http://maps.google.com/maps/api/staticmap?"
# cntr		= "center="+++(toString lat)+++","+++(toString lng)
# zm		= "zoom="+++(toString zoom)
# sz		= "size=800x600"
# tp		= "maptype="+++(toString mapType)
# mrkrs		= "markers="+++(convertMarkers markers)
= GoogleStaticMap 800 600 (url+++cntr+++"&"+++zm+++"&"+++sz+++"&"+++tp+++"&"+++mrkrs+++"&sensor=false&key="+++GOOGLE_API_KEY)
where
	convertMarkers :: [GoogleMapMarker] -> String
	convertMarkers [] = "";
	convertMarkers [x] = convertMarker x
	convertMarkers [x:xs] = (convertMarker x)+++"|"+++(convertMarkers xs)
	
	convertMarker :: GoogleMapMarker -> String
	convertMarker mrkr =: {position ={lat,lng}, infoWindow}
	= toString lat+++","+++toString lng
