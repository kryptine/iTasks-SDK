implementation module GoogleMaps

import Html, StdEnv, JSON, GenUpdate, GenVisualize, GenVerify, GenMerge

derive JSONEncode TUIGoogleMap, TUIGoogleMapOptions, TUIGoogleStaticMap
derive JSONDecode MVCUpdate, ClickUpdate, ClickSource, ClickEvent

derive gVisualize   	GoogleMapMarker, GoogleMapInfoWindow, GoogleMapType
derive gUpdate	  		GoogleMapMarker, GoogleMapInfoWindow, GoogleMapType, GoogleStaticMap
derive gMerge	  		GoogleMap, GoogleMapMarker, GoogleMapInfoWindow, GoogleMapType, GoogleStaticMap
derive gVerify			GoogleMap, GoogleMapMarker, GoogleMapInfoWindow, GoogleMapType, GoogleStaticMap
derive JSONEncode		GoogleMap, GoogleMapMarker, GoogleMapInfoWindow, GoogleMapType, GoogleStaticMap
derive JSONDecode		GoogleMap, GoogleMapMarker, GoogleMapInfoWindow, GoogleMapType, GoogleStaticMap

derive bimap	Maybe, (,)

:: TUIGoogleMap = 
	{ center 			:: Coordinate
	, width				:: Int
	, height			:: Int
	, mapType			:: GoogleMapType
	, markers			:: [GoogleMapMarker]
	, xtype				:: String
	, id				:: String
	, name				:: String
	, fieldLabel		:: Maybe String
	, hideLabel			:: Bool
	, editor			:: Bool
	, options			:: TUIGoogleMapOptions
	}
	
:: TUIGoogleMapOptions =
	{ mapTypeControl 	:: Bool
	, navigationControl :: Bool
	, scaleControl		:: Bool
	, streetViewControl	:: Bool
	, scrollwheel		:: Bool
	, draggable			:: Bool
	, zoom				:: Int
	}
	
::TUIGoogleStaticMap =
	{ width				:: Int
	, height			:: Int
	, xtype				:: String
	, id				:: String
	, name				:: String
	, url				:: String
	}
	
instance toString GoogleMapType
where 
	toString ROADMAP 	= "ROADMAP"
	toString SATELLITE 	= "SATELLITE"
	toString HYBRID 	= "HYBRID"
	toString TERRAIN 	= "TERRAIN"

gVisualize {|GoogleMap|} val vst=:{vizType, label, idPrefix, currentPath, optional, useLabels, updateMask, verifyMask}
	# (cmu,um) = popMask updateMask
	# (cmv,vm) = popMask verifyMask
	= case vizType of
		VEditorDefinition = ([TUIFragment (TUICustom   ((mapPanel val label (not useLabels) idPrefix currentPath True)))],{VSt | vst & currentPath = stepDataPath currentPath, updateMask = um, verifyMask = vm})
		_				  = (staticMapPanel val, {VSt | vst & currentPath = stepDataPath currentPath})
where
	mapPanel Nothing fl hl 		  idp cp ed	= toJSON (tuidef mkMap fl hl idp cp ed)
	mapPanel (Just map) fl hl idp cp ed		= toJSON (tuidef map   fl hl idp cp ed)

	staticMapPanel Nothing
		# (GoogleStaticMap w h u) = convertToStaticMap mkMap
		= ([HtmlFragment [ImgTag [SrcAttr u, WidthAttr (toString w), HeightAttr (toString h)]]])
	staticMapPanel (Just map)
		# (GoogleStaticMap w h u) = convertToStaticMap map
		= ([HtmlFragment [ImgTag [SrcAttr u, WidthAttr (toString w), HeightAttr (toString h)]]])

	tuidef map fl hl idp cp ed =
		{ TUIGoogleMap
		| center = map.GoogleMap.center
		, width = map.GoogleMap.width
		, height = map.GoogleMap.height
		, mapType = map.GoogleMap.mapType
		, markers = map.GoogleMap.markers
		, xtype = "itasks.tui.GMapControl"
		, name = dp2s cp
		, id = dp2id idp cp
		, fieldLabel = fl
		, hideLabel = hl
		, editor = ed
		, options =
			{ TUIGoogleMapOptions
			| mapTypeControl = map.GoogleMap.mapTypeControl
			, navigationControl = map.GoogleMap.navigationControl
			, scaleControl = map.GoogleMap.scaleControl
			, streetViewControl = map.GoogleMap.streetViewControl
			, scrollwheel = map.GoogleMap.scrollwheel
			, draggable = map.GoogleMap.draggable
			, zoom = map.GoogleMap.zoom
			}
		}

gVisualize {|GoogleStaticMap|} Nothing vst = ([TextFragment "-"],vst)
gVisualize {|GoogleStaticMap|} (Just (GoogleStaticMap w h u)) vst=:{vizType,idPrefix,currentPath}
	= case vizType of
		VHtmlDisplay	= ([HtmlFragment [ImgTag [SrcAttr u, WidthAttr (toString w), HeightAttr (toString h)]]],{VSt | vst & currentPath = stepDataPath currentPath})
		VTextDisplay	= ([TextFragment ("Static Map: "+++u)],{VSt | vst & currentPath = stepDataPath currentPath})
		VHtmlLabel		= ([HtmlFragment [Text "Static Map"]],{VSt | vst & currentPath = stepDataPath currentPath})
		VTextLabel		= ([TextFragment "Static Map"],{VSt | vst & currentPath = stepDataPath currentPath})
		_				= ([TUIFragment (TUICustom ((toJSON staticMap)))],{VSt | vst & currentPath = stepDataPath currentPath})
where
	staticMap =
		{ TUIGoogleStaticMap
		| width 	= w
		, height 	= h	
		, xtype		= "itasks.gstaticmappanel"
		, name		= dp2s currentPath
		, id		= dp2id idPrefix currentPath
		, url		= u
		}	
		
gUpdate {|GoogleMap|} _ ust =: {USt | mode=UDCreate,newMask} = (mkMap,{USt | ust & newMask = appendToMask newMask Untouched})

gUpdate {|GoogleMap|} s ust =: {USt | mode=UDSearch, searchPath, currentPath, update,oldMask,newMask}
	# (cm,om) = popMask oldMask
	| currentPath == searchPath
		= (parseUpdate s update, {USt | ust & newMask = appendToMask newMask (Touched True []), oldMask = om})
	| otherwise
		= (s, {USt | ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask (cleanUpdMask cm), oldMask = om})
where
	parseUpdate orig update
		# mbMVC		= fromJSON (fromString update)
		| isJust mbMVC
			# mvc = fromJust mbMVC
			= {GoogleMap | orig & center = mvc.MVCUpdate.center, zoom = mvc.MVCUpdate.zoom, mapType = mvc.MVCUpdate.type}
		# mbClick 	= fromJSON (fromString update)
		| isJust mbClick
			# click = fromJust mbClick
			# marker = {GoogleMapMarker | position = click.ClickUpdate.point, infoWindow = {GoogleMapInfoWindow | content = "", width=0}} 
			= {GoogleMap | orig & markers = [marker:orig.GoogleMap.markers]}
		| otherwise = orig

gUpdate {|GoogleMap|} s ust =: {USt | mode = UDMask, currentPath, newMask}
	= (s, {USt | ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask (Touched True [])})

gUpdate {|GoogleMap|} s ust = (s,ust)

// -- Utility Functions --

mkMap :: GoogleMap
mkMap = { GoogleMap
		| center 			= (51.82,5.86)
		, width 			= 710
		, height 			= 300
		, mapTypeControl	= True
		, navigationControl = True
		, scaleControl		= True
		, streetViewControl	= True
		, scrollwheel		= True
		, draggable			= True
		, zoom				= 10
		, mapType			= ROADMAP
		, markers			= []
		}

minimalMap :: GoogleMap
minimalMap = { GoogleMap
		| center 			= (51.82,5.86)
		, width 			= 710
		, height 			= 300
		, mapTypeControl	= False
		, navigationControl = False
		, scaleControl		= False
		, streetViewControl	= False
		, scrollwheel		= False
		, draggable			= False
		, zoom				= 10
		, mapType			= ROADMAP
		, markers			= []
		}


convertToStaticMap :: GoogleMap -> GoogleStaticMap
convertToStaticMap map =:{GoogleMap | center = (lat,lng), width, height, zoom, mapType, markers}
# url 		= "http://maps.google.com/maps/api/staticmap?"
# cntr		= "center="+++(toString lat)+++","+++(toString lng)
# zm		= "zoom="+++(toString zoom)
# sz		= "size="+++(toString width)+++"x"+++(toString height)
# tp		= "maptype="+++(toString mapType)
# mrkrs		= "markers="+++(convertMarkers markers)
= GoogleStaticMap width height (url+++cntr+++"&"+++zm+++"&"+++sz+++"&"+++tp+++"&"+++mrkrs+++"&sensor=false&key="+++GOOGLE_API_KEY)
where
	convertMarkers :: [GoogleMapMarker] -> String
	convertMarkers [] = "";
	convertMarkers [x] = convertMarker x
	convertMarkers [x:xs] = (convertMarker x)+++"|"+++(convertMarkers xs)
	
	convertMarker :: GoogleMapMarker -> String
	convertMarker mrkr =: {position = (lat,lng), infoWindow}
	= toString lat+++","+++toString lng
