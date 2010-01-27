implementation module GeoDomain

import Html, InteractionTasks, StdEnv, JSON, CommonDomain

derive JSONEncode TUIGoogleMap, TUIGoogleStaticMap,GoogleMapMarker, GoogleMapType, GoogleMapInfoWindow
derive JSONDecode MVCUpdate, ClickUpdate, GoogleMapType, ClickSource, ClickEvent

derive gPrint 	  	GoogleMap, GoogleMapMarker, GoogleMapInfoWindow, GoogleMapType, GoogleStaticMap
derive gParse 	  	GoogleMap, GoogleMapMarker, GoogleMapInfoWindow, GoogleMapType, GoogleStaticMap
derive gVisualize   GoogleMapMarker, GoogleMapInfoWindow, GoogleMapType
derive gUpdate	  	GoogleMapMarker, GoogleMapInfoWindow, GoogleMapType, GoogleStaticMap

:: TUIGoogleMap = 
	{ center 			:: Coordinate
	, width				:: Int
	, height			:: Int
	, mapTypeControl 	:: Bool
	, navigationControl :: Bool
	, scaleControl		:: Bool
	, zoom				:: Int
	, mapType			:: GoogleMapType
	, markers			:: [GoogleMapMarker]
	, xtype				:: String
	, id				:: String
	, name				:: String
	, fieldLabel		:: Maybe String
	, hideLabel			:: Bool
	, editor			:: Bool
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

gVisualize {|GoogleMap|} old new vst=:{vizType, label, idPrefix, currentPath, valid, optional, useLabels}
	= case vizType of
		VEditorDefinition = ([TUIFragment (TUICustom   (JSON ((mapPanel old (label2s optional label) (not useLabels) idPrefix currentPath True))))], {VSt | vst & currentPath = stepDataPath currentPath })
		VEditorUpdate	  = ([TUIUpdate   (TUISetValue (dp2id idPrefix currentPath) (mapPanel new (label2s optional label) (not useLabels) idPrefix currentPath True))], {VSt | vst & currentPath = stepDataPath currentPath })
		//_				  = ([TUIFragment (TUICustom   (JSON ((mapPanel old (label2s optional label) (not useLabels) idPrefix currentPath False))))], {VSt | vst & currentPath = stepDataPath currentPath })
		_				  = (staticMapPanel old, {VSt | vst & currentPath = stepDataPath currentPath})

mapPanel VBlank fl hl 		  idp cp ed = toJSON (tuidef mkMap fl hl idp cp ed)
mapPanel (VValue map _) fl hl idp cp ed = toJSON (tuidef map   fl hl idp cp ed)

staticMapPanel VBlank
	# (GoogleStaticMap w h u) = convertToStaticMap mkMap
	= ([HtmlFragment [ImgTag [SrcAttr u, WidthAttr (toString w), HeightAttr (toString h)]]])
staticMapPanel (VValue map _)
	# (GoogleStaticMap w h u) = convertToStaticMap map
	= ([HtmlFragment [ImgTag [SrcAttr u, WidthAttr (toString w), HeightAttr (toString h)]]])

tuidef map fl hl idp cp ed =
		{ TUIGoogleMap
		| center = map.GoogleMap.center
		, width = map.GoogleMap.width
		, height = map.GoogleMap.height
		, mapTypeControl = map.GoogleMap.mapTypeControl
		, navigationControl = map.GoogleMap.navigationControl
		, scaleControl = map.GoogleMap.scaleControl
		, zoom = map.GoogleMap.zoom
		, mapType = map.GoogleMap.mapType
		, markers = map.GoogleMap.markers
		, xtype = "itasks.gmappanel"
		, name = dp2s cp
		, id = dp2id idp cp
		, fieldLabel = fl
		, hideLabel = hl
		, editor = ed
		}

gVisualize {|GoogleStaticMap|} VBlank _ vst = ([TextFragment "-"],vst)
gVisualize {|GoogleStaticMap|} (VValue (GoogleStaticMap w h u) _ ) _ vst=:{vizType,idPrefix,currentPath}
	= case vizType of
		VHtmlDisplay	= ([HtmlFragment [ImgTag [SrcAttr u, WidthAttr (toString w), HeightAttr (toString h)]]],{VSt | vst & currentPath = stepDataPath currentPath})
		VTextDisplay	= ([TextFragment ("Static Map: "+++u)],{VSt | vst & currentPath = stepDataPath currentPath})
		VHtmlLabel		= ([HtmlFragment [Text "Static Map"]],{VSt | vst & currentPath = stepDataPath currentPath})
		VTextLabel		= ([TextFragment "Static Map"],{VSt | vst & currentPath = stepDataPath currentPath})
		_				= ([TUIFragment (TUICustom (JSON (toJSON staticMap)))],{VSt | vst & currentPath = stepDataPath currentPath})
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
		
gUpdate {|GoogleMap|} _ ust =: {USt | mode=UDCreate} = (
	{ GoogleMap
	| center 			= (51.82,5.86)
	, width	 			= 400
	, height 			= 300
	, mapTypeControl 	= True
	, navigationControl = True
	, scaleControl 		= True
	, zoom				= 10
	, mapType			= ROADMAP
	, markers			= []
	}, ust)
	
gUpdate {|GoogleMap|} s ust =: {USt | mode=UDSearch, searchPath, currentPath, update}
	| currentPath == searchPath
		= (parseUpdate s update, toggleMask {USt | ust & mode = UDDone})
	| otherwise
		= (s, {USt | ust & currentPath = stepDataPath currentPath})
where
	parseUpdate orig update
	# mbMVC		= fromJSON update
	| isJust mbMVC
		# mvc = fromJust mbMVC
		= {GoogleMap | orig & center = mvc.MVCUpdate.center, zoom = mvc.MVCUpdate.zoom, mapType = mvc.MVCUpdate.type}
	# mbClick 	= fromJSON update
	| isJust mbClick
		# click = fromJust mbClick
		# marker = {GoogleMapMarker | position = click.ClickUpdate.point, infoWindow = {GoogleMapInfoWindow | content = "", width=0}} 
		= {GoogleMap | orig & markers = [marker:orig.GoogleMap.markers]}
	| otherwise = orig

gUpdate {|GoogleMap|} s ust =: {USt | mode = UDMask, currentPath, mask}
	= (s, {USt | ust & currentPath = stepDataPath currentPath, mask = [currentPath:mask]})

gUpdate {|GoogleMap|} s ust = (s,ust)

// -- Utility Functions --

mkMap :: GoogleMap
mkMap = { GoogleMap
		| center 			= (51.82,5.86)
		, width 			= 400
		, height 			= 300
		, mapTypeControl	= True
		, navigationControl = True
		, scaleControl		= True
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
