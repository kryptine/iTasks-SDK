implementation module GeoDomain

import Html, InteractionTasks, StdEnv, JSON, CommonDomain

derive JSONEncode JSONMap, JSONStaticMap,GoogleMapMarker, GoogleMapType, GoogleMapInfoWindow
derive JSONDecode MVCUpdate, ClickUpdate, GoogleMapType, ClickSource, ClickEvent

derive gPrint 	  	GoogleMap, GoogleMapMarker, GoogleMapInfoWindow, GoogleMapType, GoogleStaticMap
derive gParse 	  	GoogleMap, GoogleMapMarker, GoogleMapInfoWindow, GoogleMapType, GoogleStaticMap
derive gVisualize   GoogleMapMarker, GoogleMapInfoWindow, GoogleMapType
derive gUpdate	  	GoogleMapMarker, GoogleMapInfoWindow, GoogleMapType, GoogleStaticMap

:: JSONMap = 
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
	, isEditor			:: Bool
	}
	
::JSONStaticMap =
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

gVisualize {|GoogleMap|} old new vst=:{vizType,label,idPrefix,currentPath, valid, optional}
	= case vizType of
		VEditorDefinition	= ([TUIFragment (mapsPanel old True)], {VSt | vst & currentPath = stepDataPath currentPath })
		VEditorUpdate		
			= case new of
				VBlank = ([TUIFragment (mapsPanel old True)], {VSt | vst & currentPath = stepDataPath currentPath })
				(VValue map _) = ([TUIUpdate (TUISetValue mapid (toJSON (jsonMap map True)))], {VSt | vst & currentPath = stepDataPath currentPath })
		_	= ([TUIFragment (mapsPanel new False)], {VSt | vst & currentPath = stepDataPath currentPath })
where
	mapsPanel  VBlank editor  = TUICustom (JSON (""))
	mapsPanel (VValue map _) editor = TUICustom (JSON (toJSON (jsonMap map editor)))
	
	mapid = dp2id idPrefix currentPath
	
	jsonMap map editor =
		{ JSONMap
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
		, name = dp2s currentPath
		, id = dp2id idPrefix currentPath
		, isEditor = editor
		}

gVisualize {|GoogleStaticMap|} VBlank _ vst = ([TextFragment ""],vst)
gVisualize {|GoogleStaticMap|} (VValue (GoogleStaticMap w h u) _ ) _ vst=:{vizType,idPrefix,currentPath}
	= case vizType of
		VHtmlDisplay	= ([HtmlFragment [ImgTag [SrcAttr u, WidthAttr (toString w), HeightAttr (toString h)]]],{VSt | vst & currentPath = stepDataPath currentPath})
		//VHtmlDisplay	= ([HtmlFragment [IframeTag [SrcAttr u, FrameborderAttr "0", WidthAttr (toString w), HeightAttr (toString h)][(Text "Cannot Display iFrame")]]],{VSt | vst & currentPath = stepDataPath currentPath})
		VTextDisplay	= ([TextFragment ("Static Map: "+++u)],{VSt | vst & currentPath = stepDataPath currentPath})
		VHtmlLabel		= ([HtmlFragment [Text "Static Map"]],{VSt | vst & currentPath = stepDataPath currentPath})
		VTextLabel		= ([TextFragment "Static Map"],{VSt | vst & currentPath = stepDataPath currentPath})
		_				= ([TUIFragment (TUICustom (JSON (toJSON staticMap)))],{VSt | vst & currentPath = stepDataPath currentPath})
where
	staticMap =
		{ JSONStaticMap
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
	, width	 			= 500
	, height 			= 400
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

//gUpdate {|StaticMap|} s ust =:{USt | currentPath, mask} = (s,{USt | ust & currentPath = stepDataPath currentPath, mask = [currentPath:mask]})

// -- Utility Functions --

mkMap :: GoogleMap
mkMap = { GoogleMap
		| center 			= (0.0,0.0)
		, width 			= 500
		, height 			= 400
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
