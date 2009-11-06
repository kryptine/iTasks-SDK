implementation module GeoDomain

import Html, InteractionTasks, StdEnv, JSON, CommonDomain

derive JSONEncode JSONMap, MapMarker, GoogleMapType, MapInfoWindow
derive JSONDecode MVCUpdate, ClickUpdate, GoogleMapType, ClickSource, ClickEvent

derive gPrint 	  	Map, MapMarker, MapInfoWindow, GoogleMapType
derive gParse 	  	Map, MapMarker, MapInfoWindow, GoogleMapType
derive gVisualize   MapMarker, MapInfoWindow, GoogleMapType
derive gUpdate	  	MapMarker, MapInfoWindow, GoogleMapType

:: JSONMap = 
	{ center 			:: Coordinate
	, width				:: Int
	, height			:: Int
	, mapTypeControl 	:: Bool
	, navigationControl :: Bool
	, scaleControl		:: Bool
	, zoom				:: Int
	, mapType			:: GoogleMapType
	, markers			:: [MapMarker]
	, xtype				:: String
	, id				:: String
	, name				:: String
	, isEditor			:: Bool
	}
	
instance toString GoogleMapType
where 
	toString ROADMAP = "ROADMAP"
	toString SATELLITE = "SATELLITE"
	toString HYBRID = "HYBRID"
	toString TERRAIN = "TERRAIN"
	
instance html StaticMap
where
	html (StaticMap width height url) = [DivTag [WidthAttr (toString width), HeightAttr (toString height)] [ImgTag [SrcAttr url, WidthAttr (toString width), HeightAttr (toString height)]]]
	
gVisualize {|Map|} old new vst=:{vizType,label,idPrefix,currentPath, valid, optional}
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
		| center = map.Map.center
		, width = map.Map.width
		, height = map.Map.height
		, mapTypeControl = map.Map.mapTypeControl
		, navigationControl = map.Map.navigationControl
		, scaleControl = map.Map.scaleControl
		, zoom = map.Map.zoom
		, mapType = map.Map.mapType
		, markers = map.Map.markers
		, xtype = "itasks.gmappanel"
		, name = dp2s currentPath
		, id = dp2id idPrefix currentPath
		, isEditor = editor
		}

gUpdate {|Map|} _ ust =: {USt | mode=UDCreate} = (
	{ Map
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
	
gUpdate {|Map|} s ust =: {USt | mode=UDSearch, searchPath, currentPath, update}
	| currentPath == searchPath
		= (parseUpdate s update, toggleMask {USt | ust & mode = UDDone})
	| otherwise
		= (s, {USt | ust & currentPath = stepDataPath currentPath})
where
	parseUpdate orig update
	# mbMVC		= fromJSON update
	| isJust mbMVC
		# mvc = fromJust mbMVC
		//= case orig.Map.mvcFun of
		//	Nothing 	
		= {Map | orig & center = mvc.MVCUpdate.center, zoom = mvc.MVCUpdate.zoom, mapType = mvc.MVCUpdate.type}
		//	(Just fun)	= fun mvc
	# mbClick 	= fromJSON update
	| isJust mbClick
		# click = fromJust mbClick
		//= case orig.Map.clickFun of
		//	Nothing 	= orig
		//	(Just fun) 	= fun click
		# marker = {MapMarker | position = click.ClickUpdate.point, infoWindow = {MapInfoWindow | content = "", width=0}} 
		= {Map | orig & markers = [marker:orig.Map.markers]}
	| otherwise = orig

gUpdate {|Map|} s ust =: {USt | mode = UDMask, currentPath, mask}
	= (s, {USt | ust & currentPath = stepDataPath currentPath, mask = [currentPath:mask]})

gUpdate {|Map|} s ust = (s,ust)

// -- Utility Functions --

mkMap :: Map
mkMap = { Map
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

convertToStaticMap :: Map -> StaticMap
convertToStaticMap map =:{Map | center = (lat,lng), width, height, zoom, mapType, markers}
# url 		= "http://maps.google.com/maps/api/staticmap?"
# cntr		= "center="+++(toString lat)+++","+++(toString lng)
# zm		= "zoom="+++(toString zoom)
# sz		= "size="+++(toString width)+++"x"+++(toString height)
# tp		= "maptype="+++(toString mapType)
# mrkrs		= "markers="+++(convertMarkers markers)
= StaticMap width height (url+++cntr+++"&"+++zm+++"&"+++sz+++"&"+++tp+++"&"+++mrkrs+++"&sensor=false&key="+++GOOGLE_API_KEY)
where
	convertMarkers :: [MapMarker] -> String
	convertMarkers [] = "";
	convertMarkers [x] = convertMarker x
	convertMarkers [x:xs] = (convertMarker x)+++"|"+++(convertMarkers xs)
	
	convertMarker :: MapMarker -> String
	convertMarker mrkr =: {position = (lat,lng), infoWindow}
	= toString lat+++","+++toString lng
