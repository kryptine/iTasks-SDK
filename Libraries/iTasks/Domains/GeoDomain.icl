implementation module GeoDomain

import Html, InteractionTasks, StdEnv, JSON

derive JSONEncode JSONMap, MapMarker, GoogleMapType, MapInfoWindow
derive JSONDecode MVCUpdate, ClickUpdate, GoogleMapType, ClickSource, ClickEvent

derive gPrint 	  	Map, StaticMap, MapMarker, MapInfoWindow, GoogleMapType
derive gParse 	  	Map, StaticMap, MapMarker, MapInfoWindow, GoogleMapType
derive gVisualize   StaticMap, MapMarker, MapInfoWindow, GoogleMapType
derive gUpdate	  	StaticMap, MapMarker, MapInfoWindow, GoogleMapType

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
	
:: MVCUpdate = 
	{ center			:: Coordinate
	, zoom				:: Int
	, type				:: GoogleMapType
	}	
	
:: ClickUpdate = 
	{ event				:: ClickEvent
	, source			:: ClickSource
	, point				:: Coordinate
	}
	
:: ClickEvent	= LEFTCLICK | RIGHTCLICK | DBLCLICK
:: ClickSource  = MAP | MARKER Coordinate

instance toString StaticMap
where
	toString (StaticMap url) = url
	
instance toString GoogleMapType
where 
	toString ROADMAP = "ROADMAP"
	toString SATELLITE = "SATELLITE"
	toString HYBRID = "HYBRID"
	toString TERRAIN = "TERRAIN"
	
instance html StaticMap
where 
	html (StaticMap url) = [ImgTag [SrcAttr url]]
	
convertToStaticMap :: Map -> StaticMap
convertToStaticMap map = (StaticMap "")

gVisualize {|Map|} old new vst=:{vizType,label,idPrefix,currentPath, valid, optional}
	= case vizType of
		VEditorDefinition	= ([TUIFragment (mapsPanel old currentPath idPrefix True)], {VSt | vst & currentPath = stepDataPath currentPath })
		VEditorUpdate		= ([TUIUpdate (TUIReplace mapid (mapsPanel old currentPath idPrefix True))],{VSt | vst & currentPath = stepDataPath currentPath })
		_					= ([TUIFragment (mapsPanel new currentPath idPrefix False)], {VSt | vst & currentPath = stepDataPath currentPath })
where
	mapsPanel  VBlank cp ip editor  = TUICustom (JSON (""))
	mapsPanel (VValue map _) cp ip editor = TUICustom (JSON (toJSON (jsonMap map cp ip editor)))
	
	mapid = dp2id idPrefix currentPath
	
	jsonMap map cp ip editor =
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
		, name = dp2s cp
		, id = dp2id ip cp
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
		= {Map | orig & center = mvc.MVCUpdate.center, zoom = mvc.MVCUpdate.zoom, mapType = mvc.MVCUpdate.type}
	# mbClick 	= fromJSON update
	| isJust mbClick
		# click = fromJust mbClick
		# marker = {MapMarker | position = click.ClickUpdate.point, infoWindow = {MapInfoWindow | content = "HI!", width=200}}
		= {Map | orig & markers = [marker : orig.Map.markers]}
	| otherwise = orig

gUpdate {|Map|} s ust =: {USt | mode = UDMask, currentPath, mask}
	= (s, {USt | ust & currentPath = stepDataPath currentPath, mask = [currentPath:mask]})

gUpdate {|Map|} s ust = (s,ust)