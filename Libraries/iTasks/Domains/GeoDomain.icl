implementation module GeoDomain

import Html, InteractionTasks, StdEnv, JSON

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

instance toString GoogleMapType
where 
	toString ROADMAP = "ROADMAP"
	toString SATELLITE = "SATELLITE"
	toString HYBRID = "HYBRID"
	toString TERRAIN = "TERRAIN"
	
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