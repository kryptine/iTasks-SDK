implementation module iTasks.API.Extensions.GIS.GoogleMap

import iTasks, iTasks.API.Core.Client.Editlet, iTasks.API.Core.Client.Interface
import Data.Functor, Text

from Data.Map import newMap

//--------------------------------------------------------------------------------------------------

:: GoogleMapState :== Maybe (JSVal JSObject)

// Parameter object for creating google.maps.Map
:: MapOptions = {center    :: JSVal JSObject
          		,zoom      :: Int
          		,mapTypeId :: JSVal JSObject
        		}

googleMapEditlet :: GoogleMap -> Editlet GoogleMap GoogleMapDiff
googleMapEditlet g = {Editlet
				|value		= g
				,html		= \id -> DivTag [IdAttr (mapdomid id), StyleAttr "width:100%; height:100%"] []
				,handlers	= [ComponentEvent "editlet" "init" onUpdate, ComponentEvent "editlet" "update" onUpdate]
				,genDiff	= genDiff
				,appDiff	= appDiff
				}
where
	mapdomid id = "map_place_holder_" +++ id

	updatePerspective _ _ map (Just mapobj) world 
		# (center, mapobj, world) = callObjectMethod "getCenter" [] mapobj world
		# (lat, center, world)  = callObjectMethod "lat" [] center world
		# (lng, center, world)  = callObjectMethod "lng" [] center world
		= ({map & perspective={GoogleMapPerspective | map.perspective & center = {lat = jsValToReal lat, lng = jsValToReal lng}}}, Just mapobj, world)
		
    onScriptLoad id event map _ world
	    # world = setDomAttr (mapdomid id) "innerHTML" (toJSVal "<div id=\"map_canvas\" style=\"width:100%; height:100%\"/>") world
	    # (mapdiv, world) = getDomElement "map_canvas" world
	        
	    # (mapTypeId, world) = findObject (toString (map.perspective.GoogleMapPerspective.type)) world
	    # (center, world) = jsNewObject "google.maps.LatLng" 
	    				[toJSArg map.perspective.GoogleMapPerspective.center.lat
	    				,toJSArg map.perspective.GoogleMapPerspective.center.lng] world

	    # (mapobj, world) = jsNewObject "google.maps.Map" 
	    				[toJSArg mapdiv
			    		,toJSArg {zoom = map.perspective.GoogleMapPerspective.zoom, center = center, mapTypeId = mapTypeId}] world

	    # (mapevent, world) = findObject "google.maps.event" world
		# (_, mapevent, world) = callObjectMethod "addListener" [toJSArg mapobj, toJSArg "dragend", toJSArg onChange] mapevent world
		# (_, mapevent, world) = callObjectMethod "addListener" [toJSArg mapobj, toJSArg "maptypeid_changed", toJSArg onChange] mapevent world
		# (_, mapevent, world) = callObjectMethod "addListener" [toJSArg mapobj, toJSArg "zoom_changed", toJSArg onChange] mapevent world
		= (map, Just mapobj, world)
	where
		onChange = createEditletEventHandler updatePerspective id

	onUpdate :: !ComponentId !(JSVal EditletEvent) !GoogleMap !GoogleMapState !*JSWorld -> (!GoogleMap, !GoogleMapState, !*JSWorld)
	onUpdate id event map Nothing world
		# (mapsobj, world) = findObject "google.maps" world
		| jsIsUndefined mapsobj
		= (map, Nothing, loadMapsAPI id event world)
		= onScriptLoad id event map Nothing world
		
	onUpdate id event map st world = (map, st, world)
		
	loadMapsAPI id e world	
		# (_, world) = jsSetObjectAttr "gmapscallback" (createEditletEventHandler onScriptLoad id) jsWindow world
		= addJSFromUrl "http://maps.googleapis.com/maps/api/js?sensor=false&callback=gmapscallback"
				Nothing world
			
	genDiff :: GoogleMap GoogleMap -> Maybe GoogleMapDiff
	genDiff g1 g2 = Just (g1.perspective, g1.GoogleMap.markers)

	appDiff :: GoogleMapDiff GoogleMap -> GoogleMap
	appDiff (p, ms) g = {g & perspective = p, markers = ms}

//--------------------------------------------------------------------------------------------------

instance toString GoogleMapType
where
	toString ROADMAP = "google.maps.MapTypeId.ROADMAP"
	toString SATELLITE = "google.maps.MapTypeId.SATELLITE"
	toString HYBRID = "google.maps.MapTypeId.HYBRID"
	toString TERRAIN = "google.maps.MapTypeId.TERRAIN"

//* Geograpic data and Google Maps
gEditor{|GoogleMap|} dp vv=:(val,mask,ver) meta vst=:{VSt|taskId}
	# editOpts	= {UIEditOpts|taskId=taskId,editorId=editorId dp,value=Nothing}
	# opts		= mapOpts val
	= (NormalEditor [(UIEditGoogleMap defaultSizeOpts editOpts opts,/*editorAttributes vv meta*/ newMap)],vst)
where	
	mapOpts map =
		{ UIGoogleMapOpts
		| center = (map.perspective.GoogleMapPerspective.center.lat,map.perspective.GoogleMapPerspective.center.lng)
		, mapType = mapType map.perspective.GoogleMapPerspective.type
		, markers = [{UIGoogleMapMarker|markerId=markerId,position=(lat,lng),title=title,icon=icon,infoWindow=fmap toString infoWindow,draggable=draggable,selected=selected}
					\\ {GoogleMapMarker|markerId,position={lat,lng},title,icon,infoWindow,draggable,selected} <- map.GoogleMap.markers]
		, options =
			{ UIGoogleMapOptions
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
	mapType ROADMAP 	= "ROADMAP"
	mapType SATELLITE 	= "SATELLITE"
	mapType HYBRID 		= "HYBRID"
	mapType TERRAIN 	= "TERRAIN"

gVisualizeText{|GoogleMapPosition|} _  {GoogleMapPosition|lat,lng} = [toString lat + " " + toString lng]

//Helper types for GoogleMap gUpdate instance
:: MVCUpdate = 
	{ center			:: !(Real,Real)
	, zoom				:: !Int
	, type				:: !GoogleMapType
	}	
	
:: MapClickUpdate = 
	{ event				:: !ClickEvent
	, point				:: !(Real,Real)
	}

:: ClickEvent	= LEFTCLICK | RIGHTCLICK | DBLCLICK

:: MarkerClickUpdate =
	{ index				:: !Int
	, event				:: !ClickEvent
	}
:: MarkerDragUpdate = 
	{ index				:: !Int
	, point				:: !(Real,Real)
	}

derive JSONDecode MVCUpdate, MapClickUpdate, ClickEvent, MarkerClickUpdate, MarkerDragUpdate

gUpdate{|GoogleMap|} target upd val = basicUpdate parseUpdate target upd val
where
	parseUpdate json orig
		# mbMVC		= fromJSON json
		| isJust mbMVC
			# {MVCUpdate|center=(lat,lng),zoom,type} = fromJust mbMVC
			= Just {GoogleMap | orig & perspective = {GoogleMapPerspective|orig.perspective & center = {lat=lat,lng=lng}, zoom = zoom, type = type}}
		# mbMarkerDrag = fromJSON json
		| isJust mbMarkerDrag
			# {MarkerDragUpdate|index,point=(lat,lng)}	= fromJust mbMarkerDrag
			= Just {GoogleMap | orig & markers = [if (i == index) {GoogleMapMarker|m & position = {lat=lat,lng=lng}} m \\ m <- orig.GoogleMap.markers & i <- [0..]]}
		# mbMarkerClick = fromJSON json
		| isJust mbMarkerClick
			# {MarkerClickUpdate|index,event} = fromJust mbMarkerClick
			= Just {GoogleMap| orig & markers = [{GoogleMapMarker|m & selected = i == index} \\ m <- orig.GoogleMap.markers & i <- [0..]]}
		| otherwise	
			= Just orig

gVerify{|GoogleMap|} _ mv = alwaysValid mv
//derive gVerify GoogleMap

gDefault{|GoogleMapPerspective|} =
	{ GoogleMapPerspective
	| type				= ROADMAP
	, center 			= {GoogleMapPosition|lat = 51.82, lng = 5.86}
	, zoom				= 10
	}
gDefault{|GoogleMapSettings|} =
	{ GoogleMapSettings
	| mapTypeControl	= True
	, panControl		= True
	, streetViewControl	= True
	, zoomControl		= True
	, scaleControl		= True
	, scrollwheel		= True
	, draggable			= True
	}

derive JSONEncode		GoogleMap, GoogleMapSettings, GoogleMapPerspective, GoogleMapPosition, GoogleMapMarker, GoogleMapType, GoogleMapIcon, GoogleMapComplexIcon
derive JSONDecode		GoogleMap, GoogleMapSettings, GoogleMapPerspective, GoogleMapPosition, GoogleMapMarker, GoogleMapType, GoogleMapIcon, GoogleMapComplexIcon
derive gDefault			GoogleMap, GoogleMapPosition, GoogleMapMarker, GoogleMapType, GoogleMapIcon, GoogleMapComplexIcon
derive gEq				GoogleMap, GoogleMapSettings, GoogleMapPerspective, GoogleMapPosition, GoogleMapMarker, GoogleMapType, GoogleMapIcon, GoogleMapComplexIcon
derive gVisualizeText	GoogleMap, GoogleMapSettings, GoogleMapPerspective, GoogleMapMarker, GoogleMapType, GoogleMapIcon, GoogleMapComplexIcon
derive gEditor GoogleMapSettings, GoogleMapPerspective, GoogleMapPosition, GoogleMapMarker, GoogleMapType, GoogleMapIcon, GoogleMapComplexIcon
derive gEditMeta		GoogleMap, GoogleMapSettings, GoogleMapPerspective, GoogleMapPosition, GoogleMapMarker, GoogleMapType, GoogleMapIcon, GoogleMapComplexIcon
derive gUpdate			GoogleMapSettings, GoogleMapPerspective, GoogleMapPosition, GoogleMapMarker, GoogleMapType, GoogleMapIcon, GoogleMapComplexIcon
derive gVerify			GoogleMapSettings, GoogleMapPerspective, GoogleMapPosition, GoogleMapMarker, GoogleMapType, GoogleMapIcon, GoogleMapComplexIcon



