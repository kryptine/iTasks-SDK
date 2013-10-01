implementation module iTasks.API.Extensions.GIS.GoogleMap

import iTasks
import iTasks.API.Core.Client.Editlet
import iTasks.API.Core.Client.Interface
import iTasks.API.Core.Client.Map

import Data.Functor, Text, StdMisc

from Data.Map import newMap

//--------------------------------------------------------------------------------------------------

:: GoogleMapState = {mapobj       :: JSVal JSObject
					,nextMarkerId :: Int
					,markerMap	  :: JSVal (JSMap String GoogleMapMarker)
					}

// Parameter object for creating google.maps.Map
:: MapOptions = {center    			:: JSVal JSObject
          		,zoom      			:: Int
          		,mapTypeId 			:: JSVal JSObject
				,mapTypeControl		:: Bool
				,panControl			:: Bool
				,zoomControl		:: Bool
				,streetViewControl	:: Bool
				,scaleControl		:: Bool
				,scrollwheel		:: Bool
				,draggable			:: Bool
        		}

// Parameter object for creating google.maps.Marker
:: MarkerOptions = {map        :: JSVal JSObject
				   ,position   :: JSVal JSObject
				   ,title      :: String
				   ,draggable  :: Bool
				   ,icon       :: Maybe (JSVal JSObject)
				   ,id		   :: String
				   }

googleMapEditlet :: GoogleMap -> Editlet GoogleMap GoogleMapDiff
googleMapEditlet g = {Editlet
				|value		= g
				,html		= \id -> DivTag [IdAttr (mapdomid id), StyleAttr "width:100%; height:100%"] []
				,updateUI   = onUpdate
				,handlers	= \_ -> []
				,genDiff	= genDiff
				,appDiff	= appDiff
				}
where
	mapdomid cid = "map_place_holder_" +++ cid
		
    onScriptLoad cid _ map _ world
	    # world = setDomAttr (mapdomid cid) "innerHTML" (toJSVal "<div id=\"map_canvas\" style=\"width:100%; height:100%\"/>") world
	    # (mapdiv, world) = getDomElement "map_canvas" world
	    # (mapTypeId, world) = findObject ("google.maps.MapTypeId." +++ toString (map.perspective.GoogleMapPerspective.type)) world
	    # (center, world) = jsNewObject "google.maps.LatLng"
	    				[toJSArg map.perspective.GoogleMapPerspective.center.lat
	    				,toJSArg map.perspective.GoogleMapPerspective.center.lng] world

	    # (mapobj, world) = jsNewObject "google.maps.Map"
	    				[toJSArg mapdiv
			    		,toJSArg {MapOptions
			    				 |zoom 				= map.perspective.GoogleMapPerspective.zoom
			    				 ,center 			= center
			    				 ,mapTypeId 		= mapTypeId
			    				 ,mapTypeControl	= map.settings.GoogleMapSettings.mapTypeControl
			    				 ,panControl		= map.settings.GoogleMapSettings.panControl
								 ,zoomControl		= map.settings.GoogleMapSettings.zoomControl
								 ,streetViewControl	= map.settings.GoogleMapSettings.streetViewControl
								 ,scaleControl		= map.settings.GoogleMapSettings.scaleControl
								 ,scrollwheel		= map.settings.GoogleMapSettings.scrollwheel
								 ,draggable			= map.settings.GoogleMapSettings.draggable
								 }
						] world

	    # (mapevent, world) = findObject "google.maps.event" world
		# (_, world) = callObjectMethod "addListener" [toJSArg mapobj, toJSArg "dragend", toJSArg (onChange "dragend")] mapevent world
		# (_, world) = callObjectMethod "addListener" [toJSArg mapobj, toJSArg "maptypeid_changed", toJSArg (onChange "maptype")] mapevent world
		# (_, world) = callObjectMethod "addListener" [toJSArg mapobj, toJSArg "zoom_changed", toJSArg (onChange "zoom")] mapevent world
		# (_, world) = callObjectMethod "addListener" [toJSArg mapobj, toJSArg "click", toJSArg onClick] mapevent world	

		# (markerMap, world) = jsNewMap world
		# world = foldl (putOnMarker mapobj markerMap) world map.GoogleMap.markers

		= (map, Just {mapobj = mapobj, nextMarkerId = 1, markerMap = markerMap}, world)
	where
		onChange t = createEditletEventHandler (updatePerspective t) cid
		onClick = createEditletEventHandler addMarker cid
		putOnMarker mapobj markerMap world markrec = createMarker cid mapobj markerMap markrec world

	onUpdate id Nothing map Nothing world
		# (mapsobj, world) = findObject "google.maps" world
		| jsIsUndefined mapsobj
		= (map, Nothing, loadMapsAPI id undef world)
		= onScriptLoad id undef map Nothing world
	
	// TODO
	onUpdate id (Just newmap) map st world = (map, st, world)	
	
	loadMapsAPI id _ world	
		# world = jsSetObjectAttr "gmapscallback" (createEditletEventHandler onScriptLoad id) jsWindow world
		= addJSFromUrl "http://maps.googleapis.com/maps/api/js?sensor=false&callback=gmapscallback"
				Nothing world

	updatePerspective "zoom" _ event map st=:(Just {mapobj}) world 
		# (zoom, world) = callObjectMethod "getZoom" [] mapobj world
		= ({map & perspective={GoogleMapPerspective | map.perspective & zoom = jsValToInt zoom}}, st, world)

	updatePerspective "maptype" _ event map st=:(Just {mapobj}) world 
		# (maptypeid, world) = callObjectMethod "getMapTypeId" [] mapobj world
		= ({map & perspective={GoogleMapPerspective | map.perspective & type = fromString (toUpperCase (jsValToString maptypeid))}}, st, world)
		
	updatePerspective "dragend" _ event map st=:(Just {mapobj}) world 
		# (center, world) 	  	= callObjectMethod "getCenter" [] mapobj world
		# ((lat, lng), world) 	= getPos center world
		= ({map & perspective={GoogleMapPerspective | map.perspective & center = {lat = lat, lng = lng}}}, st, world)
			
	addMarker cid event map=:{GoogleMap|markers} (Just st=:{mapobj,nextMarkerId,markerMap}) world 
		# (latlng, world)     = jsGetObjectAttr "latLng" event world
		# ((lat, lng), world) = getPos latlng world
		
		# markrec 	= createMarkerRecord markerId lat lng Nothing
		# markers 	= [markrec: markers]
		# world 	= createMarker cid mapobj markerMap markrec world
						
		= ({GoogleMap|map&markers=markers}, Just {st&nextMarkerId=nextMarkerId+1}, world)
	where
		markerId = cid +++ "_" +++ toString nextMarkerId

	getPos obj world
		# (lat, world) = callObjectMethod "lat" [] obj world
		# (lng, world) = callObjectMethod "lng" [] obj world
		= ((jsValToReal lat,jsValToReal lng), world)

	createMarkerRecord markerId lat lng mbTitle =
		{  GoogleMapMarker 
		 | markerId		= markerId
		 , position     = {GoogleMapPosition | lat = lat, lng = lng}
		 , title 		= mbTitle
		 , icon 		= Nothing
		 , infoWindow 	= Nothing
		 , draggable	= True
		 , selected 	= False}
	
	createMarker cid mapobj markerMap {GoogleMapMarker|markerId,position,title,draggable,icon} world
	    # (latlng, world) = jsNewObject "google.maps.LatLng"
	    				[toJSArg position.lat
	    				,toJSArg position.lng] world	
	
		# (mbIconObj, world) = case icon of
					Nothing = (Nothing, world)
					(Just (GoogleMapSimpleIcon name)) = (Just (toJSVal ("/icons/"+++name)), world)
					(Just (GoogleMapComplexIcon prop))
						# (size, world) = jsNewObject "google.maps.Size"
								[toJSArg (fst prop.GoogleMapComplexIcon.size), toJSArg (snd prop.GoogleMapComplexIcon.size)] world

						# (origin, world) = jsNewObject "google.maps.Point"
								[toJSArg (fst prop.origin), toJSArg (snd prop.origin)] world

						# (anchor, world) = jsNewObject "google.maps.Point"
								[toJSArg (fst prop.anchor), toJSArg (snd prop.anchor)] world						

						# (iconObj, world) = jsNewObject "google.maps.MarkerImage"
								[toJSArg ("/icons/"+++prop.image)
								,toJSArg size
								,toJSArg origin
								,toJSArg anchor] world
								
						= (Just iconObj, world)
		
		# (marker, world)
				= jsNewObject "google.maps.Marker"
						[toJSArg {MarkerOptions
								 | map = mapobj
								 , position = latlng
								 , title = maybe "" id title
								 , draggable = draggable
								 , icon = mbIconObj
								 , id = markerId}]
						world
	
    	# (mapevent, world) = findObject "google.maps.event" world
		# (_, world)     	= callObjectMethod "addListener" [toJSArg marker, toJSArg "dragend", toJSArg onDrag] mapevent world
		
		= jsPut markerId marker markerMap world
	where
		onDrag = createEditletEventHandler onDragWP cid	
	
		onDragWP cid event gmap=:{GoogleMap|markers} mbSt world
			# (latlng, world)      = jsGetObjectAttr "latLng" event world
			# ((lat, lng), world)  = getPos latlng world
			# markers      		   = updateWP lat lng markers
			= ({GoogleMap|gmap&markers=markers}, mbSt, world)
		where
	        updateWP nlat nlng markers = map ud markers 
	        where 
	        	ud m | m.GoogleMapMarker.markerId == markerId 
    	    			= {GoogleMapMarker|m&position={GoogleMapPosition | lat = nlat, lng = nlng}}
						= m	
	
    //TODO
	genDiff :: GoogleMap GoogleMap -> Maybe GoogleMapDiff
	genDiff g1 g2 = Just g2

	appDiff :: GoogleMapDiff GoogleMap -> GoogleMap
	appDiff d g = d

//--------------------------------------------------------------------------------------------------

instance toString GoogleMapType
where
	toString ROADMAP = "ROADMAP"
	toString SATELLITE = "SATELLITE"
	toString HYBRID = "HYBRID"
	toString TERRAIN = "TERRAIN"

instance fromString GoogleMapType
where 
	fromString "ROADMAP" = ROADMAP
	fromString "SATELLITE" = SATELLITE
	fromString "HYBRID" = HYBRID
	fromString "TERRAIN" = TERRAIN			

gVisualizeText{|GoogleMapPosition|} _  {GoogleMapPosition|lat,lng} = [toString lat + " " + toString lng]

gEditor{|GoogleMap|} dp vv=:(val,mask,ver) meta vst
    = gEditor{|*|} dp (googleMapEditlet val,mask,ver) meta vst

gUpdate{|GoogleMap|} dp upd (val,mask)
    # ({Editlet|value},mask) = gUpdate{|*|} dp upd (googleMapEditlet val,mask)
    = (value,mask)

//derive gUpdate GoogleMap 
gVerify{|GoogleMap|} _ mv = alwaysValid mv

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
derive gEditor                     GoogleMapSettings, GoogleMapPerspective, GoogleMapPosition, GoogleMapMarker, GoogleMapType, GoogleMapIcon, GoogleMapComplexIcon
derive gEditMeta		GoogleMap, GoogleMapSettings, GoogleMapPerspective, GoogleMapPosition, GoogleMapMarker, GoogleMapType, GoogleMapIcon, GoogleMapComplexIcon
derive gUpdate			GoogleMapSettings, GoogleMapPerspective, GoogleMapPosition, GoogleMapMarker, GoogleMapType, GoogleMapIcon, GoogleMapComplexIcon
derive gVerify			GoogleMapSettings, GoogleMapPerspective, GoogleMapPosition, GoogleMapMarker, GoogleMapType, GoogleMapIcon, GoogleMapComplexIcon

