implementation module iTasks.API.Extensions.GIS.GoogleMap

import iTasks
import iTasks.API.Core.Client.Editlet
import iTasks.API.Core.Client.Interface
import iTasks.API.Core.Client.Map

import Data.Functor, Text, StdMisc

from Data.Map import newMap

:: GoogleMapDiff
    = SetSettings GoogleMapSettings
    | SetPerspective GoogleMapPerspective
    | AddMarkers [GoogleMapMarker]
    | UpdateMarkers [GoogleMapMarker]
    | RemoveMarkers [String]


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

googleMapEditlet :: GoogleMap -> Editlet GoogleMap [GoogleMapDiff]
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
    mapcanvasid cid = "map_canvas_" +++ cid
    onScriptLoad cid _ map _ world
	    # world = setDomAttr (mapdomid cid) "innerHTML" (toJSVal ("<div id=\""+++mapcanvasid cid +++"\" style=\"width:100%; height:100%\"/>")) world
	    # (mapdiv, world) = getDomElement (mapcanvasid cid) world
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

        # (editlets,world)  = findObject "itwc.global.controller.editlets" world
        # (cmp,world)       = jsGetObjectAttr cid editlets world
        # (_,world)         = callObjectMethod "addManagedListener" [toJSArg cmp,toJSArg "afterlayout",toJSArg onAfterComponentLayout,toJSArg cmp] cmp world
        //Place initial markers
		# (markerMap, world) = jsNewMap world
		# world             = foldl (putOnMarker mapobj markerMap) world map.GoogleMap.markers
		= (map, Just {mapobj = mapobj, nextMarkerId = 1, markerMap = markerMap}, world)
	where
		onChange t = createEditletEventHandler (updatePerspective t) cid
		onClick = createEditletEventHandler addMarker cid
        onAfterComponentLayout = createEditletEventHandler resizeMap cid
		putOnMarker mapobj markerMap world markrec = createMarker cid mapobj markerMap markrec world

	onUpdate id Nothing map Nothing world
		# (mapsobj, world) = findObject "google.maps" world
		| jsIsUndefined mapsobj
		    = (map, Nothing, loadMapsAPI id undef world)
		= onScriptLoad id undef map Nothing world
	onUpdate id (Just [SetPerspective {GoogleMapPerspective|type,center,zoom}:updates]) map st=:(Just {mapobj}) world //Update the map perspective
        //Update type
	    # (mapTypeId, world)= findObject ("google.maps.MapTypeId." +++ toString type) world
        # (_, world)        = callObjectMethod "setMapTypeId" [toJSArg mapTypeId] mapobj world
        //Update center
        # (latlng, world)   = jsNewObject "google.maps.LatLng" [toJSArg center.lat,toJSArg center.lng] world	
        # (_, world)        = callObjectMethod "setCenter" [toJSArg latlng] mapobj world
        //Update zoom
        # (_, world)        = callObjectMethod "setZoom" [toJSArg zoom] mapobj world
        = onUpdate id (Just updates) map st world
    onUpdate cid (Just [AddMarkers markers:updates]) map st=:(Just {mapobj,markerMap}) world
        //TODO: don't update instead of create existing markers but raise exception, this should not happen (but currently does)
        # world = foldl (\w m -> updateMarker cid mapobj markerMap m w) world markers
        = onUpdate cid (Just updates) map st world
    onUpdate cid (Just [RemoveMarkers markers:updates]) map st=:(Just {markerMap}) world
	    # world = foldl (\w m -> removeMarker markerMap m w) world markers
        = onUpdate cid (Just updates) map st world
    onUpdate cid (Just [UpdateMarkers markers:updates]) map st=:(Just {mapobj,markerMap}) world
        //Simply remove and a marker
        # world = foldl (\w m -> updateMarker cid mapobj markerMap m w) world markers
        = onUpdate cid (Just updates) map st world
    onUpdate id (Just []) map st world
        = (map, st, world)	
	onUpdate id diff map st world //Catchall
        = (map, st, world)

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

    resizeMap cid event map=:{GoogleMap|perspective={GoogleMapPerspective|center}} (Just st=:{mapobj}) world
        //Resize map
        # (mapevent, world) = findObject "google.maps.event" world
		# (_, world)     	= callObjectMethod "trigger" [toJSArg mapobj, toJSArg "resize"] mapevent world
        //Correct center
        # (latlng, world)   = jsNewObject "google.maps.LatLng" [toJSArg center.lat,toJSArg center.lng] world	
        # (_, world)        = callObjectMethod "setCenter" [toJSArg latlng] mapobj world
        = (map,Just st, world)

	getPos obj world
		# (lat, world) = callObjectMethod "lat" [] obj world
		# (lng, world) = callObjectMethod "lng" [] obj world
		= ((jsValToReal lat,jsValToReal lng), world)

	createMarkerRecord markerId lat lng mbTitle =
	    { GoogleMapMarker
		| markerId	    = markerId
		, position      = {GoogleMapPosition | lat = lat, lng = lng}
		, title 		= mbTitle
		, icon 		    = Nothing
		, infoWindow    = Nothing
		, draggable     = True
		, selected      = False}
	
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
		# (_, world)     	= callObjectMethod "addListener" [toJSArg marker, toJSArg "click", toJSArg onClick] mapevent world
		# (_, world)     	= callObjectMethod "addListener" [toJSArg marker, toJSArg "dragend", toJSArg onDrag] mapevent world
		= jsPut markerId marker markerMap world
	where
        onClick = createEditletEventHandler onMarkerClick cid
        onMarkerClick cid event gmap=:{GoogleMap|markers} mbSt world
            //Toggle selection
            # markers = [if (m.GoogleMapMarker.markerId == markerId) {GoogleMapMarker|m & selected = not m.selected} m \\ m <- markers]
            = ({GoogleMap|gmap&markers=markers}, mbSt, world)

		onDrag = createEditletEventHandler onMarkerDrag cid	
		onMarkerDrag cid event gmap=:{GoogleMap|markers} mbSt world
			# (latlng, world)      = jsGetObjectAttr "latLng" event world
			# ((lat, lng), world)  = getPos latlng world
            # markers = [if (m.GoogleMapMarker.markerId == markerId) {GoogleMapMarker|m & position= {GoogleMapPosition | lat = lat, lng = lng}} m \\ m <- markers]
			= ({GoogleMap|gmap&markers=markers}, mbSt, world)

    removeMarker markerMap markerId world
        # (mbMarker,world) = jsGet markerId markerMap world
        = case mbMarker of
            Just marker
                # (_, world)    = callObjectMethod "setMap" [toJSArg jsNull] marker world
                # world         = jsDel markerId markerMap world
                = world
            Nothing         = world
	
	updateMarker cid mapobj markerMap marker=:{GoogleMapMarker|markerId,position,title,draggable,icon} world
        # world = removeMarker markerMap markerId world
        = createMarker cid mapobj markerMap marker world

	genDiff :: GoogleMap GoogleMap -> Maybe [GoogleMapDiff]
	genDiff g1 g2 = case settingsDiff ++ perspectiveDiff ++ remMarkersDiff ++ addMarkersDiff ++ updMarkersDiff of
        []      = Nothing
        diffs   = Just diffs
    where
        settingsDiff    = if (g1.GoogleMap.settings === g2.GoogleMap.settings) [] [SetSettings g2.GoogleMap.settings]
        perspectiveDiff = if (g1.GoogleMap.perspective === g2.GoogleMap.perspective) [] [SetPerspective g2.GoogleMap.perspective]
        remMarkersDiff = case [markerId \\ markerId <-oldMarkerIds | not (isMember markerId newMarkerIds)] of
            []          = []
            markerIds   = [RemoveMarkers markerIds]
        addMarkersDiff = case [marker \\ marker=:{GoogleMapMarker|markerId} <- g2.GoogleMap.markers | not (isMember markerId oldMarkerIds)] of
            []          = []
            markers     = [AddMarkers markers]
        updMarkersDiff = case [marker \\ marker=:{GoogleMapMarker|markerId} <- g2.GoogleMap.markers | isUpdated marker] of
            []          = []
            markers     = [UpdateMarkers markers]
        where
            isUpdated marker = not (isEmpty [m \\ m <- g1.GoogleMap.markers | m.GoogleMapMarker.markerId == marker.GoogleMapMarker.markerId && m =!= marker])

        oldMarkerIds = [markerId \\ {GoogleMapMarker|markerId} <- g1.GoogleMap.markers]
        newMarkerIds = [markerId \\ {GoogleMapMarker|markerId} <- g2.GoogleMap.markers]

	appDiff :: [GoogleMapDiff] GoogleMap -> GoogleMap
	appDiff d g = foldl app g d
    where
        app g (SetSettings settings)        = {GoogleMap|g & settings = settings}
        app g (SetPerspective perspective)  = {GoogleMap|g & perspective = perspective}
        app g (AddMarkers m)                = {GoogleMap|g & markers = g.GoogleMap.markers ++ m}
        app g (UpdateMarkers m)             = {GoogleMap|g & markers = foldl upd g.GoogleMap.markers m}
        where
            upd markers updated = [if (m.GoogleMapMarker.markerId == updated.GoogleMapMarker.markerId) updated m \\ m <- markers]
        app g (RemoveMarkers m)             = {GoogleMap|g & markers = [marker \\ marker <- g.GoogleMap.markers | not (isMember marker.GoogleMapMarker.markerId m)]}

//--------------------------------------------------------------------------------------------------
instance toString GoogleMapType
where
	toString ROADMAP    = "ROADMAP"
	toString SATELLITE  = "SATELLITE"
	toString HYBRID     = "HYBRID"
	toString TERRAIN    = "TERRAIN"

instance fromString GoogleMapType
where 
	fromString "ROADMAP"    = ROADMAP
	fromString "SATELLITE"  = SATELLITE
	fromString "HYBRID"     = HYBRID
	fromString "TERRAIN"    = TERRAIN			

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

derive JSONEncode		GoogleMap, GoogleMapSettings, GoogleMapPerspective, GoogleMapPosition, GoogleMapMarker, GoogleMapType, GoogleMapIcon, GoogleMapComplexIcon,GoogleMapDiff
derive JSONDecode		GoogleMap, GoogleMapSettings, GoogleMapPerspective, GoogleMapPosition, GoogleMapMarker, GoogleMapType, GoogleMapIcon, GoogleMapComplexIcon,GoogleMapDiff
derive gDefault			GoogleMap, GoogleMapPosition, GoogleMapMarker, GoogleMapType, GoogleMapIcon, GoogleMapComplexIcon,GoogleMapDiff
derive gEq				GoogleMap, GoogleMapSettings, GoogleMapPerspective, GoogleMapPosition, GoogleMapMarker, GoogleMapType, GoogleMapIcon, GoogleMapComplexIcon,GoogleMapDiff
derive gVisualizeText	GoogleMap, GoogleMapSettings, GoogleMapPerspective, GoogleMapMarker, GoogleMapType, GoogleMapIcon, GoogleMapComplexIcon,GoogleMapDiff
derive gEditor                     GoogleMapSettings, GoogleMapPerspective, GoogleMapPosition, GoogleMapMarker, GoogleMapType, GoogleMapIcon, GoogleMapComplexIcon,GoogleMapDiff
derive gEditMeta		GoogleMap, GoogleMapSettings, GoogleMapPerspective, GoogleMapPosition, GoogleMapMarker, GoogleMapType, GoogleMapIcon, GoogleMapComplexIcon,GoogleMapDiff
derive gUpdate			GoogleMapSettings, GoogleMapPerspective, GoogleMapPosition, GoogleMapMarker, GoogleMapType, GoogleMapIcon, GoogleMapComplexIcon,GoogleMapDiff
derive gVerify			GoogleMapSettings, GoogleMapPerspective, GoogleMapPosition, GoogleMapMarker, GoogleMapType, GoogleMapIcon, GoogleMapComplexIcon,GoogleMapDiff

