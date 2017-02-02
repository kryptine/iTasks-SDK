implementation module iTasks.API.Extensions.GIS.GoogleMap

import iTasks
import iTasks.UI.Definition, iTasks.UI.JS.Interface, iTasks.UI.Editor
import iTasks.UI.JS.Map

import Data.Functor, Text, StdMisc

import qualified Data.Map as DM
from StdArray import class Array(uselect), instance Array {} a

GOOGLEMAP_JS = "http://maps.googleapis.com/maps/api/js?callback=googleMapsLoaded"

:: JSGM = JSGM

:: GoogleMapDiff
    = SetSettings GoogleMapSettings
    | SetPerspective GoogleMapPerspective
    | AddMarkers [GoogleMapMarker]
    | UpdateMarkers [GoogleMapMarker]
    | RemoveMarkers [String]

//--------------------------------------------------------------------------------------------------

:: GoogleMapState = {mapobj       :: JSObj JSGM
					,nextMarkerId :: Int
					,markerMap	  :: JSMap String (JSObj GoogleMapMarker)
					}

:: GoogleMapClient = {val	        :: GoogleMap
					 ,mbSt          :: Maybe GoogleMapState
                     ,libsAdded     :: Bool
					 }

// Parameter object for creating google.maps.Map
:: MapOptions = {center    			:: JSObj JSGM
          		,zoom      			:: Int
          		,mapTypeId 			:: JSObj JSGM
				,mapTypeControl		:: Bool
				,panControl			:: Bool
				,zoomControl		:: Bool
				,streetViewControl	:: Bool
				,scaleControl		:: Bool
				,scrollwheel		:: Bool
				,draggable			:: Bool
        		}

// Parameter object for creating google.maps.Marker
:: MarkerOptions = {map        :: JSObj JSGM
				   ,position   :: JSObj JSGM
				   ,title      :: String
				   ,draggable  :: Bool
				   ,icon       :: Maybe (JSObj JSGM)
				   ,id		   :: String
				   }

derive class iTask GoogleMapClient, GoogleMapState, JSGM

googleMapEditlet :: Editlet GoogleMap
googleMapEditlet
    = { Editlet
      | genUI  = genUI
      , initUI = initUI 
      , onEdit = onEdit
      , onRefresh = onRefresh
      }
where
	genUI dp val world
		# attr = 'DM'.unions [sizeAttr (ExactSize 500) (ExactSize 200), valueAttr (toJSON val)]
		= (Ok (uia UIComponent attr,newFieldMask),world)

	initUI me world
		# (jsInitDOM, world)   = jsWrapFun (initDOM me) world
		# (gmaps_loaded,world) = findObject "googlemaps_loaded" world
		| jsIsUndefined gmaps_loaded
			//Check if another editlet has already loaded the javascript
			# (gmaps_callbacks,world) = findObject "googlemaps_callbacks" world
			| jsIsUndefined gmaps_callbacks
				//Create array of callback functions
				# (gmaps_callbacks,world)      = newJSArray world
				# (_,world)                    = jsArrayPush jsInitDOM gmaps_callbacks world
				# world                        = (jsWindow .# "googlemaps_callbacks" .= gmaps_callbacks) world
				//Prepare callback functions
				# (jsGoogleMapsCallback,world) = jsWrapFun googleMapsLoaded world
				# world                        = (jsWindow .# "googleMapsLoaded" .= jsGoogleMapsCallback) world
				//Load Google Maps library
				# world                        = addJSFromUrl GOOGLEMAP_JS Nothing world
				= world
			| otherwise
				//Just add a callback to the existing set of callbacks
				# (_,world) = jsArrayPush jsInitDOM gmaps_callbacks world
				= world
		| otherwise
			# world = (me .# "initDOMEl" .= jsInitDOM) world
			= world

	googleMapsLoaded args world
		# world = (jsWindow .# "googlemaps_loaded" .= (toJSVal True)) world
		# (callbacks,world)  = .? (jsWindow .# "googlemaps_callbacks") world
		# (callbacks, world) = fromJSArray callbacks id world
		//Call all the callbacks in the array
		# world = foldl (\w cb -> snd (jsApply cb jsWindow [] w)) world callbacks 
		= (jsNull, world)

	initDOM me args world
		//Create the parameter object
		# (options,world)    = jsEmptyObject world
		# (mapType,world)    = .? (me .# "value.perspective.type" .# 0) world
		# (mapTypeId,world)  = .? (jsWindow .# "google.maps.MapTypeId" .# (jsValToString mapType)) world
		# world              = (options .# "mapTypeId" .= mapTypeId) world
		# (lat,world)        = .? (me .# "value.perspective.center.lat") world
		# (lng,world)        = .? (me .# "value.perspective.center.lng") world
	    # (center, world)    = jsNewObject "google.maps.LatLng" [toJSArg lat,toJSArg lng] world
		# world              = (options .# "center" .= center) world
		# world              = copy (me .# "value.perspective.zoom") (options .# "zoom") world
		# world              = copy (me .# "value.settings.mapTypeControl") (options .# "mapTypeControl") world
		# world              = copy (me .# "value.settings.panControl") (options .# "panControl") world
		# world              = copy (me .# "value.settings.zoomControl") (options .# "zoomControl") world
		# world              = copy (me .# "value.settings.streetViewControl") (options .# "streetViewControl") world
		# world              = copy (me .# "value.settings.scaleControl") (options .# "scaleControl") world
		# world              = copy (me .# "value.settings.scrollwheel") (options .# "scrollwheel") world
		# world              = copy (me .# "value.settings.draggable") (options .# "draggable") world
		//Create the map object
		# (domEl,world)       = .? (me .# "domEl") world
	    # (mapobj, world)     = jsNewObject "google.maps.Map" [toJSArg domEl,toJSArg options] world
		# world               = (me .# "map" .= mapobj) world
		//Attach event handlers
		# (jsOnShow,world)    = jsWrapFun (onShow me) world
		# world               = (me .# "onShow" .= jsOnShow) world
		# (jsOnDragEnd,world) = jsWrapFun (onDragEnd me) world
		# (_, world)          = (jsWindow .# "google.maps.event.addListener" .$ (mapobj, "dragend", jsOnDragEnd)) world
		# (jsOnMapTypeChanged,world) = jsWrapFun (onMapTypeChanged me) world
		# (_, world)          = (jsWindow .# "google.maps.event.addListener" .$ (mapobj, "maptypeid_changed", jsOnMapTypeChanged)) world
		# (jsOnZoomChanged,world) = jsWrapFun (onZoomChanged me) world
		# (_, world)          = (jsWindow .# "google.maps.event.addListener" .$ (mapobj, "zoom_changed", jsOnZoomChanged)) world
		# (jsOnMapClick,world) = jsWrapFun (onMapClick me) world
		# (_, world)          = (jsWindow .# "google.maps.event.addListener" .$ (mapobj, "click", jsOnMapClick)) world
		//Create initial markers
		= (jsNull,world)

	copy src tgt world //Util
		# (tmp,world) = .? src world
		= (tgt .= tmp) world
	
	onShow me args world
	    //Trigger a resize event for the map
		# (map,world) = .? (me .# "map") world
        # (_, world)  = (jsWindow .# "google.maps.event.trigger" .$ (map, "resize")) world
		//Correct center
		# (lat,world)        = .? (me .# "value.perspective.center.lat") world
		# (lng,world)        = .? (me .# "value.perspective.center.lng") world
	    # (center, world)    = jsNewObject "google.maps.LatLng" [toJSArg lat,toJSArg lng] world
        # (_, world)         = (map .# "setCenter" .$ center) world
		= (jsNull,world)

	onDragEnd me args world
		= (jsNull,jsTrace "onDragEnd" world)

	onMapTypeChanged me args world
		= (jsNull,jsTrace "onMapTypeChange" world)

	onZoomChanged me args world
		= (jsNull,jsTrace "onZoomChange" world)

	onMapClick me args world
		# (latlng, world)       = .? (toJSVal (args !! 0) .# "latLng") world
		# ((lat, lng), world)   = getPos latlng world
		# (marker,world) 		= jsEmptyObject world
		# (latlng, world) 		= jsEmptyObject world
		# world 				= (latlng .# "lat" .= lat) world
		# world 				= (latlng .# "lng" .= lng) world
		# world 				= (marker .# "latlng" .= latlng) world
		# (_,world) 			= addMarker me marker world
		= (jsNull,jsTrace latlng world)

	addMarker me marker world
		# (options, world)   = jsEmptyObject world
		# world              = copy (me .# "map") (options .# "map") world
		# (lat,world)        = .? (marker .# "latlng.lat") world
		# (lng,world)        = .? (marker .# "latlng.lng") world
	    # (position, world)  = jsNewObject "google.maps.LatLng" [toJSArg lat,toJSArg lng] world
		# world              = (options .# "position" .= position) world
		# (marker, world)    = jsNewObject "google.maps.Marker" [toJSArg options] world
		= (marker,jsTrace marker world)

	//==================== DEPRECATED =====================================================//
	mapdomid cid    = "map_place_holder_" +++ cid
    mapcanvasid cid = "map_canvas_" +++ cid
/*
    initEditlet mkEventHandler cid _ clval=:{val} world
	    # world = (getElementById (mapdomid cid).# "innerHTML" .= ("<div id=\""+++mapcanvasid cid +++"\" style=\"width: 100%; height: 100%;\"/>")) world
	    # (mapdiv, world) = .? (getElementById (mapcanvasid cid)) world
	    # (mapTypeId, world) = findObject ("google.maps.MapTypeId." +++ toString (val.perspective.GoogleMapPerspective.type)) world
	    # (center, world) = jsNewObject "google.maps.LatLng"
	    				[toJSArg val.perspective.GoogleMapPerspective.center.lat
	    				,toJSArg val.perspective.GoogleMapPerspective.center.lng] world
        # options = toJSArg {MapOptions
			    				 |zoom 				= val.perspective.GoogleMapPerspective.zoom
			    				 ,center 			= center
			    				 ,mapTypeId 		= mapTypeId
			    				 ,mapTypeControl	= val.settings.GoogleMapSettings.mapTypeControl
			    				 ,panControl		= val.settings.GoogleMapSettings.panControl
								 ,zoomControl		= val.settings.GoogleMapSettings.zoomControl
								 ,streetViewControl	= val.settings.GoogleMapSettings.streetViewControl
								 ,scaleControl		= val.settings.GoogleMapSettings.scaleControl
								 ,scrollwheel		= val.settings.GoogleMapSettings.scrollwheel
								 ,draggable			= val.settings.GoogleMapSettings.draggable
								 }
	    # (mapobj, world) = jsNewObject "google.maps.Map" [toJSArg mapdiv,options] world
	    # (mapevent, world) = findObject "google.maps.event" world
		# (_, world) = callObjectMethod "addListener" [toJSArg mapobj, toJSArg "dragend", toJSArg (onChange "dragend")] mapevent world
		# (_, world) = callObjectMethod "addListener" [toJSArg mapobj, toJSArg "maptypeid_changed", toJSArg (onChange "maptype")] mapevent world
		# (_, world) = callObjectMethod "addListener" [toJSArg mapobj, toJSArg "zoom_changed", toJSArg (onChange "zoom")] mapevent world
		# (_, world) = callObjectMethod "addListener" [toJSArg mapobj, toJSArg "click", toJSArg onClick] mapevent world	
        # (editlets,world)  = findObject "itwc.controller.editlets" world
        # (cmp,world)       = jsGetObjectAttr cid editlets world
        # world             = jsSetObjectAttr "afterShow" (toJSVal afterShow) cmp world
        //Place initial markers
		# (markerMap, world) = jsNewMap world
		# world             = foldl (putOnMarker mapobj markerMap) world val.GoogleMap.markers
        = ({clval & mbSt = Just {mapobj = mapobj, nextMarkerId = 1, markerMap = markerMap}},NoDiff,world)
	where
		onChange t              = mkEventHandler (onUpdatePerspective t) cid
		onClick                 = mkEventHandler (addMarker` mkEventHandler) cid
        onAfterComponentLayout  = mkEventHandler resizeMap cid
        afterShow               = mkEventHandler resizeMap cid
		putOnMarker mapobj markerMap world markrec = createMarker mkEventHandler cid mapobj markerMap markrec world
*/

	//appDiffClt mkEventHandler cid diffs clval world
	//	= updateUI mkEventHandler cid diffs {clval & val = appDiff diffs clval.val} world
/*
	updateUI mkEventHandler cid [SetPerspective {GoogleMapPerspective|type,center,zoom}:updates] clval=:{mbSt=Just {mapobj}} world //Update the map perspective
        //Update type
	    # (mapTypeId, world)= findObject ("google.maps.MapTypeId." +++ toString type) world
        # (_, world)        = callObjectMethod "setMapTypeId" [toJSArg mapTypeId] mapobj world
        //Update center
        # (latlng, world)   = jsNewObject "google.maps.LatLng" [toJSArg center.lat,toJSArg center.lng] world	
        # (_, world)        = callObjectMethod "setCenter" [toJSArg latlng] mapobj world
        //Update zoom
        # (_, world)        = callObjectMethod "setZoom" [toJSArg zoom] mapobj world
        = updateUI mkEventHandler cid updates clval world

    updateUI mkEventHandler cid [AddMarkers markers:updates] clval=:{val,mbSt=Just {mapobj,markerMap}} world
        # world = foldl (\w m -> createMarker mkEventHandler cid mapobj markerMap m w) world markers
        = updateUI mkEventHandler cid updates clval world
    updateUI mkEventHandler cid [RemoveMarkers markers:updates] clval=:{mbSt=Just {markerMap}} world
	    # world = foldl (\w m -> removeMarker markerMap m w) world markers
        = updateUI mkEventHandler cid updates clval world
    updateUI mkEventHandler cid [UpdateMarkers markers:updates] clval=:{mbSt=Just {mapobj,markerMap}} world
        //Simply remove and add a marker
        # world = foldl (\w m -> updateMarker mkEventHandler cid mapobj markerMap m w) world markers
        = updateUI mkEventHandler cid updates clval world
    updateUI mkEventHandler id [] clval world
        = (clval, world)	
	updateUI mkEventHandler id diff clval world //Catchall
        = (clval, world)
*/
/*
	onUpdatePerspective "zoom" _ _ clval=:{val,mbSt=Just st=:{mapobj}} world
		# (zoom, world) = callObjectMethod "getZoom" [] mapobj world
		# perspective = {GoogleMapPerspective | val.perspective & zoom = jsValToInt zoom}
		= ({clval & val={val&perspective=perspective}}, Diff [SetPerspective perspective] ignoreConflict, world)
	onUpdatePerspective "maptype" _ _ clval=:{val,mbSt=Just {mapobj}} world
		# (maptypeid, world) = callObjectMethod "getMapTypeId" [] mapobj world
		# perspective = {GoogleMapPerspective | val.perspective & type = fromString (toUpperCase (jsValToString maptypeid))}
		= ({clval & val={val&perspective=perspective}}, Diff [SetPerspective perspective] ignoreConflict, world)
	onUpdatePerspective "dragend" _ _ clval=:{val,mbSt=Nothing} world
        # world = jsTrace "WRONG DRAGEND..." world
        = (clval, NoDiff, world)
	onUpdatePerspective "dragend" _ _ clval=:{val,mbSt=Just {mapobj}} world 
		# (center, world) 	  	= callObjectMethod "getCenter" [] mapobj world
		# ((lat, lng), world) 	= getPos center world
		# perspective 			= {GoogleMapPerspective | val.perspective & center = {lat = lat, lng = lng}}
		= ({clval & val={val&perspective=perspective}}, Diff [SetPerspective perspective] ignoreConflict, world)
    onUpdatePerspective e _ _ clval world
        # world = jsTrace "CATCHALL" world
        # world = jsTrace e world
        = (clval, NoDiff, world) //Catchall
*/			
/*
	addMarker` mkEventHandler cid {[0]=event} clval=:{val={markers}, mbSt=Just st=:{mapobj,nextMarkerId,markerMap}} world 
		# (latlng, world)     = jsGetObjectAttr "latLng" event world
		# ((lat, lng), world) = getPos latlng world
		# markrec 	= createMarkerRecord markerId lat lng Nothing
		# markers 	= [markrec: markers]
		# world 	= createMarker mkEventHandler cid mapobj markerMap markrec world
		= ({clval&val={clval.val&markers=markers}, mbSt=Just {st&nextMarkerId=nextMarkerId+1}}, Diff [AddMarkers [markrec]] ignoreConflict, world)
	where
		markerId = cid +++ "_" +++ toString nextMarkerId
*/
/*
    resizeMap cid event clval=:{val={perspective={GoogleMapPerspective|center}}, mbSt=Just {mapobj}} world
        //Resize map
        # (mapevent, world) = findObject "google.maps.event" world
		# (_, world)     	= callObjectMethod "trigger" [toJSArg mapobj, toJSArg "resize"] mapevent world
        //Correct center
        # (latlng, world)   = jsNewObject "google.maps.LatLng" [toJSArg center.lat,toJSArg center.lng] world	
        # (_, world)        = callObjectMethod "setCenter" [toJSArg latlng] mapobj world
        = (clval, NoDiff, world)
*/
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
/*	
	createMarker mkEventHandler cid mapobj markerMap {GoogleMapMarker|markerId,position,title,draggable,icon} world
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
*/
        //onClick = mkEventHandler onMarkerClick cid
/*
        onMarkerClick cid _ clval=:{val={markers}} world
            //Toggle selection
            # markers = [{GoogleMapMarker|m & selected = (m.GoogleMapMarker.markerId == markerId)} \\ m <- markers]
			= ({clval&val={clval.val&markers=markers}}, Diff [UpdateMarkers markers] ignoreConflict, world)
*/

		//onDrag = mkEventHandler onMarkerDrag cid	
/*
		onMarkerDrag cid {[0]=event} clval=:{val={markers}} world
			# (latlng, world)      = jsGetObjectAttr "latLng" event world
			# ((lat, lng), world)  = getPos latlng world
            # markers = [if (m.GoogleMapMarker.markerId == markerId) {GoogleMapMarker|m & position= {GoogleMapPosition | lat = lat, lng = lng}} m \\ m <- markers]
			= ({clval&val={clval.val&markers=markers}}, Diff [UpdateMarkers markers] ignoreConflict, world)
*/

    removeMarker markerMap markerId world
        # (mbMarker,world) = jsGet markerId markerMap world
        = case mbMarker of
            Just marker
                # (_, world)    = callObjectMethod "setMap" [toJSArg jsNull] marker world
                # world         = jsDel markerId markerMap world
                = world
            Nothing         = world
/*	
	updateMarker mkEventHandler cid mapobj markerMap marker=:{GoogleMapMarker|markerId,position,title,draggable,icon} world
        # world = removeMarker markerMap markerId world
        = createMarker mkEventHandler cid mapobj markerMap marker world
*/

    onScriptLoad args world
        # world                   = jsSetObjectAttr "googlemaps_loaded" (toJSVal True) jsWindow world
        # (gmaps_callbacks,world) = findObject "googlemaps_callbacks" world
        # (object,world)          = findObject "Object" world
        # (cids,world)            = callObjectMethod "keys" [toJSArg gmaps_callbacks] object world
        # (cids,world)            = fromJSArray cids jsValToString world
        # world                   = foldl (call gmaps_callbacks) world cids
        = (jsNull,world)
    where
        call callbacks world cid
            # (cb,world) = jsGetObjectAttr cid callbacks world
            # (_,world) = jsApply cb jsWindow [] world
            = world

	onRefresh _ g2 g1 mask vst = case settingsDiff ++ perspectiveDiff ++ remMarkersDiff ++ addMarkersDiff ++ updMarkersDiff of
        []      = (Ok (NoChange,mask),g2,vst)
        diffs   = (Ok (ChangeUI [SetAttribute "diff" (toJSON diffs)] [],mask),g2,vst)
    where
        settingsDiff    = if (g1.GoogleMap.settings === g2.GoogleMap.settings) [] [SetSettings g2.GoogleMap.settings]
        perspectiveDiff = if (g1.GoogleMap.perspective === g2.GoogleMap.perspective) [] [SetPerspective g2.GoogleMap.perspective]
        remMarkersDiff = case [markerId \\ markerId <-oldMarkerIds | not (isMember markerId newMarkerIds)] of
            []          = []
            markerIds   = [RemoveMarkers markerIds]
        addMarkersDiff = case [marker \\ marker=:{GoogleMapMarker|markerId} <- g2.GoogleMap.markers | not (isMember markerId oldMarkerIds)] of
            []          = []
            markers     = [AddMarkers markers]
        updMarkersDiff = case [marker \\ marker <- g2.GoogleMap.markers | isUpdated marker] of
            []          = []
            markers     = [UpdateMarkers markers]
        where
            isUpdated marker = not (isEmpty [m \\ m <- g1.GoogleMap.markers | m.GoogleMapMarker.markerId == marker.GoogleMapMarker.markerId && m =!= marker])

        oldMarkerIds = [markerId \\ {GoogleMapMarker|markerId} <- g1.GoogleMap.markers]
        newMarkerIds = [markerId \\ {GoogleMapMarker|markerId} <- g2.GoogleMap.markers]

	onEdit dp ([],d) g msk ust = case fromJSON d of
		Just diffs = (Ok (NoChange,msk),foldl app g diffs,ust)
		Nothing    = (Ok (NoChange,msk),g,ust)
    where
        app g (SetSettings settings)        = {GoogleMap|g & settings = settings}
        app g (SetPerspective perspective)  = {GoogleMap|g & perspective = perspective}
        app g (AddMarkers m)                = {GoogleMap|g & markers = g.GoogleMap.markers ++ m}
        app g (UpdateMarkers m)             = {GoogleMap|g & markers = foldl upd g.GoogleMap.markers m}
        where
            upd markers updated = [if (m.GoogleMapMarker.markerId == updated.GoogleMapMarker.markerId) updated m \\ m <- markers]
        app g (RemoveMarkers m)             = {GoogleMap|g & markers = [marker \\ marker <- g.GoogleMap.markers | not (isMember marker.GoogleMapMarker.markerId m)]}
        app g _ = g
	onEdit _ d g msk ust = (Ok (NoChange,msk),g,ust)

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

gText{|GoogleMapPosition|} _ (Just {GoogleMapPosition|lat,lng}) = [toString lat + " " + toString lng]
gText{|GoogleMapPosition|} _ _ = [""]

gEditor{|GoogleMap|} = fromEditlet googleMapEditlet

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
derive gText	        GoogleMap, GoogleMapSettings, GoogleMapPerspective, GoogleMapMarker, GoogleMapType, GoogleMapIcon, GoogleMapComplexIcon,GoogleMapDiff
derive gEditor                     GoogleMapSettings, GoogleMapPerspective, GoogleMapPosition, GoogleMapMarker, GoogleMapType, GoogleMapIcon, GoogleMapComplexIcon,GoogleMapDiff
