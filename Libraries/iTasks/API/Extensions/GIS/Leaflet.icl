implementation module iTasks.API.Extensions.GIS.Leaflet

import iTasks
import iTasks.UI.Definition, iTasks.UI.JS.Map, iTasks.UI.Editor
import StdMisc, Data.Tuple
import qualified Data.Map as DM

from StdArray import class Array(uselect), instance Array {} a

LEAFLET_JS :== "/leaflet-0.7.2/leaflet.js"
LEAFLET_CSS :== "/leaflet-0.7.2/leaflet.css"

:: IconOptions =
    { iconUrl   :: !String
    , iconSize  :: ![Int]
    }
:: MapOptions =
    { attributionControl    :: !Bool
    , zoomControl           :: !Bool
    }
:: CursorOptions =
    { color     :: !String
    , opacity   :: !Real
    , radius    :: !Int
    }

derive JSONEncode IconOptions

CURSOR_OPTIONS  :== {color = "#00f", opacity = 1.0, radius = 3}
MAP_OPTIONS     :== {attributionControl = False, zoomControl = True}

:: LeafletEdit
    //Perspective
    = LDSetZoom         !Int
    | LDSetCenter       !LeafletLatLng
    | LDSetCursor       !LeafletLatLng
    | LDSetBounds       !LeafletBounds
	//Updating markers 
	| LDSelectMarker    LeafletObjectID

openStreetMapTiles :: String
openStreetMapTiles = "http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png"
	
leafletEditor :: Editor LeafletMap
leafletEditor = {Editor|genUI = withClientSideInit initUI genUI, onEdit  = onEdit, onRefresh = onRefresh}
where
	genUI dp val=:{LeafletMap|perspective={center,zoom,cursor},tilesUrl,objects,icons} world
		# mapAttr = 'DM'.fromList
			[("zoom", JSONInt zoom)
			,("center", JSONArray [JSONReal center.LeafletLatLng.lat, JSONReal center.LeafletLatLng.lng])
			,("cursor", maybe JSONNull toJSON cursor)
			,("tilesUrl", maybe JSONNull JSONString tilesUrl)
			,("icons", JSONArray [toJSON (iconId,{IconOptions|iconUrl=iconUrl,iconSize=[w,h]}) \\ {iconId,iconUrl,iconSize=(w,h)} <- icons])
			]
		# attr = 'DM'.unions [mapAttr, sizeAttr (ExactSize 500) (ExactSize 150)]
		# children = map encodeUI objects
		= (Ok (uiac UIHtmlView attr children,newFieldMask), world)

	encodeUI (Marker o) = let (JSONObject attr) = toJSON o in uia UIData ('DM'.fromList [("type",JSONString "marker"):attr])
	encodeUI (Polyline o) = let (JSONObject attr) = toJSON o in uia UIData ('DM'.fromList [("type",JSONString "polyline"):attr])
	encodeUI (Polygon o) = let (JSONObject attr) = toJSON o in uia UIData ('DM'.fromList [("type",JSONString "polygon") : attr])

	initUI me world
		# (jsInitDOM,world) = jsWrapFun (initDOM me) world
		//Check if the leaflet library is loaded and either load it, 
		//and delay dom initialization or set the initDOM method to continue
		//as soon as the component's DOM element is available
        # (l, world) = findObject "L" world
        | jsIsUndefined l
            # world = addCSSFromUrl LEAFLET_CSS world
            # world = addJSFromUrl LEAFLET_JS (Just jsInitDOM) world
			= world
		| otherwise
			# world = (me .# "initDOMEl" .= jsInitDOM) world
			= world

	initDOM me args world
        # (l,world)         = findObject "L" world
		# (domEl,world) 	= .? (me .# "domEl") world
		//Create the map
		# (mapObj,world)    = (l .# "map" .$ (domEl,MAP_OPTIONS)) world
		# world 			= (me .# "map" .= mapObj) world
		//Set perspective
		# (center,world)    = .? (me .# "attributes.center") world
		# (zoom,world)      = .? (me .# "attributes.zoom") world
		# (cursor,world)    = .? (me .# "attributes.cursor") world
        # (_,world)         = (mapObj .# "setView" .$ (center,zoom)) world
		//Set initial cursor
        # world             = setMapCursor me mapObj cursor world
        //Add icons
		# (icons,world)     = .? (me .# "attributes.icons") world
		# world             = setMapIcons me mapObj icons world 
		//Create tile layer
		# (tilesUrl,world)    = .? (me .# "attributes.tilesUrl") world
		# world               = setMapTilesLayer me mapObj tilesUrl world 
		//Synchronize lat/lng bounds to server (they depend on the size of the map in the browser)
		# (taskId,world)    = .? (me .# "attributes.taskId") world
		# (editorId,world)  = .? (me .# "attributes.editorId") world
        # (bounds,world)    = getMapBounds mapObj world
		# (_,world)         = ((me .# "doEditEvent") .$ (taskId,editorId,[LDSetBounds bounds])) world
        //Add initial objects
		# (objects,world)   = .? (me .# "children") world
		# world             = createMapObjects me mapObj objects world
		//Add event handlers
		# (cb,world)       = jsWrapFun (\a w -> onResize me w) world	
		# world            = ((me .# "onResize") .= cb) world
		# (cb,world)       = jsWrapFun (\a w -> onShow me w) world	
		# world            = ((me .# "onShow") .= cb) world
		# (cb,world)       = jsWrapFun (\a w -> onAttributeChange me a w) world	
		# world            = ((me .# "onAttributeChange") .= cb) world
		# (cb,world)       = jsWrapFun (\a w -> onAfterChildInsert me a w) world	
		# world            = ((me .# "afterChildInsert") .= cb) world
		# (cb,world)       = jsWrapFun (\a w -> onBeforeChildRemove me a w) world	
		# world            = ((me .# "beforeChildRemove") .= cb) world
		# (cb,world)       = jsWrapFun (\a w -> onMapDragEnd me a w) world
        # (_,world)        = (mapObj .# "addEventListener" .$ ("dragend",cb)) world
		# (cb,world)       = jsWrapFun (\a w -> onMapZoomEnd me a w) world
        # (_,world)        = (mapObj .# "addEventListener" .$ ("zoomend",cb)) world
		# (cb,world)       = jsWrapFun (\a w -> onMapClick me a w) world
        # (_,world)        = (mapObj .# "addEventListener" .$ ("click",cb)) world
		= (jsNull,world)

	onResize me world
		# (mapObj,world) 	= .? (me .# "map") world
        # (_,world)         = (mapObj .# "invalidateSize" .$ ()) world
		= (jsNull,world)

	onShow me world
		# (mapObj,world) 	= .? (me .# "map") world
        # (_,world)         = (mapObj .# "invalidateSize" .$ ()) world
		= (jsNull,world)

	onMapDragEnd me args world
		# (taskId,world)    = .? (me .# "attributes.taskId") world
		# (editorId,world)  = .? (me .# "attributes.editorId") world
        # (mapObj,world)    = .? (toJSVal (args !! 0) .# "target") world
        # (center,world)    = getMapCenter mapObj world
        # (bounds,world)    = getMapBounds mapObj world
		# (_,world)         = ((me .# "doEditEvent") .$ (taskId,editorId,[LDSetCenter center,LDSetBounds bounds])) world
		= (jsNull,world)

	onMapZoomEnd me args world
		# (taskId,world)    = .? (me .# "attributes.taskId") world
		# (editorId,world)  = .? (me .# "attributes.editorId") world
        # (mapObj,world)    = .? (toJSVal (args !! 0) .# "target") world
        # (zoom,world)      = getMapZoom mapObj world
        # (bounds,world)    = getMapBounds mapObj world
		# (_,world)         = ((me .# "doEditEvent") .$ (taskId,editorId,[LDSetZoom zoom,LDSetBounds bounds])) world
		= (jsNull,world)

	onMapClick me args world
		# (taskId,world)    = .? (me .# "attributes.taskId") world
		# (editorId,world)  = .? (me .# "attributes.editorId") world
        # (mapObj,world)    = .? (toJSVal (args !! 0) .# "target") world
        # (clickPos,world)  = .? (toJSVal (args !! 0) .# "latlng") world
		# (cursor,world)    = toLatLng clickPos world 
		# (_,world)         = ((me .# "doEditEvent") .$ (taskId,editorId,[LDSetCursor cursor])) world
		//Update cursor position on the map
		# world             = setMapCursor me mapObj (toJSVal cursor) world
		= (jsNull,world)

	onMarkerClick me markerId args world
		# (taskId,world)    = .? (me .# "attributes.taskId") world
		# (editorId,world)  = .? (me .# "attributes.editorId") world
		# (_,world)         = ((me .# "doEditEvent") .$ (taskId,editorId,[LDSelectMarker markerId])) world
		= (jsNull,world)

	onAttributeChange me args world
		# (mapObj,world)    = .? (me .# "map") world
		= case jsArgToString (args !! 0) of
			"center"  = (jsNull,setMapCenter mapObj (args !! 1) world)
			"zoom"    = (jsNull,setMapZoom mapObj (args !! 1) world)
			"cursor"  = (jsNull,setMapCursor me mapObj (toJSVal (args !! 1)) world)
			_ 		  = (jsNull,world)
		
	onAfterChildInsert me args world
		# (l, world)      	= findObject "L" world
		# (mapObj,world)    = .? (me .# "map") world
		= (jsNull,createMapObject me mapObj l (toJSVal (args !! 1)) world)

	onBeforeChildRemove me args world
		# (layer,world)     = .? (toJSVal (args !! 1) .# "layer") world
		# (mapObj,world)    = .? (me .# "map") world
        # (_,world)         = (mapObj.# "removeLayer" .$ layer) world
		= (jsNull,world)

	//Map object access
	toLatLng obj world
		# (lat,world)     = .? (obj .# "lat") world
		# (lng,world)     = .? (obj .# "lng") world
		= ({LeafletLatLng|lat=jsValToReal lat,lng=jsValToReal lng}, world)

    getMapBounds mapObj env
        # (bounds,env)      = (mapObj .# "getBounds" .$ ()) env
        # (sw,env)          = (bounds .# "getSouthWest" .$ ()) env
        # (ne,env)          = (bounds .# "getNorthEast" .$ ()) env
		# (swpos,env)       = toLatLng sw env
		# (nepos,env)       = toLatLng ne env
        = ({southWest=swpos,northEast=nepos},env)

	getMapZoom mapObj world
		# (zoom,world) = (mapObj .# "getZoom" .$ ()) world
		= (jsValToInt zoom, world)

	setMapZoom mapObj zoom world
		# (_,world) = (mapObj .# "setZoom" .$ zoom) world
		= world

	getMapCenter mapObj world
        # (center,world)    = (mapObj .# "getCenter" .$ ()) world
        = toLatLng center world
	
	setMapCenter mapObj center world
		# (_,world) = (mapObj .# "panTo" .$ center) world
		= world

	setMapCursor me mapObj position world
		# (cursor,world) = .? (me .# "cursor") world
		| jsIsUndefined cursor
			| jsIsNull position //Nothing to do
				= world
			| otherwise
				//Create the cursor
				# (l, world)      = findObject "L" world
				# (cursor,world)  = (l .# "circleMarker" .$ (position, CURSOR_OPTIONS)) world
				# (_,world)       = (cursor .# "addTo" .$ mapObj) world
				# world           = ((me .# "cursor") .= cursor) world	
				= world
		| otherwise //Update the position
			| jsIsNull position
				//Destroy the cursor
				# (_,world)       = (mapObj .# "removeLayer" .$ cursor) world
				# world           = jsDeleteObjectAttr "cursor" me world
				= world
			| otherwise
        		# (_,world)       = (cursor .# "setLatLng" .$ position) world
				= world

	setMapTilesLayer me mapObj tilesUrl world 
		| jsIsNull tilesUrl = world
		# (l, world)      	= findObject "L" world
        # (layer,world)     = (l .# "tileLayer" .$ tilesUrl) world
        # (_,world)         = (layer .# "addTo" .$ mapObj) world
		= world

	setMapIcons me mapObj icons world 
		# (l, world)      	= findObject "L" world
		# (index,world) 	= jsEmptyObject world
		# world 			= ((me .# "icons") .= index) world
		= forall (createMapIcon me mapObj l index) icons world
	where	
		createMapIcon me mapObj l index def world
			# (iconId,world)   = .? (def .# 0) world
			# (iconSpec,world) = .? (def .# 1) world
        	# (icon,world)     = (l .# "icon" .$ iconSpec) world
			# world            = ((index .# jsValToString iconId) .= icon) world
			= world

	createMapObjects me mapObj objects world
		# (l, world)      	= findObject "L" world
		= forall (createMapObject me mapObj l) objects world

	createMapObject me mapObj l object world
		# (type,world)        = .? (object .# "attributes.type") world
		= case jsValToString type of
			"marker"   = createMarker me mapObj l object world
			"polyline" = createPolyline me mapObj l object world
			"polygon"  = createPolygon me mapObj l object world
			_ 		   = world

	createMarker me mapObj  l object world 
        # (options,world)     = jsEmptyObject world
		//Set title
		# (title,world)       = .? (object .# "attributes.title") world
		# world               = (options .# "title" .= title) world
		# world               = (options .# "alt" .= title) world
		//Optionally set icon
		# (iconId,world)      = .? (object .# "attributes.icon") world
		# (icons,world)       = .? (me .# "icons") world
		# world               = addIconOption iconId icons options world
		//Create marker
		# (position,world)    = .? (object .# "attributes.position") world
        # (layer,world)       = (l .# "marker" .$ (position,options) ) world
        # (_,world)           = (layer .# "addTo" .$ (toJSArg mapObj)) world
		# world               = (object .# "layer" .= layer) world
		//Set click handler
		# (markerId,world)    = .? (object .# "attributes.markerId") world
		# (cb,world)          = jsWrapFun (\a w -> onMarkerClick me (jsValToString markerId) a w) world
        # (_,world)           = (layer .# "addEventListener" .$ ("click",cb)) world
		= world	
	where
		addIconOption iconId icons options world
			| jsIsUndefined iconId = world
			# (icon,world) = .? (icons .# (jsValToString iconId)) world
			| jsIsUndefined icon = world
			# world = (options .# "icon" .= icon) world
			= world
	
	createPolyline me mapObj l object world 
		//Set options
        # (options,world)     = jsEmptyObject world
		# (stroke,world)      = .? (object .# "attributes.strokeWidth") world
		# (color,world)       = .? (object .# "attributes.strokeColor") world
		# world               = (options .# "stroke" .= stroke) world
		# world               = (options .# "color" .= color) world
		# (points,world)      = .? (object .# "attributes.points") world
        # (layer,world)       = (l .# "polyline" .$ (points ,options)) world
        # (_,world)           = (layer .# "addTo" .$ (toJSArg mapObj)) world
		# world               = (object .# "layer" .= layer) world
		= world	

	createPolygon me mapObj l object world 
		//Set options
        # (options,world)     = jsEmptyObject world
		# (stroke,world)      = .? (object .# "attributes.strokeWidth") world
		# (strokeColor,world) = .? (object .# "attributes.strokeColor") world
		# (fillColor,world)   = .? (object .# "attributes.fillColor") world
		# world               = (options .# "stroke" .= stroke) world
		# world               = (options .# "color"  .= strokeColor) world
		# world               = (options .# "fillColor"  .= fillColor) world
		# (points,world)      = .? (object .# "attributes.points") world
        # (layer,world)       = (l .# "polygon" .$ (points ,options)) world
        # (_,world)           = (layer .# "addTo" .$ (toJSArg mapObj)) world
		# world               = (object .# "layer" .= layer) world
		= world	

	//Loop through a javascript array
	forall f array world
		# (len,world) = .? (array .# "length") world
		= forall` 0 (jsValToInt len) world
	where
		forall` i len world
			| i >= len = world
			# (el,world) = .? (array .# i) world
			= forall` (i + 1) len (f el world)

	//Process the edits received from the client
	onEdit dp ([],diff) m msk vst = case fromJSON diff of
		Just diffs = (Ok (NoChange,msk),foldl app m diffs,vst)
		Nothing = (Ok (NoChange,msk),m,vst)
	where
		app m (LDSetZoom zoom)          = {LeafletMap|m & perspective = {m.perspective & zoom = zoom}}
		app m (LDSetCenter center)      = {LeafletMap|m & perspective = {m.perspective & center = center}}
		app m (LDSetCursor cursor)      = {LeafletMap|m & perspective = {m.perspective & cursor = Just cursor}}
		app m (LDSetBounds bounds)      = {LeafletMap|m & perspective = {m.perspective & bounds = Just bounds}}
		app m (LDSelectMarker markerId) = {LeafletMap|m & objects = map (sel markerId) m.LeafletMap.objects}
		where
			sel x (Marker m=:{LeafletMarker|markerId}) = Marker {LeafletMarker|m & selected = markerId == x}
			sel x o = o
		app m _ = m
	onEdit _ _ m msk ust = (Ok (NoChange,msk),m,ust)

	//Check for changed objects and update the client
	onRefresh _ m2 m1 mask vst
		//Determine attribute changes
		# attrChanges = diffAttributes m1 m2
		//Determine object changes
		# childChanges = diffObjects 0 m1.LeafletMap.objects m2.LeafletMap.objects
		= (Ok (ChangeUI attrChanges childChanges,mask),m2,vst)
	where
		diffObjects _ [] [] = []
		diffObjects i [] objs = [(n,InsertChild (encodeUI o)) \\ o <- objs & n <- [i..]] //Add new objects
		diffObjects i objs [] = repeatn (length objs) (i,RemoveChild)                   //Remove trailing objects
		diffObjects i [x:xs] [y:ys]
			| x === y = diffObjects (i + 1) xs ys //The head has not changed, just compare the remainder
					  = [(i,RemoveChild),(i,InsertChild (encodeUI y)):diffObjects (i + 1) xs ys] //Replace the head

		//Only center, zoom and cursor are synced to the client, bounds are only synced from client to server
		diffAttributes {LeafletMap|perspective=p1} {LeafletMap|perspective=p2}
			//Center
			# center = if (p2.LeafletPerspective.center === p1.LeafletPerspective.center) [] [SetAttribute "center" (toJSON p2.LeafletPerspective.center)]
			//Zoom
			# zoom = if (p2.LeafletPerspective.zoom === p1.LeafletPerspective.zoom) [] [SetAttribute "zoom" (toJSON p2.LeafletPerspective.zoom)]
			//Cursor
			# cursor = if (p2.LeafletPerspective.cursor === p1.LeafletPerspective.cursor) [] [SetAttribute "cursor" (maybe JSONNull toJSON p2.LeafletPerspective.cursor)]
			= center ++ zoom ++ cursor

gEditor{|LeafletMap|} = leafletEditor

gDefault{|LeafletMap|}
	= {LeafletMap|perspective=defaultValue, tilesUrl =Just openStreetMapTiles, objects = [Marker homeMarker], icons = []}
where
	homeMarker = {markerId = "home", position= {LeafletLatLng|lat = 51.82, lng = 5.86}, title = Just "HOME", icon = Nothing, selected = False}

gDefault{|LeafletPerspective|}
    = {LeafletPerspective|center = {LeafletLatLng|lat = 51.82, lng = 5.86}, zoom = 7, cursor = Nothing, bounds = Nothing}

//Comparing reals may have unexpected results, especially when comparing constants to previously stored ones
gEq{|LeafletLatLng|} x y = (toString x.lat == toString y.lat) && (toString x.lng == toString y.lng)

derive JSONEncode LeafletMap, LeafletPerspective, LeafletIcon, LeafletLatLng, LeafletBounds, LeafletObject, LeafletMarker, LeafletPolyline, LeafletPolygon, LeafletEdit
derive JSONDecode LeafletMap, LeafletPerspective, LeafletIcon, LeafletLatLng, LeafletBounds, LeafletObject, LeafletMarker, LeafletPolyline, LeafletPolygon, LeafletEdit
derive gDefault   LeafletIcon, LeafletLatLng, LeafletBounds, LeafletObject, LeafletMarker, LeafletPolyline, LeafletPolygon, LeafletEdit
derive gEq        LeafletMap, LeafletPerspective, LeafletIcon, LeafletBounds, LeafletObject, LeafletMarker, LeafletPolyline, LeafletPolygon, LeafletEdit
derive gText      LeafletMap, LeafletPerspective, LeafletIcon, LeafletLatLng, LeafletBounds, LeafletObject, LeafletMarker, LeafletPolyline, LeafletPolygon, LeafletEdit
derive gEditor    LeafletPerspective, LeafletIcon, LeafletLatLng, LeafletBounds, LeafletObject, LeafletMarker, LeafletPolyline, LeafletPolygon, LeafletEdit
