implementation module iTasks.API.Extensions.GIS.Leaflet

import iTasks
import iTasks.UI.Definition, iTasks.UI.JS.Map, iTasks.UI.Editor
import StdMisc, Data.Tuple
import qualified Data.Map as DM

from StdArray import class Array(uselect), instance Array {} a
//TODOS:
//The client side update* and remove* diff operations need to make sure that all
//the indexed javascript objects are properly removed from all indices.
//Otherwise you'll get a memory leak in the javascript

LEAFLET_JS :== "leaflet-0.7.2/leaflet.js"
LEAFLET_CSS :== "leaflet-0.7.2/leaflet.css"

:: JSLM = JSLM

:: LeafletClientState =
    {mapObj         :: !JSObj JSLM
    ,mapLayers      :: !JSArr (JSObject JSLM)
    ,mapIcons       :: !JSArr (JSObject JSLM)
    ,mapCursor      :: !Maybe (JSObj JSLM)
    ,mapObjects     :: !JSMap Int (JSArr JSLM) //For every object layer we keep an index of the objects in this layer
    }
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

CURSOR_OPTIONS  :== {color = "#00f", opacity = 1.0, radius = 3}
MAP_OPTIONS     :== {attributionControl = False, zoomControl = True}

:: LeafletDiff
    //Perspective
    = LDSetZoom         !Int
    | LDSetCenter       !LeafletLatLng
    | LDSetCursor       !(Maybe LeafletLatLng)
    | LDSetBounds       !(Maybe LeafletBounds)
    //Icons
    | LDAddIcons        ![LeafletIcon]
    | LDUpdateIcon      !Int LeafletIcon
    | LDRemoveIcons     !Int
    //Layers
    | LDAddLayers       ![LeafletLayer]
    | LDUpdateLayer     !Int LeafletLayer
    | LDRemoveLayers    !Int
    //Individual objects
    | LDAddObjects      !Int ![LeafletObject]
    | LDUpdateObject    !Int !Int !LeafletObject
    | LDRemoveObjects   !Int !Int

updUI :: DataPath LeafletMap EditMask LeafletMap EditMask *VSt -> *(!MaybeErrorString UIChange,!*VSt)
updUI _ m1 _ m2 _ vst = case diffs of [] = (Ok NoChange,vst) ; _ = (Ok (ChangeUI [SetAttribute "diff" (toJSON diffs)] []),vst)
where
    diffs
        =   diffPerspectives m1.perspective m2.perspective
        ++  diffIcons 0 m1.icons m2.icons
        ++  diffLayers 0 m1.layers m2.layers

    diffPerspectives p1 p2
        =   if (p1.zoom === p2.zoom) [] [LDSetZoom p2.zoom]
        ++  if (p1.center === p2.center) [] [LDSetCenter p2.center]
        ++  if (p1.cursor === p2.cursor) [] [LDSetCursor p2.cursor]
        ++  if (p1.bounds === p2.bounds) [] [LDSetBounds p2.bounds]

    diffIcons i [] [] = []
    diffIcons i [] i2 = [LDAddIcons i2]
    diffIcons i i1 [] = [LDRemoveIcons i]
    diffIcons i [i1:is1] [i2:is2]
        | i1 === i2     = diffIcons (inc i) is1 is2
                        = [LDUpdateIcon i i2:diffIcons (inc i) is1 is2]

    diffLayers i [] [] = []
    diffLayers i [] l2 = [LDAddLayers l2]
    diffLayers i l1 [] = [LDRemoveIcons i]
    diffLayers i [TileLayer url1:ls1] [TileLayer url2:ls2]
        | url1 == url2  = diffLayers (inc i) ls1 ls2
                        = [LDUpdateLayer i (TileLayer url2):diffLayers (inc i) ls1 ls2]
    diffLayers i [ObjectLayer obj1:ls1] [ObjectLayer obj2:ls2]
                        = diffObjects i 0 obj1 obj2 ++ diffLayers (inc i) ls1 ls2
    diffLayers i [l1:ls1] [l2:ls2]
                        = [LDUpdateLayer i l2:diffLayers (inc i) ls1 ls2]

    diffObjects l i [] [] = []
    diffObjects l i [] o2 = [LDAddObjects l o2]
    diffObjects l i o1 [] = [LDRemoveObjects l i]
    diffObjects l i [o1:os1] [o2:os2]
        | o1 === o2     = diffObjects l (inc i) os1 os2
                        = [LDUpdateObject l i o2:diffObjects l (inc i) os1 os2]

onEdit :: DataPath JSONNode LeafletMap EditMask !*USt -> *(!LeafletMap,!EditMask,!*USt)
onEdit [] diff m msk ust = case fromJSON diff of
	Just diffs = (app diffs m,msk,ust)
	Nothing = (m,msk,ust)
where
	app [] m = m
	app [LDSetZoom zoom:ds] m           = app ds {m & perspective = {m.perspective & zoom = zoom}}
	app [LDSetCenter center:ds] m       = app ds {m & perspective = {m.perspective & center = center}}
	app [LDSetCursor cursor:ds] m       = app ds {m & perspective = {m.perspective & cursor = cursor}}
	app [LDSetBounds bounds:ds] m       = app ds {m & perspective = {m.perspective & bounds = bounds}}
	app [LDAddIcons icons:ds] m         = app ds {m & icons = m.icons ++ icons}
	app [LDRemoveIcons n:ds] m          = app ds {m & icons = take n m.icons}
	app [LDUpdateIcon i icon:ds] m      = app ds {m & icons = updateAt i icon m.icons}
	app [LDAddLayers layers:ds] m       = app ds {m & layers = m.layers ++ layers}
	app [LDRemoveLayers n:ds] m         = app ds {m & layers = take n m.layers}
	app [LDUpdateLayer i layer:ds] m    = app ds {m & layers = updateAt i layer m.layers}
	app [LDAddObjects l objects:ds] m   = let (ObjectLayer o) = m.layers !! l in
		app ds {m & layers = updateAt l (ObjectLayer (o++objects)) m.layers}
	app [LDRemoveObjects l n:ds] m     = let (ObjectLayer o) = m.layers !! l in
    	app ds {m & layers = updateAt l (ObjectLayer (take n o)) m.layers}
	app [LDUpdateObject l i object:ds] m = let (ObjectLayer o) = m.layers !! l in
    	app ds {m & layers = updateAt l (ObjectLayer (updateAt i object o)) m.layers}
onEdit _ _ m msk ust                             = (m,msk,ust)

openStreetMapTiles :: LeafletLayer
openStreetMapTiles = TileLayer "http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png"
	
leafletEditlet :: Editlet LeafletMap
leafletEditlet 
  = { Editlet
    | genUI   = genUI 
    , initUI  = initUI
    , updUI   = updUI
    , onEdit  = onEdit
    }
where
	genUI dp val mask world
		# attr = sizeAttr (ExactSize 500) (ExactSize 150)
		= (Ok (uia UIViewHtml attr), world)

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
		# map=:{LeafletMap|perspective,icons,layers} = {gDefault{|*|} & layers =[openStreetMapTiles]} //Initial default map
        # (l,world)         = findObject "L" world
		# (domEl,world) 	= .? (me .# "domEl") world
		//Create the map
		# (mapObj,world)    = (l .# "map" .$ (domEl,MAP_OPTIONS)) world
		# world 			= (me .# "map" .= mapObj) world
		//Set perspective
        # (center,world)    = toJSArray [perspective.center.lat,perspective.center.lng] world
        # (_,world)         = (mapObj .# "setView" .$ (center, perspective.LeafletPerspective.zoom)) world
        //Update map bounds
        # (bounds,world)      = getMapBounds mapObj world
        # map = {LeafletMap|map & perspective ={LeafletPerspective|perspective & bounds = bounds}}
		//Create cursor
        # (mapCursor,world) = case perspective.cursor of
            Nothing     = (Nothing,world)
            Just {LeafletLatLng|lat,lng}
                # (mapCursor,world)   = (l .# "circleMarker" .$ ([lat,lng],CURSOR_OPTIONS)) world
                # (_,world)           = (mapCursor .# "addTo" .$ mapObj) world
                = (Just mapCursor,world)
        //Add icons
        # (mapIcons,world)  = newJSArray world
        # world             = foldl (\w icon -> addIcon icon l mapIcons w) world icons
		# world 			= (me .# "icons" .= mapIcons) world
        //Add initial layers
        # (mapLayers,world)   = newJSArray world
        //# (mapObjects,world)  = jsNewMap world
        # (_,world)           = foldl (\w layer -> addLayer layer l mapObj mapLayers mapIcons w) (0,world) layers
		= (jsNull,world)

	//createIcon :: !LeafletIcon !(JSObj JSLM) !(JSArr (JSObject b)) !*JSWorld -> *JSWorld
    addIcon {LeafletIcon|iconUrl,iconSize=(w,h)} l icons world
        # (icon,world)    = (l .# "icon" .$ (toJSArg {IconOptions|iconUrl=iconUrl,iconSize=[w,h]})) world
        # (_,world)       = jsArrayPush icon icons world
        = world

    addLayer (TileLayer url) l mapObj mapLayers mapIcons (i,world)
        # (layer,world)       = (l .# "tileLayer" .$ url) world
        # (_,world)           = jsArrayPush layer mapLayers world
        # (_,world)           = (layer .# "addTo" .$ (toJSArg mapObj)) world
        = (i + 1,world)

/*
    onLibLoaded mkHandler cid _ (map=:{LeafletMap|perspective,icons,layers},_) env
        # (l,env)           = findObject "L" env
        //Create map
        # env               = syncMapDivSize cid env
        # (mapObj,env)      = (l .# "map" .$ (mapdivid cid,MAP_OPTIONS)) env
        //Set perspective
        # (center,env)      = toJSArray [perspective.center.lat,perspective.center.lng] env
        # (_,env)           = (mapObj .# "setView" .$ (center, perspective.LeafletPerspective.zoom)) env
        //Update map bounds
        # (bounds,env)      = getMapBounds mapObj env
        # map = {LeafletMap|map & perspective ={LeafletPerspective|perspective & bounds = bounds}}
        //Create cursor
        # (mapCursor,env) = case perspective.cursor of
            Nothing     = (Nothing,env)
            Just {LeafletLatLng|lat,lng}
                # (mapCursor,env)   = (l .# "circleMarker" .$ ([lat,lng],CURSOR_OPTIONS)) env
                # (_,env)           = (mapCursor .# "addTo" .$ mapObj) env
                = (Just mapCursor,env)
        //Add icons
        # (mapIcons,env)    = newJSArray env
        # env               = foldl (\w icon -> createIcon icon l mapIcons w) env icons
        //Add initial layers
        # (mapLayers,env)   = newJSArray env
        # (mapObjects,env)  = jsNewMap env
        # (_,env)           = foldl (\w layer -> createLayer mkHandler layer l mapObj mapLayers mapObjects mapIcons cid w) (0,env) layers
        //Add map event handlers
        # (_,env)           = (mapObj .# "addEventListener" .$ ("dragend",mkHandler onMapMove cid)) env
        # (_,env)           = (mapObj .# "addEventListener" .$ ("zoomend",mkHandler onZoomChange cid)) env
        # (_,env)           = (mapObj .# "addEventListener" .$ ("click",mkHandler onMapClick cid)) env
        //Add editlet event handler
        # (editlets,env)    = findObject "itwc.controller.editlets" env
        # (cmp,env)         = .? (editlets .# cid) env
        # env               = (cmp .# "afterShow" .= (toJSVal (mkHandler onAfterShow cid))) env
        # env               = (cmp .# "afterResize" .= (toJSVal (mkHandler onAfterShow cid))) env
        = ((map,Just {mapObj=mapObj,mapIcons=mapIcons,mapLayers=mapLayers,mapObjects,mapCursor=mapCursor}),Diff [LDSetBounds bounds] ignoreConflict,env)
*/



	mapdivid cid = "map_div_" +++ cid
/*
	onInit _ mkHandler cid env
		# map = gDefault{|*|}
        # (l, env) = findObject "L" env
        | jsIsUndefined l
            # env = addCSSFromUrl LEAFLET_CSS env
            # env = addJSFromUrl LEAFLET_JS (Just (mkHandler (onLibLoaded mkHandler) cid)) env
            = ((map,Nothing), env)
        | otherwise
            # (clval, _, env) = onLibLoaded mkHandler cid (abort "Not implemented") (map,Nothing) env
            = (clval, env)
*/

    onUpdate mkHandler cid [] (map,st) env
        = ((map,st),env)
    onUpdate mkHandler cid [LDSetZoom zoom:diffs] (map,Just st=:{mapObj}) env
        # (_,env) = (mapObj .# "setZoom" .$ zoom) env
        = onUpdate mkHandler cid diffs (map,Just st) env
    onUpdate mkHandler cid [LDSetCenter {LeafletLatLng|lat,lng}:diffs] (map,Just st=:{mapObj}) env
        # (_,env) = (mapObj .# "panTo" .$ (toJSArg [lat,lng])) env
        = onUpdate mkHandler cid diffs (map,Just st) env
    onUpdate mkHandler cid [LDSetCursor (Just {LeafletLatLng|lat,lng}):diffs] (map,Just st=:{mapObj,mapCursor=Nothing}) env
        # (l, env)        = findObject "L" env
        # (mapCursor,env) = (l .# "circleMarker" .$ ([lat,lng],CURSOR_OPTIONS)) env
        # (_,env)         = (mapCursor .# "addTo" .$ mapObj) env
        = onUpdate mkHandler cid diffs (map,Just {st & mapCursor = Just mapCursor}) env
    onUpdate mkHandler cid [LDSetCursor (Just {LeafletLatLng|lat,lng}):diffs] (map,Just st=:{mapObj,mapCursor=Just mapCursor}) env
        # (_,env)         = (mapCursor .# "setLatLng" .$ (toJSArg [lat,lng])) env
        = onUpdate mkHandler cid diffs (map,Just st) env
    onUpdate mkHandler cid [LDSetCursor Nothing:diffs] (map,Just st=:{mapObj,mapCursor=Just mapCursor}) env
        # (_,env)       = (mapObj .# "removeLayer" .$ mapCursor) env
        = onUpdate mkHandler cid diffs (map ,Just {st & mapCursor = Nothing}) env
	onUpdate mkHandler cid [LDSetBounds _:diffs] (map,Just st) env
		//Bounds are never set on the client
		= onUpdate mkHandler cid diffs (map,Just st) env
	onUpdate mkHandler cid [LDAddIcons icons:diffs] (map,Just st=:{mapObj,mapIcons}) env
        # (l,env)         = findObject "L" env
        # env             = foldl (\w icon -> createIcon icon l mapIcons w) env icons
        = onUpdate mkHandler cid diffs (map ,Just st) env
	onUpdate mkHandler cid [LDUpdateIcon idx icon:diffs] (map,Just st) env
		//TODO
		= onUpdate mkHandler cid diffs (map, Just st) env
	onUpdate mkHandler cid [LDRemoveIcons idx:diffs] (map,Just st=:{mapIcons}) env
        # (removeRefs,env)  = (mapIcons .# "splice" .$ idx) env 
		//Icons don't need to be explicitly destroyed
		= onUpdate mkHandler cid diffs (map, Just st) env
    onUpdate mkHandler cid [LDAddLayers []:diffs] (map,st) env
        = onUpdate mkHandler cid diffs (map,st) env
    onUpdate mkHandler cid [LDAddLayers [layer:layers]:diffs] (map,Just st=:{mapObj,mapIcons,mapLayers,mapObjects}) env
        # (l,env)           = findObject "L" env
        # (lidx,env)        = .? (mapLayers .# "length") env
		# (_,env) 			= createLayer mkHandler layer l mapObj mapLayers mapObjects mapIcons cid (jsValToInt lidx,env)
        = onUpdate mkHandler cid [LDAddLayers layers:diffs] (map,Just st) env
    onUpdate mkHandler cid [LDUpdateLayer idx (ObjectLayer objects):diffs] (map,Just st=:{mapObj,mapIcons,mapLayers,mapObjects}) env
        # (l,env)         = findObject "L" env
        # (layer,env)     = .? (mapLayers .# idx) env
        # (_,env)         = (mapObj .# "removeLayer" .$ layer) env
        # (layer,env)     = (l .# "layerGroup" .$ ()) env
        # (objRefs,env)   = newJSArray env
        # (_,env)         = foldl (\w object -> createObject mkHandler object l layer objRefs mapIcons cid w) (0,env) objects
        # env             = (mapLayers .# idx .= layer) env
		# env             = jsPut idx objRefs mapObjects env
        # (_,env)         = (layer .# "addTo" .$ mapObj) env
        = onUpdate mkHandler cid diffs (map,Just st) env
    onUpdate mkHandler cid [LDRemoveLayers idx:diffs] (map,Just st=:{mapObj,mapIcons,mapLayers,mapObjects}) env
		//TODO
		= onUpdate mkHandler cid diffs (map, Just st) env
    onUpdate mkHandler cid [LDAddObjects lidx objects:diffs] (map,Just st=:{mapObj,mapIcons,mapLayers,mapObjects}) env
        # (l, env)          = findObject "L" env
        # (layer,env)       = .? (mapLayers .# lidx) env
        # (objRefs,env)     = appFst fromJust (jsGet lidx mapObjects env)
        # (oidx,env)        = .? (objRefs .# "length") env
        # (_,env)           = foldl (\w object -> createObject mkHandler object l layer objRefs mapIcons cid w) (jsValToInt oidx,env) objects
        = onUpdate mkHandler cid diffs (map,Just st) env
    onUpdate mkHandler cid [LDUpdateObject lidx oidx object:diffs] (map,Just st=:{mapObj,mapIcons,mapLayers,mapObjects}) env
        # (l, env)          = findObject "L" env
        # (layer,env)       = .? (mapLayers .# lidx) env
        # (objRefs,env)     = appFst fromJust (jsGet lidx mapObjects env)
        # (objRef,env)      = .? (objRefs .# oidx) env
        # (_,env)           = (layer .# "removeLayer" .$ objRef) env
        # (_,env)           = createObject mkHandler object l layer objRefs mapIcons cid (oidx,env)
        = onUpdate mkHandler cid diffs (map,Just st) env
    onUpdate mkHandler cid [LDRemoveObjects lidx oidx:diffs] (map,Just st=:{mapLayers,mapObjects}) env
        # (layer,env)       = .? (mapLayers .# lidx) env
        # (objRefs,env)     = appFst fromJust (jsGet lidx mapObjects env)
        # (removeRefs,env)  = (objRefs .# "splice" .$ oidx) env
        # env               = removeObjects removeRefs layer env
        = onUpdate mkHandler cid diffs (map,Just st) env
	onUpdate mkHandler cid [_:updates] (map,Just st) env
		//Unimplemented clientside update -> continue with the other updates
		= onUpdate mkHandler cid updates (map,Just st) env
    onUpdate mkHandler cid updates (map,st) env
        = ((map,st),env)
/*
    onLibLoaded mkHandler cid _ (map=:{LeafletMap|perspective,icons,layers},_) env
        # (l,env)           = findObject "L" env
        //Create map
        # env               = syncMapDivSize cid env
        # (mapObj,env)      = (l .# "map" .$ (mapdivid cid,MAP_OPTIONS)) env
        //Set perspective
        # (center,env)      = toJSArray [perspective.center.lat,perspective.center.lng] env
        # (_,env)           = (mapObj .# "setView" .$ (center, perspective.LeafletPerspective.zoom)) env
        //Update map bounds
        # (bounds,env)      = getMapBounds mapObj env
        # map = {LeafletMap|map & perspective ={LeafletPerspective|perspective & bounds = bounds}}
        //Create cursor
        # (mapCursor,env) = case perspective.cursor of
            Nothing     = (Nothing,env)
            Just {LeafletLatLng|lat,lng}
                # (mapCursor,env)   = (l .# "circleMarker" .$ ([lat,lng],CURSOR_OPTIONS)) env
                # (_,env)           = (mapCursor .# "addTo" .$ mapObj) env
                = (Just mapCursor,env)
        //Add icons
        # (mapIcons,env)    = newJSArray env
        # env               = foldl (\w icon -> createIcon icon l mapIcons w) env icons
        //Add initial layers
        # (mapLayers,env)   = newJSArray env
        # (mapObjects,env)  = jsNewMap env
        # (_,env)           = foldl (\w layer -> createLayer mkHandler layer l mapObj mapLayers mapObjects mapIcons cid w) (0,env) layers
        //Add map event handlers
        # (_,env)           = (mapObj .# "addEventListener" .$ ("dragend",mkHandler onMapMove cid)) env
        # (_,env)           = (mapObj .# "addEventListener" .$ ("zoomend",mkHandler onZoomChange cid)) env
        # (_,env)           = (mapObj .# "addEventListener" .$ ("click",mkHandler onMapClick cid)) env
        //Add editlet event handler
        # (editlets,env)    = findObject "itwc.controller.editlets" env
        # (cmp,env)         = .? (editlets .# cid) env
        # env               = (cmp .# "afterShow" .= (toJSVal (mkHandler onAfterShow cid))) env
        # env               = (cmp .# "afterResize" .= (toJSVal (mkHandler onAfterShow cid))) env
        = ((map,Just {mapObj=mapObj,mapIcons=mapIcons,mapLayers=mapLayers,mapObjects,mapCursor=mapCursor}),Diff [LDSetBounds bounds] ignoreConflict,env)
*/
	createIcon :: !LeafletIcon !(JSObj JSLM) !(JSArr (JSObject b)) !*JSWorld -> *JSWorld
    createIcon {LeafletIcon|iconUrl,iconSize=(w,h)} l mapIcons env
        # (icon,env)    = (l .# "icon" .$ (toJSArg {IconOptions|iconUrl=iconUrl,iconSize=[w,h]})) env
        # (_,env)       = jsArrayPush icon mapIcons env
        = env

    //createLayer :: !LeafletLayer !(JSObj JSLM) a !(JSArr (JSObject b)) !(JSMap Int (JSArr JSLM)) !(JSVal (JSObject d)) !String !*(!Int,!*JSWorld) -> *(!Int,!*JSWorld)
    createLayer mkHandler (TileLayer url) l mapObj mapLayers mapObjects mapIcons cid (i,env)
        # (layer,env)       = (l .# "tileLayer" .$ url) env
        # (_,env)           = jsArrayPush layer mapLayers env
        # (_,env)           = (layer .# "addTo" .$ (toJSArg mapObj)) env
        = (i + 1,env)
    createLayer mkHandler (ObjectLayer objects) l mapObj mapLayers mapObjects mapIcons cid (i,env)
        # (layer,env)       = (l .# "layerGroup" .$ ()) env
        # (objRefs,env)     = newJSArray env
        # (_,env)           = foldl (\w object -> createObject mkHandler object l layer objRefs mapIcons cid w) (0,env) objects
        # (_,env)           = jsArrayPush layer mapLayers env
		# env               = jsPut i objRefs mapObjects env
        # (_,env)           = (layer .# "addTo" .$ (toJSArg mapObj)) env
        = (i + 1,env)

    //createObject :: !LeafletObject !(JSObj JSLM) !a !(JSVal (JSObject b)) !(JSVal (JSObject c)) !String !(!Int,!*JSWorld) -> (!Int,!*JSWorld)
    createObject mkHandler (Marker {LeafletMarker|markerId,position,title,icon}) l layer objRefs mapIcons cid (i,env)
        # (options,env)     = jsEmptyObject env
        # (options,env)     = case icon of
                                Nothing         = (options, env)
                                Just iconIdx
                                    # (iconRef, env) = .? (mapIcons .# iconIdx) env
                                    # env            = (options .# "icon" .= iconRef) env
                                    = (options, env)
        # (options,env)     = case title of
                                Nothing         = (options, env)
                                Just title
                                    # env = (options .# "title" .= title) env
                                    # env = (options .# "alt"   .= title) env
                                    = (options, env)
        # args              = [toJSArg [position.lat,position.lng], toJSArg options]

        # (marker,env)      = (l .# "marker" .$ args) env
        # env               = (objRefs .# i .= marker) env
        //# (_,env)           = (marker .# "addEventListener" .$ ("click",mkHandler (onMarkerClick markerId) cid)) env
        # (_,env)           = (marker .# "addTo" .$ (toJSArg layer)) env
        = (i + 1,env)
    createObject mkHandler (Polyline {LeafletPolyline|polylineId,points,strokeWidth,strokeColor}) l layer objRefs mapIcons cid (i,env)
        # (options,env)     = jsEmptyObject env
        # env               = (options .# "stroke" .= strokeWidth) env
        # env               = (options .# "color" .= strokeColor) env
        # (polyline,env)    = (l .# "polyline" .$ ([toJSArg [p.lat,p.lng] \\ p <- points],options)) env
        # env               = (objRefs .# i .= polyline) env
        # (_,env)           = (polyline .# "addTo" .$ (toJSArg layer)) env
        = (i + 1,env)
    createObject mkHandler (Polygon {LeafletPolygon|polygonId,points,strokeWidth,strokeColor,fillColor}) l layer objRefs mapIcons cid (i,env)
        # (options,env)     = jsEmptyObject env
        # env               = (options .# "stroke" .= strokeWidth) env
        # env               = (options .# "color" .= strokeColor) env
        # env               = (options .# "fill" .= (isJust fillColor)) env
        # env               = (maybe id (\c -> (options .# "fillColor" .= c)) fillColor) env
        # env               = (options .# "clickable" .= False) env
        # (polygon,env)     = (l .# "polygon" .$ ([toJSArg [p.lat,p.lng] \\ p <- points],options)) env
        # env               = (objRefs .# i .= polygon) env
        # (_,env)           = (polygon .# "addTo" .$ (toJSArg layer)) env
        = (i + 1,env)
/*
    onMapMove cid {[0]=event} (map=:{LeafletMap|perspective},mbSt) env
        # (mapobj,env)      = .? (event .# "target") env
        # (latlng,env)      = (mapobj .# "getCenter" .$ ()) env
        # (center,env)      = getPos latlng env
        # (bounds,env)      = getMapBounds mapobj env
        = (({LeafletMap|map & perspective = {perspective & center = center,bounds=bounds}},mbSt),Diff [LDSetCenter center] ignoreConflict,env)
    onZoomChange cid {[0]=event} (map=:{LeafletMap|perspective},mbSt) env
        # (mapobj,env)      = .? (event .# "target") env
        # (zoom,env)        = (mapobj .# "getZoom" .$ ()) env
        # (bounds,env)      = getMapBounds mapobj env
        = (({LeafletMap|map & perspective = {perspective & zoom = jsValToInt zoom,bounds=bounds}},mbSt),Diff [LDSetZoom (jsValToInt zoom)] ignoreConflict,env)
    onMapClick cid {[0]=event} (map=:{LeafletMap|perspective},Just st=:{mapObj,mapCursor=Nothing}) env
        # (l,env)           = findObject "L" env
        # (latlng,env)      = .? (event .# "latlng") env
        # (cursor,env)      = getPos latlng env
        # (mapCursor,env)   = (l .# "circleMarker" .$ (latlng,CURSOR_OPTIONS)) env
        # (_,env)           = (mapCursor .# "addTo" .$ (toJSArg mapObj)) env
        = (({LeafletMap|map & perspective = {perspective & cursor = Just cursor}},Just {st & mapCursor = Just mapCursor}),Diff [LDSetCursor (Just cursor)] ignoreConflict,env)
    onMapClick cid {[0]=event} (map=:{LeafletMap|perspective},Just st=:{mapObj,mapCursor=Just mapCursor}) env
        # (latlng,env)      = .? (event .# "latlng") env
        # (cursor,env)      = getPos latlng env
        # (_,env)           = (mapCursor .# "setLatLng" .$ (toJSArg latlng)) env
        = (({LeafletMap|map & perspective = {perspective & cursor = Just cursor}},Just st),Diff [LDSetCursor (Just cursor)] ignoreConflict,env)

    onMarkerClick markerId cid event (map=:{LeafletMap|layers},st) env
        # layers = [selectMarker l \\ l <- layers]
		# diff = maybe NoDiff (\d -> Diff d ignoreConflict) (updUI map ({map & layers = layers}))
        = (({map & layers = layers},st),diff,env)
    where
        selectMarker (ObjectLayer objects)
            = ObjectLayer [Marker {m & selected = m.markerId == markerId} \\ Marker m <- objects]
        selectMarker l = l

    onAfterShow cid event (map=:{LeafletMap|perspective},Just st=:{mapObj}) env
        # env           = syncMapDivSize cid env
        # (_,env)       = (mapObj .# "invalidateSize" .$ ()) env
        # (bounds,env)  = getMapBounds mapObj env
        # map = {LeafletMap|map & perspective ={LeafletPerspective|perspective & bounds = bounds}}
        = ((map,Just st),Diff [LDSetBounds bounds] ignoreConflict,env)
    onAfterShow cid event st env
        = (st,NoDiff,env)
*/
	getPos obj env
		# (lat,env)     = .? (obj .# "lat") env
		# (lng,env)     = .? (obj .# "lng") env
		= ({LeafletLatLng|lat=jsValToReal lat,lng=jsValToReal lng}, env)

    getMapBounds mapObj env
        # (bounds,env)      = (mapObj .# "getBounds" .$ ()) env
        # (sw,env)          = (bounds .# "getSouthWest" .$ ()) env
        # (ne,env)          = (bounds .# "getNorthEast" .$ ()) env
		# (swpos,env)       = getPos sw env
		# (nepos,env)       = getPos ne env
        = (Just {southWest=swpos,northEast=nepos},env)

    removeObjects :: !(JSArr a) !(JSObj b) !*JSWorld -> *JSWorld
    removeObjects removeRefs layer env
        # (ref,env)         = jsArrayPop removeRefs env
        | jsIsUndefined ref = env
        # (_,env)           = (layer .# "removeLayer" .$ (toJSArg ref)) env
        = removeObjects removeRefs layer env

	syncMapDivSize :: !String !*JSWorld -> *JSWorld
    syncMapDivSize cid env
        # (editlets,env)    = findObject "itwc.controller.editlets" env
        # (cmp,env)         = .? (editlets .# cid) env
        # (cmpDiv,env)      = .? (cmp .# "domEl") env
        # (mapDiv,env)      = .? (getElementById (mapdivid cid)) env
        # (divSize,env)     = measureDomEl cmpDiv env
        = sizeDomEl divSize mapDiv env

    measureDomEl :: !(JSObj a) !*JSWorld -> (!(!Int,!Int),!*JSWorld)
    measureDomEl el env
        # (w,env)           = .? (el .# "clientWidth") env
        # (h,env)           = .? (el .# "clientHeight") env
        = ((jsValToInt w,jsValToInt h),env)

    sizeDomEl :: !(!Int,!Int) !(JSObj a) !*JSWorld -> *JSWorld
    sizeDomEl (w,h) el env
        # env = (el .# "style.width" .= (toString w +++"px")) env
        # env = (el .# "style.height" .= (toString h +++"px")) env
        = env

    //ignoreConflict conflict state env = (state, NoDiff, env)

gEditor{|LeafletMap|} = fromEditlet leafletEditlet

gDefault{|LeafletPerspective|}
    = {LeafletPerspective|center = {LeafletLatLng|lat = 51.82, lng = 5.86}, zoom = 7, cursor = Nothing, bounds = Nothing}

//Comparing reals may have unexpected results!!!
//Especially when comparing constants to previously stored ones
gEq{|LeafletLatLng|} x y
    = (toString x.lat == toString y.lat) && (toString x.lng == toString y.lng)

derive JSONEncode       LeafletMap, LeafletPerspective, LeafletIcon, LeafletLatLng, LeafletBounds, LeafletLayer, LeafletObject, LeafletMarker, LeafletPolyline, LeafletPolygon, LeafletDiff, LeafletClientState, JSLM
derive JSONDecode       LeafletMap, LeafletPerspective, LeafletIcon, LeafletLatLng, LeafletBounds, LeafletLayer, LeafletObject, LeafletMarker, LeafletPolyline, LeafletPolygon, LeafletDiff, LeafletClientState, JSLM
derive gDefault         LeafletMap,                     LeafletIcon, LeafletLatLng, LeafletBounds, LeafletLayer, LeafletObject, LeafletMarker, LeafletPolyline, LeafletPolygon, LeafletDiff, LeafletClientState, JSLM
derive gEq              LeafletMap, LeafletPerspective, LeafletIcon,                LeafletBounds, LeafletLayer, LeafletObject, LeafletMarker, LeafletPolyline, LeafletPolygon, LeafletDiff, LeafletClientState, JSLM
derive gText            LeafletMap, LeafletPerspective, LeafletIcon, LeafletLatLng, LeafletBounds, LeafletLayer, LeafletObject, LeafletMarker, LeafletPolyline, LeafletPolygon, LeafletDiff, LeafletClientState, JSLM
derive gEditor                      LeafletPerspective, LeafletIcon, LeafletLatLng, LeafletBounds, LeafletLayer, LeafletObject, LeafletMarker, LeafletPolyline, LeafletPolygon, LeafletDiff, LeafletClientState, JSLM
