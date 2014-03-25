implementation module iTasks.API.Extensions.GIS.Leaflet

import iTasks
import iTasks.API.Core.Client.Editlet
import iTasks.API.Core.Client.Map
import StdMisc, Data.Tuple

from StdArray import class Array(uselect), instance Array {} a

LEAFLET_JS :== "leaflet-0.7.2/leaflet.js"
LEAFLET_CSS :== "leaflet-0.7.2/leaflet.css"

:: LeafletClientState =
    {mapObj         :: !JSObj JSLM
    ,mapLayers      :: !JSVal [JSObj JSLM]
    ,mapIcons       :: !JSVal [JSObj JSLM]
    ,mapCursor      :: !Maybe (JSObj JSLM)
    ,mapObjects     :: !JSVal (JSMap Int [JSObject JSLM])
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

genDiff :: LeafletMap LeafletMap -> Maybe [LeafletDiff]
genDiff m1 m2 = case diffs of [] = Nothing ; _ = Just diffs
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

appDiff :: [LeafletDiff] LeafletMap -> LeafletMap
appDiff [] m                            = m
appDiff [LDSetZoom zoom:ds] m           = appDiff ds {m & perspective = {m.perspective & zoom = zoom}}
appDiff [LDSetCenter center:ds] m       = appDiff ds {m & perspective = {m.perspective & center = center}}
appDiff [LDSetCursor cursor:ds] m       = appDiff ds {m & perspective = {m.perspective & cursor = cursor}}
appDiff [LDSetBounds bounds:ds] m       = appDiff ds {m & perspective = {m.perspective & bounds = bounds}}
appDiff [LDAddIcons icons:ds] m         = appDiff ds {m & icons = m.icons ++ icons}
appDiff [LDRemoveIcons n:ds] m          = appDiff ds {m & icons = take n m.icons}
appDiff [LDUpdateIcon i icon:ds] m      = appDiff ds {m & icons = updateAt i icon m.icons}
appDiff [LDAddLayers layers:ds] m       = appDiff ds {m & layers = m.layers ++ layers}
appDiff [LDRemoveLayers n:ds] m         = appDiff ds {m & layers = take n m.layers}
appDiff [LDUpdateLayer i layer:ds] m    = appDiff ds {m & layers = updateAt i layer m.layers}
appDiff [LDAddObjects l objects:ds] m   = let (ObjectLayer o) = m.layers !! l in
    appDiff ds {m & layers = updateAt l (ObjectLayer (o++objects)) m.layers}
appDiff [LDRemoveObjects l n:ds] m      = let (ObjectLayer o) = m.layers !! l in
    appDiff ds {m & layers = updateAt l (ObjectLayer (take n o)) m.layers}
appDiff [LDUpdateObject l i object:ds] m     = let (ObjectLayer o) = m.layers !! l in
    appDiff ds {m & layers = updateAt l (ObjectLayer (updateAt i object o)) m.layers}
appDiff _ m                             = m

openStreetMapTiles :: LeafletLayer
openStreetMapTiles = TileLayer "http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png"
	
leafletEditlet :: LeafletMap -> Editlet LeafletMap [LeafletDiff]
leafletEditlet map = Editlet map
    { EditletServerDef
    | genUI		= \cid world -> (uiDef cid, world)
    , defVal 	= gDefault{|*|}
    , genDiff	= genDiff
    , appDiff	= appDiff
    }
    { EditletClientDef
    | updateUI	= onUpdate
    , defVal 	= (gDefault{|*|},Nothing)
    , genDiff	= \(v1,_) (v2,_) -> genDiff v1 v2
    , appDiff	= \diff (map,st) -> (appDiff diff map,st)
    }
where
    uiDef cid
          = { html          = DivTag [IdAttr (mapdivid cid)] []
            , eventHandlers = []
            , width         = ExactSize 600
            , height        = ExactSize 300
            }

	mapdivid cid = "map_div_" +++ cid

	onUpdate cid mbDiff (map,Nothing) world
        # (l, world) = findObject "L" world
        | jsIsUndefined l
            # world = addCSSFromUrl LEAFLET_CSS world
            # world = addJSFromUrl LEAFLET_JS (Just (createEditletEventHandler onLibLoaded cid)) world
            = ((map,Nothing), world)
        | otherwise
            = onLibLoaded cid Nothing (map,Nothing) world

    onUpdate cid (Just [LDSetZoom zoom:diffs]) (map,Just st=:{mapObj}) world
        # (_,world) = callObjectMethod "setZoom" [toJSArg zoom] mapObj world
        = onUpdate cid (Just diffs) (map,Just st) world
    onUpdate cid (Just [LDSetCenter {LeafletLatLng|lat,lng}:diffs]) (map,Just st=:{mapObj}) world
        # (_,world) = callObjectMethod "panTo" [toJSArg [lat,lng]] mapObj world
        = onUpdate cid (Just diffs) (map,Just st) world
    onUpdate cid (Just [LDSetCursor (Just {LeafletLatLng|lat,lng}):diffs]) (map,Just st=:{mapObj,mapCursor=Nothing}) world
        # (l, world)        = findObject "L" world
        # (mapCursor,world) = callObjectMethod "circleMarker" [toJSArg [lat,lng],toJSArg CURSOR_OPTIONS] l world
        # (_,world)         = callObjectMethod "addTo" [toJSArg mapObj] mapCursor world
        = onUpdate cid (Just diffs) (map,Just {st & mapCursor = Just mapCursor}) world
    onUpdate cid (Just [LDSetCursor (Just {LeafletLatLng|lat,lng}):diffs]) (map,Just st=:{mapObj,mapCursor=Just mapCursor}) world
        # (_,world)         = callObjectMethod "setLatLng" [toJSArg [lat,lng]] mapCursor world
        = onUpdate cid (Just diffs) (map,Just st) world
    onUpdate cid (Just [LDSetCursor Nothing:diffs]) (map,Just st=:{mapObj,mapCursor=Just mapCursor}) world
        # (_,world)     = callObjectMethod "removeLayer" [toJSArg mapCursor] mapObj world
        = onUpdate cid (Just diffs) (map ,Just {st & mapCursor = Nothing}) world
    onUpdate cid (Just [LDUpdateLayer idx (ObjectLayer objects):diffs]) (map,Just st=:{mapObj,mapIcons,mapLayers,mapObjects}) world
        # (l, world)    = findObject "L" world
        # (layer,world) = jsGetObjectEl idx mapLayers world
        # (_,world)     = callObjectMethod "removeLayer" [toJSArg layer] mapObj world
        # (layer,world) = callObjectMethod "layerGroup" [] l world
        # (objRefs,world)   = newJSArray world
        # (_,world)     = foldl (\w object -> createObject object l layer objRefs mapIcons cid w) (0,world) objects
        # world         = jsSetObjectEl idx layer mapLayers world
		# world         = jsPut idx objRefs mapObjects world
        # (_,world)     = callObjectMethod "addTo" [toJSArg mapObj] layer world
        = onUpdate cid (Just diffs) (map,Just st) world
    onUpdate cid (Just [LDAddLayers [TileLayer url:layers]:diffs]) (map,Just st=:{mapObj,mapIcons,mapLayers}) world
        # (l, world)    = findObject "L" world
        # (layer,world) = callObjectMethod "tileLayer" [toJSArg url] l world
        # (_,world)     = jsArrayPush layer mapLayers world
        # (_,world)     = callObjectMethod "addTo" [toJSArg mapObj] layer world
        = onUpdate cid (Just [LDAddLayers layers:diffs]) (map,Just st) world
    onUpdate cid (Just [LDAddLayers [ObjectLayer objects:layers]:diffs]) (map,Just st=:{mapObj,mapIcons,mapLayers,mapObjects}) world
        # (l, world)        = findObject "L" world
        # (layer,world)     = callObjectMethod "layerGroup" [] l world
        # (objRefs,world)   = newJSArray world
        # (lidx,world)      = appFst jsValToInt (jsGetObjectAttr "length" mapLayers world)
        # (_,world)         = foldl (\w object -> createObject object l layer objRefs mapIcons cid w) (0,world) objects
		# world             = jsPut lidx objRefs mapObjects world
        # (_,world)         = jsArrayPush layer mapLayers world
        = onUpdate cid (Just [LDAddLayers layers:diffs]) (map,Just st) world
    onUpdate cid (Just [LDAddLayers []:diffs]) (map,st) world
        = onUpdate cid (Just diffs) (map,st) world
    onUpdate cid (Just [LDUpdateObject lidx oidx object:diffs]) (map,Just st=:{mapObj,mapIcons,mapLayers,mapObjects}) world
        # (l, world)        = findObject "L" world
        # (layer,world)     = jsGetObjectEl lidx mapLayers world
        # (objRefs,world)   = appFst fromJust (jsGet lidx mapObjects world)
        # (objRef,world)    = jsGetObjectEl oidx objRefs world
        # (_,world)         = callObjectMethod "removeLayer" [toJSArg objRef] layer world
        # (_,world)         = createObject object l layer objRefs mapIcons cid (oidx,world)
        = onUpdate cid (Just diffs) (map,Just st) world
    onUpdate cid (Just [LDAddObjects lidx objects:diffs]) (map,Just st=:{mapObj,mapIcons,mapLayers,mapObjects}) world
        # (l, world)        = findObject "L" world
        # (layer,world)     = jsGetObjectEl lidx mapLayers world
        # (objRefs,world)   = appFst fromJust (jsGet lidx mapObjects world)
        # (oidx,world)      = appFst jsValToInt (jsGetObjectAttr "length" objRefs world)
        # (_,world)         = foldl (\w object -> createObject object l layer objRefs mapIcons cid w) (oidx,world) objects
        = onUpdate cid (Just diffs) (map,Just st) world
    onUpdate cid (Just [LDRemoveObjects lidx oidx:diffs]) (map,Just st=:{mapLayers,mapObjects}) world
        # (layer,world)     = jsGetObjectEl lidx mapLayers world
        # (objRefs,world)   = appFst fromJust (jsGet lidx mapObjects world)
        # (removeRefs,world)= callObjectMethod "splice" [toJSArg oidx] objRefs world
        # world             = removeObjects removeRefs layer world
        = onUpdate cid (Just diffs) (map,Just st) world
    onUpdate cid _ (map,st) world
        = ((map,st),world)

    onLibLoaded cid _ (map=:{LeafletMap|perspective,icons,layers},_) world
        # (l, world) = findObject "L" world
        //Create map
        # world             = syncMapDivSize cid world
        # (mapObj, world) = callObjectMethod "map" [toJSArg (mapdivid cid),toJSArg MAP_OPTIONS] l world
        //Set perspective
        # (center,world) = toJSArray [perspective.center.lat,perspective.center.lng] world
        # (_,world) = callObjectMethod "setView" [toJSArg center, toJSArg perspective.LeafletPerspective.zoom] mapObj world
        //Update map bounds
        # (bounds,world) = getMapBounds mapObj world
        # map = {LeafletMap|map & perspective ={LeafletPerspective|perspective & bounds = bounds}}
        //Create cursor
        # (mapCursor,world) = case perspective.cursor of
            Nothing     = (Nothing,world)
            Just {LeafletLatLng|lat,lng}
                # (mapCursor,world) = callObjectMethod "circleMarker" [toJSArg [lat,lng],toJSArg CURSOR_OPTIONS] l world
                # (_,world)         = callObjectMethod "addTo" [toJSArg mapObj] mapCursor world
                = (Just mapCursor,world)
        //Add icons
        # (mapIcons,world) = newJSArray world
        # world = foldl (\w icon -> createIcon icon l mapIcons w) world icons
        //Add initial layers
        # (mapLayers,world) = newJSArray world
        # (mapObjects,world) = jsNewMap world
        # (_,world) = foldl (\w layer -> createLayer layer l mapObj mapLayers mapObjects mapIcons cid w) (0,world) layers
        //Add map event handlers
        # (_, world) = callObjectMethod "addEventListener" [toJSArg "dragend",toJSArg (createEditletEventHandler onMapMove cid)] mapObj world
        # (_, world) = callObjectMethod "addEventListener" [toJSArg "zoomend",toJSArg (createEditletEventHandler onZoomChange cid)] mapObj world
        # (_, world) = callObjectMethod "addEventListener" [toJSArg "click",toJSArg (createEditletEventHandler onMapClick cid)] mapObj world
        //Add editlet event handler
        # (editlets,world)  = findObject "itwc.controller.editlets" world
        # (cmp,world)       = jsGetObjectAttr cid editlets world
        # world             = jsSetObjectAttr "afterShow" (toJSVal (createEditletEventHandler onAfterShow cid)) cmp world
        # world             = jsSetObjectAttr "afterResize" (toJSVal (createEditletEventHandler onAfterShow cid)) cmp world
        = ((map,Just {mapObj=mapObj,mapIcons=mapIcons,mapLayers=mapLayers,mapObjects,mapCursor=mapCursor}),world)

    createIcon {LeafletIcon|iconUrl,iconSize=(w,h)} l mapIcons world
        # (icon,world)  = callObjectMethod "icon" [toJSArg {IconOptions|iconUrl=iconUrl,iconSize=[w,h]}] l world
        # (_, world)    = jsArrayPush icon mapIcons world
        = world

    createLayer :: !LeafletLayer !(JSObj JSLM) a !(JSObj [b]) !(JSObj (JSMap Int [c])) !(JSVal d) !String !*(!Int,!*JSWorld) -> *(!Int,!*JSWorld)
    createLayer (TileLayer url) l mapObj mapLayers mapObjects mapIcons cid (i,world)
        # (layer,world) = callObjectMethod "tileLayer" [toJSArg url] l world
        # (_,world)     = jsArrayPush layer mapLayers world
        # (_,world)     = callObjectMethod "addTo" [toJSArg mapObj] layer world
        = (i + 1,world)
    createLayer (ObjectLayer objects) l mapObj mapLayers mapObjects mapIcons cid (i,world)
        # (layer,world)     = callObjectMethod "layerGroup" [] l world
        # (objRefs,world)   = newJSArray world
        # (_,world)         = foldl (\w object -> createObject object l layer objRefs mapIcons cid w) (0,world) objects
        # (_,world)         = jsArrayPush layer mapLayers world
		# world             = jsPut i objRefs mapObjects world
        # (_,world)         = callObjectMethod "addTo" [toJSArg mapObj] layer world
        = (i + 1,world)

    createObject :: !LeafletObject !(JSObj JSLM) !a !(JSVal b) !(JSVal c) !String !(!Int,!*JSWorld) -> (!Int,!*JSWorld)
    createObject (Marker {LeafletMarker|markerId,position,title,icon}) l layer objRefs mapIcons cid (i,world)
        # (args,world)      = case icon of
            Nothing         = ([toJSArg [position.lat,position.lng]],world)
            Just iconIdx
                # (options,world) = jsEmptyObject world
                # (iconRef,world) = jsGetObjectEl iconIdx mapIcons world
                # world = jsSetObjectAttr "icon" iconRef options world
                = ([toJSArg [position.lat,position.lng],toJSArg options],world)
        # (marker,world) = callObjectMethod "marker" args l world
        # world          = jsSetObjectEl i marker objRefs world
        # (_,world)      = callObjectMethod "addEventListener" [toJSArg "click",toJSArg (createEditletEventHandler (onMarkerClick markerId) cid)] marker world
        # (_,world)      = callObjectMethod "addTo" [toJSArg layer] marker world
        = (i + 1,world)

    onMapMove cid {[0]=event} (map=:{LeafletMap|perspective},mbSt) world
        # (mapobj,world) = jsGetObjectAttr "target" event world
        # (latlng,world) = callObjectMethod "getCenter" [] mapobj world
        # (center,world) = getPos latlng world
        # (bounds,world) = getMapBounds mapobj world
        = (({LeafletMap|map & perspective = {perspective & center = center,bounds=bounds}},mbSt),world)
    onZoomChange cid {[0]=event} (map=:{LeafletMap|perspective},mbSt) world
        # (mapobj,world)    = jsGetObjectAttr "target" event world
        # (zoom,world)      = callObjectMethod "getZoom" [] mapobj world
        # (bounds,world)    = getMapBounds mapobj world
        = (({LeafletMap|map & perspective = {perspective & zoom = jsValToInt zoom,bounds=bounds}},mbSt),world)
    onMapClick cid {[0]=event} (map=:{LeafletMap|perspective},Just st=:{mapObj,mapCursor=Nothing}) world
        # (l, world)        = findObject "L" world
        # (latlng,world)    = jsGetObjectAttr "latlng" event world
        # (cursor,world)    = getPos latlng world
        # (mapCursor,world) = callObjectMethod "circleMarker" [toJSArg latlng,toJSArg CURSOR_OPTIONS] l world
        # (_,world)         = callObjectMethod "addTo" [toJSArg mapObj] mapCursor world
        = (({LeafletMap|map & perspective = {perspective & cursor = Just cursor}},Just {st & mapCursor = Just mapCursor}),world)
    onMapClick cid {[0]=event} (map=:{LeafletMap|perspective},Just st=:{mapObj,mapCursor=Just mapCursor}) world
        # (latlng,world)    = jsGetObjectAttr "latlng" event world
        # (cursor,world)    = getPos latlng world
        # (_,world)         = callObjectMethod "setLatLng" [toJSArg latlng] mapCursor world
        = (({LeafletMap|map & perspective = {perspective & cursor = Just cursor}},Just st),world)

    onMarkerClick markerId cid event (map=:{LeafletMap|layers},st) world
        # layers = [selectMarker l \\ l <- layers]
        = (({map & layers = layers},st),world)
    where
        selectMarker (ObjectLayer objects)
            = ObjectLayer [Marker {m & selected = m.markerId == markerId} \\ Marker m <- objects]
        selectMarker l = l

    onAfterShow cid event (map=:{LeafletMap|perspective},Just st=:{mapObj}) world
        # world     = syncMapDivSize cid world
        # (_,world) = callObjectMethod "invalidateSize" [] mapObj world
        # (bounds,world) = getMapBounds mapObj world
        # map = {LeafletMap|map & perspective ={LeafletPerspective|perspective & bounds = bounds}}
        = ((map,Just st),world)
    onAfterShow cid event st world
        = (st,world)

	getPos obj world
		# (lat, world) = jsGetObjectAttr "lat" obj world
		# (lng, world) = jsGetObjectAttr "lng" obj world
		= ({LeafletLatLng|lat=jsValToReal lat,lng=jsValToReal lng}, world)

    getMapBounds mapObj world
        # (bounds,world)    = callObjectMethod "getBounds" [] mapObj world
        # (sw,world)        = callObjectMethod "getSouthWest" [] bounds world
        # (ne,world)        = callObjectMethod "getNorthEast" [] bounds world
		# (swpos, world)    = getPos sw world
		# (nepos, world)    = getPos ne world
        = (Just {southWest=swpos,northEast=nepos},world)

    removeObjects :: !(JSVal [a]) !(JSVal b) !*JSWorld -> *JSWorld
    removeObjects removeRefs layer world
        # (ref,world) = jsArrayPop removeRefs world
        | jsIsUndefined ref = world
        # (_,world)   = callObjectMethod "removeLayer" [toJSArg ref] layer world
        = removeObjects removeRefs layer world

    syncMapDivSize :: !String !*JSWorld -> *JSWorld
    syncMapDivSize cid world
        # (editlets,world)  = findObject "itwc.controller.editlets" world
        # (cmp,world)       = jsGetObjectAttr cid editlets world
        # (cmpDiv,world)    = jsGetObjectAttr "domEl" cmp world
        # (mapDiv,world)    = getDomElement (mapdivid cid) world
        # (divSize,world)   = measureDomEl cmpDiv world
        = sizeDomEl divSize mapDiv world

    measureDomEl :: !(JSVal a) !*JSWorld -> (!(!Int,!Int),!*JSWorld)
    measureDomEl el world
        # (w,world) = jsGetObjectAttr "clientWidth" el world
        # (h,world) = jsGetObjectAttr "clientHeight" el world
        = ((jsValToInt w,jsValToInt h),world)

    sizeDomEl :: !(!Int,!Int) !(JSVal a) !*JSWorld -> *JSWorld
    sizeDomEl (w,h) el world
        # world = jsSetObjectAttr "style.width" (toJSVal (toString w +++"px")) el world
        # world = jsSetObjectAttr "style.height" (toJSVal (toString h +++"px")) el world
        = world

gEditor{|LeafletMap|} dp vv=:(val,mask,ver) meta vst
    = gEditor{|*|} dp (leafletEditlet val,mask,ver) meta vst

gUpdate{|LeafletMap|} dp upd (val,mask) iworld
    # ((Editlet value _ _,mask),iworld) = gUpdate{|*|} dp upd (leafletEditlet val,mask) iworld
    = ((value,mask),iworld)

gVerify{|LeafletMap|} _ vst = alwaysValid vst

gDefault{|LeafletPerspective|}
    = {LeafletPerspective|center = {LeafletLatLng|lat = 51.82, lng = 5.86}, zoom = 7, cursor = Nothing, bounds = Nothing}

//Comparing reals may have unexpected results!!!
//Especially when comparing constants to previously stored ones
gEq{|LeafletLatLng|} x y
    = (toString x.lat == toString y.lat) && (toString x.lng == toString y.lng)

derive JSONEncode       LeafletMap, LeafletPerspective, LeafletIcon, LeafletLatLng, LeafletBounds, LeafletLayer, LeafletObject, LeafletMarker, LeafletDiff
derive JSONDecode       LeafletMap, LeafletPerspective, LeafletIcon, LeafletLatLng, LeafletBounds, LeafletLayer, LeafletObject, LeafletMarker, LeafletDiff
derive gDefault         LeafletMap,                     LeafletIcon, LeafletLatLng, LeafletBounds, LeafletLayer, LeafletObject, LeafletMarker, LeafletDiff
derive gEq              LeafletMap, LeafletPerspective, LeafletIcon,                LeafletBounds, LeafletLayer, LeafletObject, LeafletMarker, LeafletDiff
derive gVisualizeText   LeafletMap, LeafletPerspective, LeafletIcon, LeafletLatLng, LeafletBounds, LeafletLayer, LeafletObject, LeafletMarker, LeafletDiff
derive gEditor                      LeafletPerspective, LeafletIcon, LeafletLatLng, LeafletBounds, LeafletLayer, LeafletObject, LeafletMarker, LeafletDiff
derive gEditMeta        LeafletMap, LeafletPerspective, LeafletIcon, LeafletLatLng, LeafletBounds, LeafletLayer, LeafletObject, LeafletMarker, LeafletDiff
derive gUpdate                      LeafletPerspective, LeafletIcon, LeafletLatLng, LeafletBounds, LeafletLayer, LeafletObject, LeafletMarker, LeafletDiff
derive gVerify	                    LeafletPerspective, LeafletIcon, LeafletLatLng, LeafletBounds, LeafletLayer, LeafletObject, LeafletMarker, LeafletDiff
