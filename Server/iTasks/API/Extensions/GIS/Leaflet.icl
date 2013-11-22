implementation module iTasks.API.Extensions.GIS.Leaflet

import iTasks
import iTasks.API.Core.Client.Editlet
import StdMisc

LEAFLET_JS :== "leaflet-0.6.3/leaflet.js"
LEAFLET_CSS :== "leaflet-0.6.3/leaflet.css"

:: LeafletClientState =
    {mapObj         :: !Maybe (JSVal JSObject)
    ,mapLayers      :: !Maybe (JSVal [JSObject])
    ,mapIcons       :: !Maybe (JSVal [JSObject])
    ,mapCursor      :: !Maybe (JSVal JSObject)
    ,libsInserted   :: !Bool
    }
:: IconOptions =
    { iconUrl   :: !String
    , iconSize  :: ![Int]
    }
:: CursorOptions =
    { color     :: !String
    , opacity   :: !Real
    , radius    :: !Int
    }

:: TESTTYPE     = TESTTYPE

CURSOR_OPTIONS :== {color = "#00f", opacity = 1.0, radius = 3}

:: LeafletDiff
    //Perspective
    = LDSetZoom         !Int
    | LDSetCenter       !LeafletLatLng
    | LDSetCursor       !(Maybe LeafletLatLng)
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
    | LDRemoveObjects   !Int

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

    diffIcons i [] [] = []
    diffIcons i [] i2 = [LDAddIcons i2]
    diffIcons i i1 [] = [LDRemoveIcons i]
    diffIcons i [i1:is1] [i2:is2]
        | i1 === i2     = diffIcons (inc i) is1 is2
                        = [LDUpdateIcon i i2:diffIcons (inc i) is1 is2]

    diffLayers i [] [] = []
    diffLayers i [] l2 = [LDAddLayers l2]
    diffLayers i l1 [] = [LDRemoveIcons i]
    diffLayers i [l1:ls1] [l2:ls2]
        | l1 === l2     = diffLayers (inc i) ls1 ls2
                        = [LDUpdateLayer i l2:diffLayers (inc i) ls1 ls2]

appDiff :: [LeafletDiff] LeafletMap -> LeafletMap
appDiff [] m                            = m
appDiff [LDSetZoom zoom:ds] m           = appDiff ds {m & perspective = {m.perspective & zoom = zoom}}
appDiff [LDSetCenter center:ds] m       = appDiff ds {m & perspective = {m.perspective & center = center}}
appDiff [LDSetCursor cursor:ds] m       = appDiff ds {m & perspective = {m.perspective & cursor = cursor}}
appDiff [LDAddIcons icons:ds] m         = appDiff ds {m & icons = m.icons ++ icons}
appDiff [LDRemoveIcons n:ds] m          = appDiff ds {m & icons = take n m.icons}
appDiff [LDUpdateIcon i icon:ds] m      = appDiff ds {m & icons = updateAt i icon m.icons}
appDiff [LDAddLayers layers:ds] m       = appDiff ds {m & layers = m.layers ++ layers}
appDiff [LDRemoveLayers n:ds] m         = appDiff ds {m & layers = take n m.layers}
appDiff [LDUpdateLayer i layer:ds] m    = appDiff ds {m & layers = updateAt i layer m.layers}
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
    , defVal 	= (gDefault{|*|},{mapObj = Nothing, mapIcons = Nothing, mapLayers = Nothing, mapCursor = Nothing, libsInserted = False})
    , genDiff	= \(v1,_) (v2,_) -> genDiff v1 v2
    , appDiff	= \diff (map,st) -> (appDiff diff map,st)
    }
where
    uiDef cid
          = { html          = DivTag [IdAttr (mapdivid cid), StyleAttr "width:100%; height:100%"] []
            , eventHandlers = []
            , width         = ExactSize 600
            , height        = ExactSize 300
            }

	mapdivid cid = "map_div_" +++ cid

	onUpdate cid mbDiff (map,st=:{mapObj=Nothing,libsInserted=False}) world
        # (l, world) = findObject "L" world
        | jsIsUndefined l
            # world = addCSSFromUrl LEAFLET_CSS world
            # world = addJSFromUrl LEAFLET_JS (Just (createEditletEventHandler onLibLoaded cid)) world
            = ((map,{st & libsInserted=True}), world)
        | otherwise
            = onLibLoaded cid Nothing (map,{st & libsInserted=True}) world

    onUpdate cid (Just [LDSetZoom zoom:diffs]) (map,st=:{mapObj=Just mapobj}) world
        # (_,world) = callObjectMethod "setZoom" [toJSArg zoom] mapobj world
        = onUpdate cid (Just diffs) (map,st) world
    onUpdate cid (Just [LDSetCenter {LeafletLatLng|lat,lng}:diffs]) (map,st=:{mapObj=Just mapobj}) world
        # (_,world) = callObjectMethod "panTo" [toJSArg [lat,lng]] mapobj world
        = onUpdate cid (Just diffs) (map,st) world
    onUpdate cid (Just [LDSetCursor (Just {LeafletLatLng|lat,lng}):diffs]) (map,st=:{mapObj=Just mapobj,mapCursor=Nothing}) world
        # (l, world)        = findObject "L" world
        # (mapCursor,world) = callObjectMethod "circleMarker" [toJSArg [lat,lng],toJSArg CURSOR_OPTIONS] l world
        # (_,world)         = callObjectMethod "addTo" [toJSArg mapobj] mapCursor world
        = onUpdate cid (Just diffs) (map,{st & mapCursor = Just mapCursor}) world
    onUpdate cid (Just [LDSetCursor (Just {LeafletLatLng|lat,lng}):diffs]) (map,st=:{mapObj=Just mapobj,mapCursor=Just cursorobj}) world
        # (_,world)         = callObjectMethod "setLatLng" [toJSArg [lat,lng]] cursorobj world
        = onUpdate cid (Just diffs) (map,st) world
    onUpdate cid (Just [LDSetCursor Nothing:diffs]) (map,st=:{mapObj=Just mapobj,mapCursor=Just cursorobj}) world
        # (_,world)     = callObjectMethod "removeLayer" [toJSArg cursorobj] mapobj world
        = onUpdate cid (Just diffs) (map ,{st & mapCursor = Nothing}) world
    onUpdate cid (Just [LDUpdateLayer idx (ObjectLayer objects):diffs]) (map,st=:{mapObj=Just mapobj,mapIcons=Just mapicons, mapLayers=Just maplayers}) world
        # (l, world)    = findObject "L" world
        # (layer,world) = jsGetObjectEl idx maplayers world
        # (_,world)     = callObjectMethod "removeLayer" [toJSArg layer] mapobj world
        # (layer,world) = callObjectMethod "layerGroup" [] l world
        # world         = foldl (\w object -> createObject object l layer mapicons cid w) world objects
        # world         = jsSetObjectEl idx layer maplayers world
        # (_,world)     = callObjectMethod "addTo" [toJSArg mapobj] layer world
        = onUpdate cid (Just diffs) (map,st) world
    onUpdate cid (Just [LDAddLayers [TileLayer url:layers]:diffs]) (map,st=:{mapObj=Just mapobj,mapIcons=Just mapicons, mapLayers=Just maplayers}) world
        # (l, world)    = findObject "L" world
        # (layer,world) = callObjectMethod "tileLayer" [toJSArg url] l world
        # (_,world)     = jsArrayPush layer maplayers world
        # (_,world)     = callObjectMethod "addTo" [toJSArg mapobj] layer world
        = onUpdate cid (Just [LDAddLayers layers:diffs]) (map,st) world
    onUpdate cid (Just [LDAddLayers [ObjectLayer objects:layers]:diffs]) (map,st=:{mapObj=Just mapobj,mapIcons=Just mapicons, mapLayers=Just maplayers}) world
        # (l, world)    = findObject "L" world
        # (layer,world) = callObjectMethod "layerGroup" [] l world
        # world         = foldl (\w object -> createObject object l layer mapicons cid w) world objects
        # (_,world)     = jsArrayPush layer maplayers world
        = onUpdate cid (Just [LDAddLayers layers:diffs]) (map,st) world
    onUpdate cid (Just [LDAddLayers []:diffs]) (map,st) world
        = onUpdate cid (Just diffs) (map,st) world
    onUpdate cid _ (map,st) world
        = ((map,st),world)

    onLibLoaded cid _ (map=:{LeafletMap|perspective,icons,layers},_) world
        # (l, world) = findObject "L" world
        //Create map
        # (mapobj, world) = callObjectMethod "map" [toJSArg (mapdivid cid)] l world
        //Set perspective
        # (center,world) = toJSArray [perspective.center.lat,perspective.center.lng] world
        # (_,world) = callObjectMethod "setView" [toJSArg center, toJSArg perspective.LeafletPerspective.zoom] mapobj world
        //Create cursor
        # (mapCursor,world) = case perspective.cursor of
            Nothing     = (Nothing,world)
            Just {LeafletLatLng|lat,lng}
                # (cursorobj,world) = callObjectMethod "circleMarker" [toJSArg [lat,lng],toJSArg CURSOR_OPTIONS] l world
                # (_,world)         = callObjectMethod "addTo" [toJSArg mapobj] cursorobj world
                = (Just cursorobj,world)
        //Add icons
        # (mapicons,world) = newJSArray world
        # world = foldl (\w icon -> createIcon icon l mapicons w) world icons
        //Add initial layers
        # (maplayers,world) = newJSArray world
        # world = foldl (\w layer -> createLayer layer l mapobj maplayers mapicons cid w) world layers
        //Add map event handlers
        # (_, world) = callObjectMethod "addEventListener" [toJSArg "dragend",toJSArg (createEditletEventHandler onMapMove cid)] mapobj world
        # (_, world) = callObjectMethod "addEventListener" [toJSArg "zoomend",toJSArg (createEditletEventHandler onZoomChange cid)] mapobj world
        # (_, world) = callObjectMethod "addEventListener" [toJSArg "click",toJSArg (createEditletEventHandler onMapClick cid)] mapobj world
        //Add editlet event handler
        # (editlets,world)  = findObject "itwc.controller.editlets" world
        # (cmp,world)       = jsGetObjectAttr cid editlets world
        # world             = jsSetObjectAttr "afterShow" (toJSVal (createEditletEventHandler onAfterShow cid)) cmp world
        = ((map,{mapObj=Just mapobj,mapIcons=Just mapicons,mapLayers=Just maplayers,mapCursor=mapCursor,libsInserted=True}),world)
    createIcon {LeafletIcon|iconUrl,iconSize=(w,h)} l mapicons world
        # (icon,world)  = callObjectMethod "icon" [toJSArg {IconOptions|iconUrl=iconUrl,iconSize=[w,h]}] l world
        # (_, world)    = jsArrayPush icon mapicons world
        = world
    createLayer (TileLayer url) l mapobj maplayers mapicons cid world
        # (layer,world) = callObjectMethod "tileLayer" [toJSArg url] l world
        # (_,world)     = jsArrayPush layer maplayers world
        # (_,world)     = callObjectMethod "addTo" [toJSArg mapobj] layer world
        = world
    createLayer (ObjectLayer objects) l mapobj maplayers mapicons cid world
        # (layer,world) = callObjectMethod "layerGroup" [] l world
        # world         = foldl (\w object -> createObject object l layer mapicons cid w) world objects
        # (_,world)     = jsArrayPush layer maplayers world
        # (_,world)     = callObjectMethod "addTo" [toJSArg mapobj] layer world
        = world

    createObject :: !LeafletObject !(JSVal a) !(JSVal b) !(JSVal c) !String !*JSWorld -> *JSWorld
    createObject (Marker {LeafletMarker|markerId,position,title,icon}) l layer mapicons cid world
        # (markerPos,world) = toJSArray [position.lat,position.lng] world
        # (args,world)      = case icon of
            Nothing         = ([toJSArg markerPos],world)
            Just iconIdx
                # (options,world) = jsEmptyObject world
                # (iconRef,world) = jsGetObjectEl iconIdx mapicons world
                # world = jsSetObjectAttr "icon" iconRef options world
                = ([toJSArg markerPos,toJSArg options],world)
        # (marker,world) = callObjectMethod "marker" args l world
        # (_,world)      = callObjectMethod "addEventListener" [toJSArg "click",toJSArg (createEditletEventHandler (onMarkerClick markerId) cid)] marker world
        # (_,world)      = callObjectMethod "addTo" [toJSArg layer] marker world
        = world
    onMapMove cid event (map=:{LeafletMap|perspective},mbSt) world
        # (mapobj,world) = jsGetObjectAttr "target" event world
        # (latlng,world) = callObjectMethod "getCenter" [] mapobj world
        # (center,world) = getPos latlng world
        = (({LeafletMap|map & perspective = {perspective & center = center}},mbSt),world)
    onZoomChange cid event (map=:{LeafletMap|perspective},mbSt) world
        # (mapobj,world)    = jsGetObjectAttr "target" event world
        # (zoom,world)      = callObjectMethod "getZoom" [] mapobj world
        = (({LeafletMap|map & perspective = {perspective & zoom = jsValToInt zoom}},mbSt),world)
    onMapClick cid event (map=:{LeafletMap|perspective},st=:{mapObj=Just mapobj,mapCursor=Nothing}) world
        # (l, world)        = findObject "L" world
        # (latlng,world)    = jsGetObjectAttr "latlng" event world
        # (cursor,world)    = getPos latlng world
        # (mapCursor,world) = callObjectMethod "circleMarker" [toJSArg latlng,toJSArg CURSOR_OPTIONS] l world
        # (_,world)         = callObjectMethod "addTo" [toJSArg mapobj] mapCursor world
        = (({LeafletMap|map & perspective = {perspective & cursor = Just cursor}},{st & mapCursor = Just mapCursor}),world)
    onMapClick cid event (map=:{LeafletMap|perspective},st=:{mapObj=Just mapobj,mapCursor=Just mapCursor}) world
        # (latlng,world)    = jsGetObjectAttr "latlng" event world
        # (cursor,world)    = getPos latlng world
        # (_,world)         = callObjectMethod "setLatLng" [toJSArg latlng] mapCursor world
        = (({LeafletMap|map & perspective = {perspective & cursor = Just cursor}},st),world)

    onMarkerClick markerId cid event (map=:{LeafletMap|layers},st) world
        # layers = [selectMarker l \\ l <- layers]
        = (({map & layers = layers},st),world)
    where
        selectMarker (ObjectLayer objects)
            = ObjectLayer [Marker {m & selected = m.markerId == markerId} \\ Marker m <- objects]
        selectMarker l = l

    onAfterShow cid event (map,st=:{mapObj=Just mapobj}) world
        # (_,world) = callObjectMethod "invalidateSize" [] mapobj world
        = ((map,st),world)
    onAfterShow cid event st world
        = (st,world)

	getPos obj world
		# (lat, world) = jsGetObjectAttr "lat" obj world
		# (lng, world) = jsGetObjectAttr "lng" obj world
		= ({LeafletLatLng|lat=jsValToReal lat,lng=jsValToReal lng}, world)

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

derive JSONEncode       LeafletMap, LeafletPerspective, LeafletIcon, LeafletLatLng, LeafletLayer, LeafletObject, LeafletMarker, LeafletDiff
derive JSONDecode       LeafletMap, LeafletPerspective, LeafletIcon, LeafletLatLng, LeafletLayer, LeafletObject, LeafletMarker, LeafletDiff
derive gDefault         LeafletMap,                     LeafletIcon, LeafletLatLng, LeafletLayer, LeafletObject, LeafletMarker, LeafletDiff
derive gEq              LeafletMap, LeafletPerspective, LeafletIcon,                LeafletLayer, LeafletObject, LeafletMarker, LeafletDiff
derive gVisualizeText   LeafletMap, LeafletPerspective, LeafletIcon, LeafletLatLng, LeafletLayer, LeafletObject, LeafletMarker, LeafletDiff
derive gEditor                      LeafletPerspective, LeafletIcon, LeafletLatLng, LeafletLayer, LeafletObject, LeafletMarker, LeafletDiff
derive gEditMeta        LeafletMap, LeafletPerspective, LeafletIcon, LeafletLatLng, LeafletLayer, LeafletObject, LeafletMarker, LeafletDiff
derive gUpdate                      LeafletPerspective, LeafletIcon, LeafletLatLng, LeafletLayer, LeafletObject, LeafletMarker, LeafletDiff
derive gVerify	                    LeafletPerspective, LeafletIcon, LeafletLatLng, LeafletLayer, LeafletObject, LeafletMarker, LeafletDiff
