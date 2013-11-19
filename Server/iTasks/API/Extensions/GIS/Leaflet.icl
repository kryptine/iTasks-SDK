implementation module iTasks.API.Extensions.GIS.Leaflet

import iTasks
import iTasks.API.Core.Client.Editlet
import StdMisc

LEAFLET_JS :== "leaflet-0.6.3/leaflet.js"
LEAFLET_CSS :== "leaflet-0.6.3/leaflet.css"

:: LeafletClientState =
    {mapObj         :: Maybe (JSVal JSObject)
    ,mapIcons       :: Maybe (JSVal [JSObject])
    ,libsInserted   :: Bool
    }
:: IconOptions =
    { iconUrl :: String
    , iconSize :: [Int]
    }

openStreetMapTiles :: LeafletLayer
openStreetMapTiles = TileLayer "http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png"
	
leafletEditlet :: LeafletMap -> Editlet LeafletMap LeafletMap
leafletEditlet map = Editlet map
    { EditletServerDef
    | genUI		= \cid world -> (uiDef cid, world)
    , defVal 	= gDefault{|*|}
    , genDiff	= genDiff
    , appDiff	= appDiff
    }
    { EditletClientDef
    | updateUI	= onUpdate
    , defVal 	= (gDefault{|*|},{mapObj = Nothing, mapIcons = Nothing, libsInserted = False})
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

	onUpdate cid mbDiff (map,{mapObj=Nothing,libsInserted=False}) world
        # (l, world) = findObject "L" world
        | jsIsUndefined l
            # world = addCSSFromUrl LEAFLET_CSS world
            # world = addJSFromUrl LEAFLET_JS (Just (createEditletEventHandler onLibLoaded cid)) world
            = ((map,{mapObj=Nothing,mapIcons=Nothing,libsInserted=True}), world)
        | otherwise
            = onLibLoaded cid Nothing (map,{mapObj=Nothing,mapIcons=Nothing,libsInserted=True}) world

    onUpdate cid (Just {LeafletMap|perspective}) (map,st=:{mapObj=Just mapobj}) world
        //Set perspective
        # (center,world) = toJSArray [perspective.center.lat,perspective.center.lng] world
        # world     = jsTrace "Map update" world
        # world     = jsTrace center world
        # (_,world) = callObjectMethod "setView" [toJSArg center, toJSArg perspective.LeafletPerspective.zoom] mapobj world
        = ((map,st),world)

    onUpdate cid mbDiff (map,st) world
        = ((map,st),world)

    onLibLoaded cid _ (map=:{LeafletMap|perspective,icons,layers},_) world
        # (l, world) = findObject "L" world
        //Create map
        # (mapobj, world) = callObjectMethod "map" [toJSArg (mapdivid cid)] l world
        //Set perspective
        # (center,world) = toJSArray [perspective.center.lat,perspective.center.lng] world
        # (_,world) = callObjectMethod "setView" [toJSArg center, toJSArg perspective.LeafletPerspective.zoom] mapobj world
        //Add icons
        # (mapicons,world) = newJSArray world
        # world = foldl (\w icon -> createIcon icon l mapicons w) world icons
        //Add initial layers
        # world = foldl (\w layer -> createLayer layer l mapobj mapicons w) world layers
        //Add event handlers
        # (_, world) = callObjectMethod "addEventListener" [toJSArg "dragend",toJSArg (createEditletEventHandler onMapMove cid)] mapobj world
        # (_, world) = callObjectMethod "addEventListener" [toJSArg "zoomend",toJSArg (createEditletEventHandler onZoomChange cid)] mapobj world
        = ((map,{mapObj=Just mapobj,mapIcons=Just mapicons,libsInserted=True}),world)

    createIcon {LeafletIcon|iconUrl,iconSize=(w,h)} l mapicons world
        # (icon,world)  = callObjectMethod "icon" [toJSArg {IconOptions|iconUrl=iconUrl,iconSize=[w,h]}] l world
        # (_, world)    = jsArrayPush icon mapicons world
        = world

    createLayer (TileLayer url) l mapobj mapicons world
        # (layer,world) = callObjectMethod "tileLayer" [toJSArg url] l world
        # (_,world)     = callObjectMethod "addTo" [toJSArg mapobj] layer world
        = world
    createLayer (ObjectLayer objects) l mapobj mapicons world
        # (layer,world) = callObjectMethod "layerGroup" [] l world
        # world         = foldl (\w object -> createObject object l layer mapicons w) world objects
        # (_,world)     = callObjectMethod "addTo" [toJSArg mapobj] layer world
        = world

    createObject (Marker {LeafletMarker|markerId,position,title,icon}) l layer mapicons world
        # (markerPos,world) = toJSArray [position.lat,position.lng] world
        # (args,world)      = case icon of
            Nothing         = ([toJSArg markerPos],world)
            Just iconIdx
                # (options,world) = jsEmptyObject world
                # (iconRef,world) = jsGetObjectEl iconIdx mapicons world
                # world = jsSetObjectAttr "icon" iconRef options world
                = ([toJSArg markerPos,toJSArg options],world)
        # (marker,world) = callObjectMethod "marker" args l world
        # (_,world)      = callObjectMethod "addTo" [toJSArg layer] marker world
        = world
    onMapMove cid event (map=:{LeafletMap|perspective},mbSt) world
        # (mapobj,world) = jsGetObjectAttr "target" event world
        # (latlng,world) = callObjectMethod "getCenter" [] mapobj world
        # ((lat,lng),world) = getPos latlng world
        = (({LeafletMap|map & perspective = {perspective & center = {LeafletLatLng|lat=lat,lng=lng}}},mbSt),world)
    onZoomChange cid event (map=:{LeafletMap|perspective},mbSt) world
        # (mapobj,world)    = jsGetObjectAttr "target" event world
        # (zoom,world)      = callObjectMethod "getZoom" [] mapobj world
        = (({LeafletMap|map & perspective = {perspective & zoom = jsValToInt zoom}},mbSt),world)

	getPos obj world
		# (lat, world) = jsGetObjectAttr "lat" obj world
		# (lng, world) = jsGetObjectAttr "lng" obj world
		= ((jsValToReal lat,jsValToReal lng), world)

    genDiff old new = if(old === new) Nothing (Just new)
    appDiff new old = new

gEditor{|LeafletMap|} dp vv=:(val,mask,ver) meta vst
    = gEditor{|*|} dp (leafletEditlet val,mask,ver) meta vst

gUpdate{|LeafletMap|} dp upd (val,mask) iworld
    # ((Editlet value _ _,mask),iworld) = gUpdate{|*|} dp upd (leafletEditlet val,mask) iworld
    = ((value,mask),iworld)

gVerify{|LeafletMap|} _ vst = alwaysValid vst

gDefault{|LeafletPerspective|}
    = {LeafletPerspective|center={LeafletLatLng|lat = 51.82, lng = 5.86},zoom = 7}

//Comparing reals may have unexpected results!!!
//Especially when comparing constants to previously stored ones
gEq{|LeafletLatLng|} x y
    = (toString x.lat == toString y.lat) && (toString x.lng == toString y.lng)

derive JSONEncode       LeafletMap, LeafletPerspective, LeafletIcon, LeafletLatLng, LeafletLayer, LeafletObject, LeafletMarker
derive JSONDecode       LeafletMap, LeafletPerspective, LeafletIcon, LeafletLatLng, LeafletLayer, LeafletObject, LeafletMarker
derive gDefault         LeafletMap,                     LeafletIcon, LeafletLatLng, LeafletLayer, LeafletObject, LeafletMarker
derive gEq              LeafletMap, LeafletPerspective, LeafletIcon,                LeafletLayer, LeafletObject, LeafletMarker
derive gVisualizeText   LeafletMap, LeafletPerspective, LeafletIcon, LeafletLatLng, LeafletLayer, LeafletObject, LeafletMarker
derive gEditor                      LeafletPerspective, LeafletIcon, LeafletLatLng, LeafletLayer, LeafletObject, LeafletMarker
derive gEditMeta        LeafletMap, LeafletPerspective, LeafletIcon, LeafletLatLng, LeafletLayer, LeafletObject, LeafletMarker
derive gUpdate                      LeafletPerspective, LeafletIcon, LeafletLatLng, LeafletLayer, LeafletObject, LeafletMarker
derive gVerify	                    LeafletPerspective, LeafletIcon, LeafletLatLng, LeafletLayer, LeafletObject, LeafletMarker
