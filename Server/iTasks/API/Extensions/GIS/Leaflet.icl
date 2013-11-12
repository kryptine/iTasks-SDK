implementation module iTasks.API.Extensions.GIS.Leaflet

import iTasks
import iTasks.API.Core.Client.Editlet
import StdMisc

LEAFLET_JS :== "leaflet-0.6.3/leaflet.js"
LEAFLET_CSS :== "leaflet-0.6.3/leaflet.css"

:: LeafletClientState =
    {mapObj         :: Maybe (JSVal JSObject)
    ,libsInserted   :: Bool
    }

openStreetMapTiles :: LeafletLayer
openStreetMapTiles = TileLayer "http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png"
	
leafletEditlet :: LeafletMap -> Editlet LeafletMap LeafletMap
leafletEditlet map = Editlet map
    { EditletServerDef
    | genUI		= \cid world -> (uiDef cid, world)
    , defVal 	= map
    , genDiff	= genDiff
    , appDiff	= appDiff
    }
    { EditletClientDef
    | updateUI	= onUpdate
    , defVal 	= (map,{mapObj = Nothing,libsInserted = False})
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
            = ((map,{mapObj=Nothing,libsInserted=True}), world)
        | otherwise
            = onLibLoaded cid Nothing (map,{mapObj=Nothing,libsInserted=True}) world

    onUpdate cid mbDiff (map,st) world
        = ((map,st),world)

    onLibLoaded cid _ (map=:{LeafletMap|perspective,layers},_) world
        # (l, world) = findObject "L" world
        //Create map
        # (mapobj, world) = callObjectMethod "map" [toJSArg (mapdivid cid)] l world
        //Set perspective
        # (center,world) = toJSArray [perspective.center.lat,perspective.center.lng] world
        # (_,world) = callObjectMethod "setView" [toJSArg center, toJSArg perspective.LeafletPerspective.zoom] mapobj world
        //Add initial layers
        # world = foldl (\w layer -> createLayer layer l mapobj w) world layers
        //Add event handlers
        # (_, world) = callObjectMethod "addEventListener" [toJSArg "dragend",toJSArg (createEditletEventHandler onMapMove cid)] mapobj world
        = ((map,{mapObj=Just mapobj,libsInserted=True}),world)

    createLayer (TileLayer url) l mapobj world
        # (layer,world) = callObjectMethod "tileLayer" [toJSArg url] l world
        # (_,world)     = callObjectMethod "addTo" [toJSArg mapobj] layer world
        = world

    onMapMove cid event (map=:{LeafletMap|perspective},mbSt) world
        # (mapobj,world) = jsGetObjectAttr "target" event world
        # (latlng,world) = callObjectMethod "getCenter" [] mapobj world
        # ((lat,lng),world) = getPos latlng world
        = (({LeafletMap|map & perspective = {perspective & center = {LeafletLatLng|lat=lat,lng=lng}}},mbSt),world)

	getPos obj world
		# (lat, world) = jsGetObjectAttr "lat" obj world
		# (lng, world) = jsGetObjectAttr "lng" obj world
		= ((jsValToReal lat,jsValToReal lng), world)

    genDiff old new
        | old === new   = Nothing
                        = Just new
    appDiff new old = new

gEditor{|LeafletMap|} dp vv=:(val,mask,ver) meta vst
    = gEditor{|*|} dp (leafletEditlet val,mask,ver) meta vst

gUpdate{|LeafletMap|} dp upd (val,mask) iworld
    # ((Editlet value _ _,mask),iworld) = gUpdate{|*|} dp upd (leafletEditlet val,mask) iworld
    = ((value,mask),iworld)

gVerify{|LeafletMap|} _ vst = alwaysValid vst

gDefault{|LeafletPerspective|}
    = {LeafletPerspective|center={LeafletLatLng|lat = 51.82, lng = 5.86},zoom = 7}

derive JSONEncode       LeafletMap, LeafletPerspective, LeafletLatLng, LeafletLayer, LeafletObject
derive JSONDecode       LeafletMap, LeafletPerspective, LeafletLatLng, LeafletLayer, LeafletObject
derive gDefault         LeafletMap,                     LeafletLatLng, LeafletLayer, LeafletObject
derive gEq              LeafletMap, LeafletPerspective, LeafletLatLng, LeafletLayer, LeafletObject
derive gVisualizeText   LeafletMap, LeafletPerspective, LeafletLatLng, LeafletLayer, LeafletObject
derive gEditor                      LeafletPerspective, LeafletLatLng, LeafletLayer, LeafletObject
derive gEditMeta        LeafletMap, LeafletPerspective, LeafletLatLng, LeafletLayer, LeafletObject
derive gUpdate                      LeafletPerspective, LeafletLatLng, LeafletLayer, LeafletObject
derive gVerify	                    LeafletPerspective, LeafletLatLng, LeafletLayer, LeafletObject
