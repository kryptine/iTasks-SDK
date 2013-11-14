implementation module iTasks.API.Extensions.GIS.Leaflet

import iTasks
import iTasks.API.Core.Client.Editlet
import StdMisc

LEAFLET_JS :== "leaflet-0.6.3/leaflet.js"
LEAFLET_CSS :== "leaflet-0.6.3/leaflet.css"

openStreetMapTiles :: LeafletLayer
openStreetMapTiles = TileLayer "http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png"

leafletEditlet :: LeafletMap -> Editlet LeafletMap LeafletMap
leafletEditlet map
    = {Editlet
      |value    = map
      ,html     = \cid -> DivTag [IdAttr (mapdivid cid), StyleAttr "width:100%; height:100%"] []
      ,updateUI = updateUI
      ,handlers	= \cid -> []
      ,genDiff	= genDiff
      ,appDiff	= appDiff
      }
where
	mapdivid cid = "map_div_" +++ cid

	updateUI cid Nothing map Nothing world
        # (l, world) = findObject "L" world
        | jsIsUndefined l
            # world = loadLeafletLib cid world
            = (map, Nothing, world)
            = onLibLoaded cid undef map Nothing world

    loadLeafletLib cid world
        # world = addCSSFromUrl LEAFLET_CSS world
        # world = addJSFromUrl LEAFLET_JS (Just (createEditletEventHandler onLibLoaded cid)) world
        = world

    onLibLoaded cid _ map=:{LeafletMap|perspective,layers} _ world
        # (l, world) = findObject "L" world
        //Create map
        # (mapobj, world) = callObjectMethod "map" [toJSArg (mapdivid cid)] l world
        //Set perspective
        # (center,world) = toJSArray [perspective.center.lat,perspective.center.lng] world
        # (_,world) = callObjectMethod "setView" [toJSArg center, toJSArg perspective.LeafletPerspective.zoom] mapobj world
        //Add initial layers
        # world = foldl (\w layer -> createLayer layer l mapobj w) world layers
        = (map,Nothing,world)

    createLayer (TileLayer url) l mapobj world
        # (layer,world) = callObjectMethod "tileLayer" [toJSArg url] l world
        # (_,world)     = callObjectMethod "addTo" [toJSArg mapobj] layer world
        = world

    onScriptLoad cid _ map _ world
        = onScriptLoad id undef map Nothing world

    genDiff old new = Just new
    appDiff new old = new

gEditor{|LeafletMap|} dp vv=:(val,mask,ver) meta vst
    = gEditor{|*|} dp (leafletEditlet val,mask,ver) meta vst

gUpdate{|LeafletMap|} dp upd (val,mask)
    # ({Editlet|value},mask) = gUpdate{|*|} dp upd (leafletEditlet val,mask)
    = (value,mask)

gVerify{|LeafletMap|} _ vst = alwaysValid vst

gDefault{|LeafletPerspective|}
    = {LeafletPerspective|center={LeafletLatLng|lat = 51.82, lng = 5.86},zoom = 11}

derive JSONEncode       LeafletMap, LeafletPerspective, LeafletLatLng, LeafletLayer
derive JSONDecode       LeafletMap, LeafletPerspective, LeafletLatLng, LeafletLayer
derive gDefault         LeafletMap,                     LeafletLatLng, LeafletLayer
derive gEq              LeafletMap, LeafletPerspective, LeafletLatLng, LeafletLayer
derive gVisualizeText   LeafletMap, LeafletPerspective, LeafletLatLng, LeafletLayer
derive gEditor                      LeafletPerspective, LeafletLatLng, LeafletLayer
derive gEditMeta        LeafletMap, LeafletPerspective, LeafletLatLng, LeafletLayer
derive gUpdate                      LeafletPerspective, LeafletLatLng, LeafletLayer
derive gVerify	                    LeafletPerspective, LeafletLatLng, LeafletLayer
