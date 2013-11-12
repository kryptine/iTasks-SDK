definition module iTasks.API.Extensions.GIS.Leaflet

import iTasks

:: LeafletMap =
    { perspective   :: LeafletPerspective
    , layers        :: [LeafletLayer]
    }
:: LeafletPerspective =
    { center        :: LeafletLatLng
    , zoom          :: Int
    }
:: LeafletLatLng =
    { lat :: Real
    , lng :: Real
    }

:: LeafletLayer
    = TileLayer String
    | ObjectLayer [LeafletObject]

:: LeafletObject
    = Marker LeafletLatLng

openStreetMapTiles :: LeafletLayer

derive JSONEncode       LeafletMap, LeafletPerspective, LeafletLatLng, LeafletLayer
derive JSONDecode       LeafletMap, LeafletPerspective, LeafletLatLng, LeafletLayer
derive gDefault         LeafletMap, LeafletPerspective, LeafletLatLng, LeafletLayer
derive gEq              LeafletMap, LeafletPerspective, LeafletLatLng, LeafletLayer
derive gVisualizeText   LeafletMap, LeafletPerspective, LeafletLatLng, LeafletLayer
derive gEditor          LeafletMap, LeafletPerspective, LeafletLatLng, LeafletLayer
derive gEditMeta        LeafletMap, LeafletPerspective, LeafletLatLng, LeafletLayer
derive gUpdate          LeafletMap, LeafletPerspective, LeafletLatLng, LeafletLayer
derive gVerify	        LeafletMap, LeafletPerspective, LeafletLatLng, LeafletLayer

