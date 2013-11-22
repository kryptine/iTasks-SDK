definition module iTasks.API.Extensions.GIS.Leaflet

import iTasks

:: LeafletMap =
    { perspective   :: LeafletPerspective
    , layers        :: [LeafletLayer]
    , icons         :: [LeafletIcon]    //Custom icons used in this map
    }
:: LeafletPerspective =
    { center        :: LeafletLatLng
    , zoom          :: Int
    , cursor        :: Maybe LeafletLatLng
    , bounds        :: Maybe LeafletLatLngBounds
    }
:: LeafletIcon =
    { iconUrl       :: String
    , iconSize      :: (!Int,!Int)
    }

:: LeafletLatLng =
    { lat :: Real
    , lng :: Real
    }
:: LeafletLatLngBounds :== (!LeafletLatLng,!LeafletLatLng)

:: LeafletLayer
    = TileLayer String
    | ObjectLayer [LeafletObject]

:: LeafletObject
    = Marker LeafletMarker

:: LeafletMarker =
    { markerId      :: String
    , position      :: LeafletLatLng
    , title         :: Maybe String
    , icon          :: Maybe Int        //Index in the list of icons defined for the map
    , selected      :: Bool
    }

openStreetMapTiles :: LeafletLayer

derive JSONEncode       LeafletMap, LeafletPerspective, LeafletIcon, LeafletLatLng, LeafletLayer, LeafletObject, LeafletMarker
derive JSONDecode       LeafletMap, LeafletPerspective, LeafletIcon, LeafletLatLng, LeafletLayer, LeafletObject, LeafletMarker
derive gDefault         LeafletMap, LeafletPerspective, LeafletIcon, LeafletLatLng, LeafletLayer, LeafletObject, LeafletMarker
derive gEq              LeafletMap, LeafletPerspective, LeafletIcon, LeafletLatLng, LeafletLayer, LeafletObject, LeafletMarker
derive gVisualizeText   LeafletMap, LeafletPerspective, LeafletIcon, LeafletLatLng, LeafletLayer, LeafletObject, LeafletMarker
derive gEditor          LeafletMap, LeafletPerspective, LeafletIcon, LeafletLatLng, LeafletLayer, LeafletObject, LeafletMarker
derive gEditMeta        LeafletMap, LeafletPerspective, LeafletIcon, LeafletLatLng, LeafletLayer, LeafletObject, LeafletMarker
derive gUpdate          LeafletMap, LeafletPerspective, LeafletIcon, LeafletLatLng, LeafletLayer, LeafletObject, LeafletMarker
derive gVerify	        LeafletMap, LeafletPerspective, LeafletIcon, LeafletLatLng, LeafletLayer, LeafletObject, LeafletMarker

