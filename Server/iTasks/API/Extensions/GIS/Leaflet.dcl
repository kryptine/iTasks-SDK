definition module iTasks.API.Extensions.GIS.Leaflet

import iTasks

//:: JSLM = JSLM

:: LeafletMap =
    { perspective   :: LeafletPerspective
    , layers        :: [LeafletLayer]
    , icons         :: [LeafletIcon]    //Custom icons used in this map
    }
:: LeafletPerspective =
    { center        :: LeafletLatLng
    , zoom          :: Int
    , cursor        :: Maybe LeafletLatLng
    , bounds        :: Maybe LeafletBounds
    }
:: LeafletIcon =
    { iconUrl       :: String
    , iconSize      :: (!Int,!Int)
    }

:: LeafletLatLng =
    { lat :: !Real
    , lng :: !Real
    }
:: LeafletBounds =
    { southWest :: !LeafletLatLng
    , northEast :: !LeafletLatLng
    }

:: LeafletLayer
    = TileLayer String
    | ObjectLayer [LeafletObject]

:: LeafletObject
    = Marker LeafletMarker
    | Polyline LeafletPolyline
    | Polygon LeafletPolygon

:: LeafletMarker =
    { markerId      :: String
    , position      :: LeafletLatLng
    , title         :: Maybe String
    , icon          :: Maybe Int        //Index in the list of icons defined for the map
    , selected      :: Bool
    }

:: LeafletPolyline =
    { polylineId    :: !String
    , points        :: ![LeafletLatLng]
    , strokeColor   :: !String       //html/css color definition
    , strokeWidth   :: !Int
    }

:: LeafletPolygon =
    { polygonId     :: !String
    , points        :: ![LeafletLatLng]
    , strokeColor   :: !String       //html/css color definition
    , strokeWidth   :: !Int
    , fillColor     :: !Maybe String //Nothing means no fill
    }


openStreetMapTiles :: LeafletLayer

derive JSONEncode       LeafletMap, LeafletPerspective, LeafletIcon, LeafletLatLng, LeafletBounds, LeafletLayer, LeafletObject, LeafletMarker, LeafletPolyline, LeafletPolygon
derive JSONDecode       LeafletMap, LeafletPerspective, LeafletIcon, LeafletLatLng, LeafletBounds, LeafletLayer, LeafletObject, LeafletMarker, LeafletPolyline, LeafletPolygon
derive gDefault         LeafletMap, LeafletPerspective, LeafletIcon, LeafletLatLng, LeafletBounds, LeafletLayer, LeafletObject, LeafletMarker, LeafletPolyline, LeafletPolygon
derive gEq              LeafletMap, LeafletPerspective, LeafletIcon, LeafletLatLng, LeafletBounds, LeafletLayer, LeafletObject, LeafletMarker, LeafletPolyline, LeafletPolygon
derive gText            LeafletMap, LeafletPerspective, LeafletIcon, LeafletLatLng, LeafletBounds, LeafletLayer, LeafletObject, LeafletMarker, LeafletPolyline, LeafletPolygon
derive gEditor          LeafletMap, LeafletPerspective, LeafletIcon, LeafletLatLng, LeafletBounds, LeafletLayer, LeafletObject, LeafletMarker, LeafletPolyline, LeafletPolygon
derive gEditMeta        LeafletMap, LeafletPerspective, LeafletIcon, LeafletLatLng, LeafletBounds, LeafletLayer, LeafletObject, LeafletMarker, LeafletPolyline, LeafletPolygon
derive gUpdate          LeafletMap, LeafletPerspective, LeafletIcon, LeafletLatLng, LeafletBounds, LeafletLayer, LeafletObject, LeafletMarker, LeafletPolyline, LeafletPolygon
derive gVerify	        LeafletMap, LeafletPerspective, LeafletIcon, LeafletLatLng, LeafletBounds, LeafletLayer, LeafletObject, LeafletMarker, LeafletPolyline, LeafletPolygon

