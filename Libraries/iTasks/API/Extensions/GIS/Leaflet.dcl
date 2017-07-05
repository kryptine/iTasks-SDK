definition module iTasks.API.Extensions.GIS.Leaflet

import iTasks

:: LeafletMap =
    { perspective   :: LeafletPerspective 
	, tilesUrl      :: Maybe String      
	, objects       :: [LeafletObject]    //Markers, lines and polygon
    , icons         :: [LeafletIcon]      //Custom icons used by markers. They are indexed by 'iconId' string and cannot be changed once the map is loaded
    }

:: LeafletPerspective =
    { center        :: LeafletLatLng
    , zoom          :: Int
    , cursor        :: Maybe LeafletLatLng
    , bounds        :: Maybe LeafletBounds
    }

:: LeafletIconID :== String
:: LeafletIcon =
    { iconId        :: LeafletIconID
    , iconUrl       :: String
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

:: LeafletObject
    = Marker LeafletMarker
    | Polyline LeafletPolyline
    | Polygon LeafletPolygon

:: LeafletObjectID :== String
:: LeafletMarker =
    { markerId      :: !LeafletObjectID
    , position      :: !LeafletLatLng
    , title         :: !Maybe String
    , icon          :: !Maybe LeafletIconID// Id of the list of icons defined for the map
    , popup         :: !Maybe HtmlTag
    , selected      :: !Bool
    }

:: LeafletPolyline =
    { polylineId    :: !LeafletObjectID
    , points        :: ![LeafletLatLng]
    , strokeColor   :: !String // html/css color definition
    , strokeWidth   :: !Int
    }

:: LeafletPolygon =
    { polygonId     :: !LeafletObjectID
    , points        :: ![LeafletLatLng]
    , strokeColor   :: !String       // html/css color definition
    , strokeWidth   :: !Int
    , fillColor     :: !Maybe String // Nothing means no fill
    }

//Public tileserver of openstreetmaps
openStreetMapTiles :: String

derive JSONEncode       LeafletMap, LeafletPerspective, LeafletIcon, LeafletLatLng, LeafletBounds, LeafletObject, LeafletMarker, LeafletPolyline, LeafletPolygon
derive JSONDecode       LeafletMap, LeafletPerspective, LeafletIcon, LeafletLatLng, LeafletBounds, LeafletObject, LeafletMarker, LeafletPolyline, LeafletPolygon
derive gDefault         LeafletMap, LeafletPerspective, LeafletIcon, LeafletLatLng, LeafletBounds, LeafletObject, LeafletMarker, LeafletPolyline, LeafletPolygon
derive gEq              LeafletMap, LeafletPerspective, LeafletIcon, LeafletLatLng, LeafletBounds, LeafletObject, LeafletMarker, LeafletPolyline, LeafletPolygon
derive gText            LeafletMap, LeafletPerspective, LeafletIcon, LeafletLatLng, LeafletBounds, LeafletObject, LeafletMarker, LeafletPolyline, LeafletPolygon
derive gEditor          LeafletMap, LeafletPerspective, LeafletIcon, LeafletLatLng, LeafletBounds, LeafletObject, LeafletMarker, LeafletPolyline, LeafletPolygon
