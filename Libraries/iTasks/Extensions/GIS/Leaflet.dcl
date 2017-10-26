definition module iTasks.Extensions.GIS.Leaflet

import iTasks

leafletEditor :: Editor LeafletMap

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
    = Marker   !LeafletMarker
    | Polyline !LeafletPolyline
    | Polygon  !LeafletPolygon
    | Window   !LeafletWindow

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

:: LeafletWindow =
    { windowId     :: !LeafletObjectID
    , initPosition :: !LeafletWindowPos
    , title        :: !String
    , content      :: !HtmlTag
    }

:: LeafletWindowPos = { x :: !Int, y :: !Int }

//Public tileserver of openstreetmaps
openStreetMapTiles :: String

derive JSONEncode       LeafletMap
derive JSONDecode       LeafletMap
derive gDefault         LeafletMap
derive gEq              LeafletMap
derive gText            LeafletMap
derive gEditor          LeafletMap
