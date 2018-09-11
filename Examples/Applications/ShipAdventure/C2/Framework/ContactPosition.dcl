definition module C2.Framework.ContactPosition
//This module provides a type for representing geographic positions of contacts
import iTasks
import iTasks.Extensions.GIS.Leaflet
import iTasks.Extensions.Platform
from C2.Framework.GeoRoutines import :: LatLng
from Math.Geometry import :: Angle

//from ConceptModel import :: Contact

:: ContactPosition
    = PositionDescription String (Maybe (Real,Real))
    | PositionLatLng (Real,Real)

//TILESERVER :== "http://tiles.demo.itasks.org/osm/{z}/{x}/{y}.png"
TILESERVER :== "http://c.tile.openstreetmap.fr/hot/{z}/{x}/{y}.png"
//TILESERVER :== "http://otile3.mqcdn.com/tiles/1.0.0/osm/{z}/{x}/{y}.png"
//TILESERVER :== "/tiles/{z}/{x}/{y}.png"

//Abstract maps
:: ContactMap =
    { perspective   :: ContactMapPerspective
    , markers       :: [ContactMapMarker]
    }

:: ContactMapPerspective =
    { center        :: !LatLng
    , zoom          :: !Int
    , cursor        :: !Maybe LatLng
    }

:: ContactMapMarker =
    { markerId      :: !String
    , title         :: !Maybe String
    , position      :: !LatLng
    , type          :: !Maybe ContactMapMarkerType
    , heading       :: !Maybe Real
    , selected      :: !Bool
    }

:: ContactMapMarkerType
    = CMAIS         //AIS contact
    | CMUnit        //Coastguard controlled unit
    | CMNeedsHelp   //Contact needing help
    | CMOther       //Other types of contacts
    | CMHostile
    | CMSuspect
    | CMSelf
    | CMHVU

leafletMapContactPosition    :: LatLng -> LeafletLatLng

defaultPerspective           :: ContactMapPerspective

toLeafletMap                :: ContactMap -> LeafletMap
toLeafletPerspective        :: ContactMapPerspective -> LeafletPerspective

fromLeafletMap              :: LeafletMap -> ContactMap
fromLeafletPerspective      :: LeafletPerspective -> ContactMapPerspective

derive class iTask ContactMap, ContactMapMarker, ContactMapMarkerType
derive JSONEncode ContactMapPerspective
derive JSONDecode ContactMapPerspective
derive gEditor ContactMapPerspective
derive gText ContactMapPerspective
derive gEq ContactMapPerspective
derive gDefault ContactMapPerspective
