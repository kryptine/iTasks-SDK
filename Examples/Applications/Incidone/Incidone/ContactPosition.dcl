definition module Incidone.ContactPosition
//This module provides a type for representing geographic positions of contacts
import iTasks
import iTasks.API.Extensions.GIS.Leaflet

from Incidone.OP.Concepts import :: Contact, :: ContactGeo

:: ContactPosition
    = PositionDescription String (Maybe (Real,Real))
    | PositionLatLng (Real,Real)

//Abstract maps
:: ContactMap =
    { perspective   :: ContactMapPerspective
    , layers        :: [ContactMapLayer]
    }

:: ContactMapPerspective =
    { center        :: (!Real,!Real)
    , zoom          :: !Int
    , cursor        :: !Maybe (!Real,!Real)
    , bounds        :: !Maybe ContactBounds
    }
:: ContactBounds    :== ((!Real,!Real),(!Real,!Real))

:: ContactMapLayer =
    { title         :: String
    , def           :: ContactMapLayerDefinition
    }

:: ContactMapLayerDefinition
    = CMTileLayer    !String //Url in the form "/path-to-tiles/{z}/{x}/{y}.png"
    | CMRegionsLayer ![ContactMapRegion]
    | CMMarkersLayer ![ContactMapMarker]

:: ContactMapRegion =
    { regionId      :: !String
    , title         :: !String
    , color         :: !String
    , points        :: ![ContactPosition]
    }

:: ContactMapMarker =
    { markerId      :: !String
    , title         :: !Maybe String
    , position      :: !ContactPosition
    , type          :: !Maybe ContactMapMarkerType
    , heading       :: !Maybe Int
    , track         :: !Maybe ContactTrack
    , selected      :: !Bool
    }

:: ContactMapMarkerType
    = CMAIS         //AIS contact
    | CMUnit        //Coastguard controlled unit
    | CMNeedsHelp   //Contact needing help
    | CMOther       //Other types of contacts

:: ContactTrack     = ContactTrack [(DateTime,Real,Real)]

withinBounds                :: ContactBounds ContactPosition -> Bool

latLng                      :: ContactPosition -> Maybe (Real,Real)

contactToMapMarker          :: Bool Bool Contact -> ContactMapMarker
contactGeoToMapMarker       :: Bool Bool ContactGeo -> ContactMapMarker

toLeafletMap                :: ContactMap -> LeafletMap
toLeafletPerspective        :: ContactMapPerspective -> LeafletPerspective

fromLeafletMap              :: ContactMap LeafletMap -> ContactMap
fromLeafletPerspective      :: LeafletPerspective -> ContactMapPerspective

selectionFromLeafletMap     :: LeafletMap -> [String]

//Standard layers available to use in all map views
standardPerspective         :: Shared ContactMapPerspective
standardMapLayers           :: Shared [ContactMapLayer]

derive JSONEncode ContactPosition, ContactMapPerspective
derive JSONDecode ContactPosition, ContactMapPerspective

derive gEditor ContactPosition, ContactMapPerspective
derive gEditMeta ContactPosition, ContactMapPerspective
derive gVerify ContactPosition, ContactMapPerspective

derive gText ContactPosition, ContactMapPerspective

derive gDefault ContactPosition, ContactMapPerspective
derive gEq ContactPosition, ContactMapPerspective

derive class iTask ContactMap, ContactMapLayer, ContactMapRegion, ContactMapMarker, ContactMapMarkerType, ContactTrack

