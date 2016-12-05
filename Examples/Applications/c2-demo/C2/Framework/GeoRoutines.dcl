definition module C2.Framework.GeoRoutines

from Math.Geometry import :: Angle, deg, rad
from Data.Maybe import :: Maybe

earthRadius =: 6371000.0

:: LatLng   :== (!Angle, !Angle)
:: Point    :== (!Real, !Real)
:: Speed    :== Real

// distance in meters between two positions
distance :: LatLng LatLng -> Real

// return direction from first position to second position
getDirectionToPosition :: LatLng LatLng -> Angle

// translate from position in direction over distance
translate :: LatLng !Angle !Real -> LatLng

// return the relative position in the plane of second position with respect to first in meters
relPosition :: LatLng LatLng -> (!Real, !Real)

// convert a translation in direction dir of distance dist to (x,y) in meters
dirDist2vector :: !Angle !Real -> (!Real, !Real)

headingToPosition :: !LatLng !LatLng -> Angle

interceptionTime :: !Point !Point !Real -> Maybe Real

interceptionDirection :: !LatLng !Angle !Real !LatLng !Angle !Real -> Maybe Angle

cpaTime :: !LatLng !Angle !Speed !LatLng !Angle !Speed -> Real

cpaDistance :: !LatLng !Angle !Speed !LatLng !Angle !Speed -> Real
