definition module GeoRoutines

import StdEnv

:: LatLng :== (!Real,!Real)

earth_radius :: Real

fromDegrees :: (Real,Real) -> LatLng
toDegrees   ::  LatLng ->(Real,Real)

// 3d part
distance :: LatLng LatLng -> Real

getHeadingToPositionDeg :: LatLng LatLng -> Real

getHeadingToPosition :: LatLng LatLng -> Real
                  
translate :: LatLng !Real !Real -> LatLng
      
translateAlongCurve :: LatLng !Real !Real !Real -> LatLng

// 2d part
distanceForTurning :: !Real !Real -> Real

acceleration2radius :: !Real !Real -> Real

angularVelocity2radius :: !Real !Real -> Real

radius2angularVelocity :: !Real !Real -> Real

acceleration2angularVelocity :: !Real !Real -> Real

angularVelocity2acceleration :: !Real !Real -> Real

radius2acceleration :: !Real !Real -> Real

dirDist2vector :: !Real !Real -> (!Real,!Real)

relPosition :: LatLng LatLng -> (!Real,!Real)











