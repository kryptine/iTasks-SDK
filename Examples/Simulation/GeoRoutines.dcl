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
            
normalizedlatLong2XYZ :: LatLng -> (!Real,!Real,!Real)

inner3 :: (!Real,!Real,!Real) (!Real,!Real,!Real) -> Real

inner2 :: (!Real,!Real) (!Real,!Real) -> Real

normalizeLatLong :: LatLng -> LatLng
      
translate :: LatLng !Real !Real -> LatLng
      
translateCurve :: LatLng !Real !Real !Real -> LatLng

// 2d part
dist4Corner :: !Real !Real -> Real

acceleration2radius :: !Real !Real -> Real

angularVelocity2radius :: !Real !Real -> Real

radius2angularVelocity :: !Real !Real -> Real

acceleration2angularVelocity :: !Real !Real -> Real

angularVelocity2acceleration :: !Real !Real -> Real

radius2acceleration :: !Real !Real -> Real

dirDist2vector :: !Real !Real -> (!Real,!Real)

dif2 :: (!Real,!Real) (!Real,!Real) -> (!Real,!Real)

normsq :: (!Real,!Real) -> Real

relPosition :: LatLng LatLng -> (!Real,!Real)











