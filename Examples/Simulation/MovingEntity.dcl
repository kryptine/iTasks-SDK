definition module MovingEntity

import GeoRoutines, iTasks

::MovingEntity = {id          :: !Int
                 ,position    :: (!Real,!Real)
                 ,altitude    :: Real
                 ,direction   :: Real
                 ,speed       :: Real
                 ,vertSpeed   :: Real
                 ,angVelocity :: Real                 
                 ,timeLate    :: Int
                 ,properties  ::  EntityProperties
                 }

:: EntityProperties = {maxSpeed :: Real
                      ,maxAccel :: Real
                      }

newMovingEntity :: Int LatLng !Real !Int -> MovingEntity

moveAlongWayPoints :: MovingEntity [LatLng] Int Int -> (MovingEntity,Int)
moveAlongWayPointsOld    :: MovingEntity [LatLng] Int -> (MovingEntity,[LatLng])

updatePosition :: MovingEntity !Int -> MovingEntity

moveToTarget :: MovingEntity LatLng !Int -> MovingEntity        
