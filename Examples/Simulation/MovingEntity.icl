implementation module MovingEntity

import iTasks, GeoRoutines

::MovingEntity = {id :: Int
                 ,position    :: LatLng
                 ,altitude    :: !Real
                 ,direction   :: !Real
                 ,speed       :: !Real
                 ,vertSpeed   :: !Real
                 ,angVelocity :: !Real                 
                 ,timeLate    :: !Int
                 ,properties  ::  EntityProperties
                 }

:: EntityProperties = {maxSpeed :: !Real
                      ,maxAccel :: !Real
                      }
                 
newMovingEntity :: Int LatLng !Real !Int -> MovingEntity
newMovingEntity id position speed timeLate = {id = id
                                       ,position    = position
                                       ,altitude    = 0.0
                                       ,direction   = 0.0
                                       ,speed       = speed
                                       ,vertSpeed   = 0.0
                                       ,angVelocity = 0.0
                                       ,timeLate    = timeLate
                                       ,properties  = {maxSpeed = 300.0,maxAccel = 40.0}
                                       }
                                       
updatePosition :: MovingEntity !Int -> MovingEntity
updatePosition me=:{position,direction,speed,angVelocity,altitude,vertSpeed,timeLate} time 
= {me & timeLate = time, position = newpos, direction = newdir, altitude = newalt}
where deltaTime                   = toReal (time - timeLate)
      deltaAngle                  = angVelocity * deltaTime
      deltaDist                   = speed * deltaTime
      absAngVel                   = abs angVelocity
      radius                      = angularVelocity2radius angVelocity speed
      posdir | absAngVel < 0.0001 = (translate position direction deltaDist,direction)
             | otherwise          = (translateAlongCurve position direction deltaAngle radius,normalizeDirection (direction + deltaAngle))
      newalt                      = altitude + vertSpeed * deltaTime
      (newpos,newdir)             = posdir
                                       
//distanceToTarget :: MovingEntity LatLng -> Real

moveAlongWayPoints :: MovingEntity [LatLng] Int -> (MovingEntity,[LatLng])
moveAlongWayPoints me                                   []     time = ({me & speed = 0.0},[])
moveAlongWayPoints me=:{direction,position,angVelocity} [p:ps] time  
| dist2wp < deltaDist = moveAlongWayPoints me ps time
| otherwise           = (moveToTarget me p time,[p:ps])
where dist2wp      = distance position p
      deltaDist    = toReal (time - me.timeLate) * me.speed 


// Make entity move in the direction of target pos
moveToTarget :: MovingEntity LatLng !Int -> MovingEntity        
moveToTarget me=:{direction,position,speed,properties} pos time 
= updatePosition {me & direction = newdir,angVelocity = newAngVel} time
where targetdir  = getDirectionToPosition position pos
      deltadir   = deltaDirection direction targetdir
      deltatime  = toReal (time - me.timeLate)
      angVel     = acceleration2angularVelocity properties.maxAccel speed
      newdir | abs deltadir < deltatime * angVel = targetdir
             | otherwise                         = direction 
      newAngVel | abs deltadir < angVel = 0.0
                | deltadir < 0.0        = ~angVel
                | otherwise             = angVel
