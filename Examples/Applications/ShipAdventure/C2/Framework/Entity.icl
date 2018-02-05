implementation module C2.Framework.Entity

import iTasks
import StdReal, StdOverloaded
import C2.Framework.GeoRoutines
import qualified Data.Map as DM

derive class iTask Entity, Position, RelPos, MovingPos, Structure, RectStruct, Behaviour, Angle

moveEntity :: !Entity -> Entity
moveEntity e = {e & e_position = mv e.e_position}
  where
  mv :: !Position -> Position
  mv (MovingPos mp) = MovingPos (moveEntityPos mp mp.mp_timelate)
  mv x              = x

moveEntityPos :: !MovingPos !Real -> MovingPos
moveEntityPos mp time
  # deltaTime = time - mp.mp_timelate
  # deltaDist = mp.mp_speed * deltaTime
  # newpos    = translate mp.mp_position mp.mp_direction deltaDist
  # newalt    = mp.mp_altitude + mp.mp_vertical_speed * deltaTime
  = {mp & mp_timelate = time, mp_position = newpos, mp_altitude = newalt}

newMovingEntity :: !Int !LatLng -> Entity
newMovingEntity id position = newMovingEntityWithSpeedAndDirection id position 0.0 (rad 0.0)

newMovingEntityWithSpeedAndDirection :: !Int !LatLng !Real !Angle -> Entity
newMovingEntityWithSpeedAndDirection id position speed direction
  = { e_id           = id
    , e_attributes   = 'DM'.newMap
    , e_position     = MovingPos { mp_position         = position
                                 , mp_altitude         = 0.0
                                 , mp_direction        = direction
                                 , mp_speed            = speed
                                 , mp_vertical_speed   = 0.0
                                 , mp_angular_velocity = 0.0
                                 , mp_timelate         = 0.0
                                 }
    , e_subentities  = []
    , e_behaviour    = NoBehaviour
    , e_structure    = NoStructure
    }
