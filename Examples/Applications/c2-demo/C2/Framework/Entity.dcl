definition module C2.Framework.Entity

import iTasks
from Data.Map import :: Map
from C2.Framework.GeoRoutines import :: LatLng
from Math.Geometry import :: Angle

:: Entity =
  { e_id          :: !Int
  , e_attributes  :: !Map String String
  , e_position    :: !Position
  , e_subentities :: ![Entity]
  , e_behaviour   :: !Behaviour
  , e_structure   :: !Structure
  }

:: Position
  = RelPos    !RelPos
  | MovingPos !MovingPos
  | GeoPos    !LatLng

:: RelPos =
  { rp_x :: !Real
  , rp_y :: !Real
  , rp_z :: !Real
  }

:: MovingPos =
  { mp_position         :: !LatLng
  , mp_altitude         :: !Real
  , mp_direction        :: !Angle
  , mp_speed            :: !Real
  , mp_vertical_speed   :: !Real
  , mp_angular_velocity :: !Real
  , mp_timelate         :: !Real
  }

:: Structure
  = NoStructure
  | RectangleStructure !RectStruct

:: RectStruct =
  { rs_width  :: !Real
  , rs_length :: !Real
  , rs_height :: !Real
  }

:: Behaviour
  = NoBehaviour
  | ScriptBehaviour
  | StandardBehaviour

derive class iTask Entity, Position, RelPos, MovingPos, Structure, RectStruct, Behaviour

moveEntity    :: !Entity -> Entity
moveEntityPos :: !MovingPos !Real -> MovingPos

newMovingEntity :: !Int !LatLng -> Entity
newMovingEntityWithSpeedAndDirection :: !Int !LatLng !Real !Angle -> Entity
