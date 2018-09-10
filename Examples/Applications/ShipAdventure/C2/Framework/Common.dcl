definition module C2.Framework.Common

import iTasks

from Data.IntMap.Strict import :: IntMap
from C2.Framework.Entity import :: Entity
from C2.Framework.ContactPosition import :: ContactMapPerspective

:: EntityMap :== IntMap Entity

:: MapState =
  { perspective :: ContactMapPerspective
  , entities    :: IntMap Entity
  , selection   :: Int
  }

derive class iTask MapState

mapState :: SDSLens () MapState MapState

entityMap :: SDSLens () EntityMap EntityMap

registerEntity :: (Int -> Entity) -> Task Entity

updateEntity :: Int (Entity -> Entity) -> Task ()

contactWithId :: SDSLens Int (Maybe Entity) Entity

selectedContactShare :: SDSLens () (Maybe Entity) Entity

resetMapState :: Task ()

periodicallyUpdateEntity :: !Int -> Task ()

mapView :: (sds () r w) (r -> Bool) User [Entity] -> Task () | iTask r & iTask w & RWShared sds

userMapState :: User -> SDSLens () MapState MapState 
