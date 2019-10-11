implementation module C2.Framework.Common

import iTasks

from Data.IntMap.Strict import :: IntMap
import qualified Data.IntMap.Strict as DIS
import C2.Framework.Core
import C2.Apps.ShipAdventure.Types
import C2.Framework.Util
import C2.Framework.Entity
import C2.Framework.ContactPosition
import Math.Geometry

derive class iTask MapState

defSettings :: MapState
defSettings =
  { perspective = { ContactMapPerspective
                  | center = (deg 1.442384, deg 46.590828)
                  , zoom   = 8
                  , cursor = Nothing
                  }
  , entities    = 'DIS'.newMap
  , selection   = -1
  }

mapState :: SimpleSDSLens MapState
mapState = sharedStore "mapState" defSettings

selectedContactShare :: SDSLens () (Maybe Entity) Entity
selectedContactShare = sdsLens "selectedContactShare" (const ()) (SDSRead read) (SDSWrite write) (SDSNotify notify) Nothing mapState
  where
  read :: () MapState -> MaybeError TaskException (Maybe Entity)
  read _ {selection, entities} = Ok ('DIS'.get selection entities)

  write :: () MapState Entity -> MaybeError TaskException (Maybe MapState)
  write _ st=:{selection, entities} e = Ok (Just {st & entities = 'DIS'.put selection e entities})

  notify :: () MapState Entity -> SDSNotifyPred ()
  notify _ _ _ = \_ _ -> False

userMapState :: User -> SimpleSDSLens MapState
userMapState u = sharedStore ("userMapState" +++ toString u) defSettings

entityMap :: SimpleSDSLens EntityMap
entityMap = sharedStore "entityMap" 'DIS'.newMap

registerEntity :: (Int -> Entity) -> Task Entity
registerEntity mkE
  =          get mapState
  >>- \ms -> let nextIdx = 'DIS'.size ms.entities
                 newE    = mkE nextIdx in
             set {ms & entities = 'DIS'.put nextIdx newE ms.entities} mapState
  >>|        return newE

updateEntity :: Int (Entity -> Entity) -> Task ()
updateEntity n f
  # focus = sdsFocus n contactWithId
  =          get focus
  >>- \mc -> case mc of
               Just e -> set (f e) focus @! ()
               _      -> return ()

contactWithId :: SDSLens Int (Maybe Entity) Entity
contactWithId = sdsLens "contactWithId" (const ()) (SDSRead read) (SDSWrite write) (SDSNotify notify) Nothing mapState
  where
  read :: Int MapState -> MaybeError TaskException (Maybe Entity)
  read idx {entities} = Ok ('DIS'.get idx entities)

  write :: Int MapState Entity -> MaybeError TaskException (Maybe MapState)
  write idx st=:{entities} e = Ok (Just {st & entities = 'DIS'.put idx e entities})

  notify :: Int MapState Entity -> SDSNotifyPred Int
  notify idx _ _ = \_ idx` -> idx == idx`

resetMapState :: Task ()
resetMapState = set defSettings mapState @! ()

periodicallyUpdateEntity :: !Int -> Task ()
periodicallyUpdateEntity n = updateEntity n moveEntity // TODO FIXME PERFORMANCE doTaskPeriodically 1 (updateEntity n moveEntity) <<@ NoUserInterface

mapView` :: User [Entity] -> Task ()
mapView` currentUser es = (updateSharedInformation [UpdateSharedAs toMap fromMap (const o Just)] (userMapState currentUser >*< entityMap) @! ())
  where
  toMap :: (MapState, EntityMap) -> LeafletMap
  toMap ({MapState | perspective}, markers)
    = toLeafletMap { ContactMap
                   | perspective = perspective
                   , markers     = map (entityToMarker o snd) ('DIS'.toList markers)
                   }
  fromMap :: (MapState, EntityMap) LeafletMap -> (MapState, EntityMap)
  fromMap (st, entities) leafletMap
    # contactMap = fromLeafletMap leafletMap
    # (es, st) = foldr updMapMarkers (entities, st) contactMap.ContactMap.markers
    = ({MapState | st & perspective = contactMap.ContactMap.perspective}, es)
  updMapMarkers contactMarker (markers, st)
    # mid = toInt contactMarker.ContactMapMarker.markerId
    = case 'DIS'.get mid markers of
        Just m
          //# st = if contactMarker.ContactMapMarker.selected
           //        {st & selection = mid}
            //       st
          //= ('DIS'.put mid {Entity | m & selected = contactMarker.ContactMapMarker.selected} markers, st) // TODO FIXME
          = (markers, st)
        _ = (markers, st)


mapView :: (sds () r w) (r -> Bool) User [Entity] -> Task () | iTask r & iTask w & RWShared sds
mapView sh radarWorks currentUser es = (updateSharedInformation [UpdateSharedAs toMap fromMap (const o Just)] (mapState >*| sh) @! ())
  where
  toMap ({perspective, entities = markers}, shval)
    = toLeafletMap { ContactMap
                   | perspective = perspective
                   , markers     = if (radarWorks shval) (map (entityToMarker o snd) ('DIS'.toList markers)) []
                   }
  fromMap (st=:{entities}, _) leafletMap
    # contactMap = fromLeafletMap leafletMap
    # (es, st) = foldr updMapMarkers (entities, st) contactMap.ContactMap.markers
    = {st & perspective = contactMap.ContactMap.perspective, entities = es}
  updMapMarkers contactMarker (markers, st)
    # mid = toInt contactMarker.ContactMapMarker.markerId
    = case 'DIS'.get mid markers of
        Just m
          //# st = if contactMarker.ContactMapMarker.selected
           //        {st & selection = mid}
            //       st
          //= ('DIS'.put mid {Entity | m & selected = contactMarker.ContactMapMarker.selected} markers, st) TODO FIXME
          = (markers, st)
        _ = (markers, st)

entityToMarker :: Entity -> ContactMapMarker
entityToMarker se
  = { ContactMapMarker
    | markerId = toString se.Entity.e_id
    , title    = Just ("{classification} " +++ toString se.Entity.e_id)
    , position = toPos se.Entity.e_position
    , type     = Just CMOther // TODO "{classification}"
    , heading  = case se.Entity.e_position of
                    MovingPos mp -> Just (toDeg mp.mp_direction)
                    _            -> Nothing
    , selected = False // TODO FIXME se.Entity.selected
    }
    where
    toPos (MovingPos mp) = mp.mp_position
    toPos _              = (deg 0.0, deg 0.0) // TODO FIXME

