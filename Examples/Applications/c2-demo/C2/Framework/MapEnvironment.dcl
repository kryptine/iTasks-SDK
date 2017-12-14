definition module C2.Framework.MapEnvironment
 
import iTasks
 
import iTasks._Framework.Tonic
import iTasks.API.Extensions.Admin.TonicAdmin
from Data.IntMap.Strict import :: IntMap
import qualified Data.Map as DM
from Data.Map import :: Map
import Data.Generics.GenLexOrd


:: Maps2D      :== [Map2D]                                // enumerate sub-maps, order is assumed `lowest' to `highest', mapID identifies map
:: Map2D         = { mapId       :: !MapID                // a unique identification of this map
                   , map2D       :: ![[Section]]          // a map is a matrix of sections (row-major), at least 1x1
                   , size2D      :: !Size2D               // width and height of map2D
                   , doors2D     :: !Size2D               // width and depth of door (make sure width/height fit in section)
                   , shape2D     :: !Maybe Shape2D        // optional clipping area
                   }
:: Section       = { sectionName :: !String               // descriptive name, use "" if there is none, does not have to be unique
                   , borders     :: !Borders              // the borders of a section
                   , hops        :: ![Coord3D]            // connects up/down (depending on level) with location (Coord2D) on another map (MapID)
                   }
:: Borders       = { n           :: !Border               // the border to the 'north'
                   , e           :: !Border               // the border to the 'east'
                   , s           :: !Border               // the border to the 'south'
                   , w           :: !Border               // the border to the 'west'
                   }
:: MapID       :== String                                 // identification of one map
:: Border        = Open | Door | Wall
:: Size2D      :== (!Real, !Real)                         // width and height
:: Shape2D     :== [(!Real, !Real)]                       // outline in terms of Map2D.size coordinates (origin at left-top, max at right-bottom)
:: Maps2DIndex :== Int                                    // index in Maps2D (0..length Maps2D-1)
:: Coord2D       = { col         :: !Int                  // x-coordinate (0.., identifies column)
                   , row         :: !Int                  // y-coordinate (0.., identifies row)
                   }
:: Coord3D     :== (!Maps2DIndex, !Coord2D)               // (index in Maps2D, {col,row} in map)
:: Dir           = N | E | W | S                          // north, east, west, south

:: Graph       :== Map Coord3D [(!Maybe Dir, !Coord3D)]

/********************************************************************************************************************
*
*	Editing the layout of a map
*
********************************************************************************************************************/

:: MapAction s
  = FocusOnMap     !Maps2DIndex
  | FocusOnSection !Coord3D
  | ToggleDoor     !Coord3D !Dir
  | ToggleHop      !Coord3D !Coord3D
  | SetStatus      !Coord3D !s
  | NoAction

derive class iTask Map2D, Section, Borders, Border, Coord2D, Dir, MapAction
instance toString Coord2D
instance toString Coord3D
instance toString Dir
instance ==       Dir
instance ==       Coord2D
instance <        Coord2D
instance zero     Coord2D
derive gLexOrd    Coord2D
instance == Border

:: Object objType =
  { objId    :: !ObjectId
  , objType  :: !objType
  }

:: ObjectId   :== Int
:: Locked     :== Bool
:: Weight     :== Int
:: Distance   :== Int

:: Actor obj act = { userName    :: !User         // all actors should have unique names !!
                   , carrying    :: ![Object obj] // can be anything
                   , actorStatus :: !act          // can be anything
                   }

:: UserActorMap   objType actorStatus        :== Map User (Actor objType actorStatus)
:: UserActorShare objType actorStatus        :== RWShared ()   (UserActorMap objType actorStatus)  (UserActorMap objType actorStatus)
:: FocusedUserActorShare objType actorStatus :== RWShared User (Maybe (Actor objType actorStatus)) (Actor objType actorStatus)

:: SectionStatusMap    roomStatus          :== Map Coord3D roomStatus // [Coord3D |-> roomStatus]
:: SectionInventoryMap objType             :== Map Coord3D (IntMap (Object objType)) // [Coord3D |-> [ObjId |-> Object]]
:: SectionUsersMap                         :== Map Coord3D [User]
:: SectionExitLockMap                      :== Map Coord3D [Dir]
:: SectionHopLockMap                       :== Map Coord3D [Coord3D]


:: SectionStatusShare           roomStatus          :== RWShared ()      (SectionStatusMap roomStatus) (SectionStatusMap roomStatus) // [Coord3D |-> roomStatus]
:: SectionInventoryShare        objType             :== RWShared ()      (SectionInventoryMap objType) (SectionInventoryMap objType) // [Coord3D |-> [ObjId |-> Object]]
:: SectionUsersShare                                :== RWShared ()      SectionUsersMap               SectionUsersMap
:: FocusedSectionStatusShare    roomStatus          :== RWShared Coord3D roomStatus                    roomStatus // [Coord3D |-> roomStatus]
:: FocusedSectionInventoryShare objType             :== RWShared Coord3D (IntMap (Object objType))     (IntMap (Object objType)) // [Coord3D |-> [ObjId |-> Object]]
:: FocusedSectionUsersShare                         :== RWShared Coord3D [User]                        [User]

:: DrawMapForActor r o a :== User (Shared (SectionStatusMap r)) (UserActorShare o a) (Shared (SectionInventoryMap o)) -> Task ()

instance == (Actor o a)
instance == (Object obj) | == obj

maps2DShare :: RWShared () Maps2D Maps2D

sharedGraph :: RWShared () Graph ()

sectionUsersShare :: SectionUsersShare

sectionForUserShare :: User -> RWShared () (Maybe Coord3D) SectionUsersMap

focusedSectionUsersShare :: FocusedSectionUsersShare

lockedExitsShare :: RWShared () SectionExitLockMap SectionExitLockMap

lockStatusForExit :: RWShared Coord3D [Dir] [Dir]

lockedHopsShare :: RWShared () SectionHopLockMap SectionHopLockMap

lockStatusForHop :: RWShared Coord3D [Coord3D] [Coord3D]

sectionForUser :: !User !SectionUsersMap -> Maybe Coord3D

actorsInSectionShare :: (UserActorShare o a) -> RWShared Coord3D [Actor o a] [Actor o a]

actorForUserShare :: (UserActorShare o a) -> FocusedUserActorShare o a

derive class iTask Actor, Object
instance toString (Object obj) | toString obj

// place an new actor into a room of your shared map after which the actor can freely move around

addActorToMap :: !(DrawMapForActor r o a) !(Actor o a) !Coord3D
                 !(FocusedSectionInventoryShare o)
                 !(SectionStatusShare r) !(UserActorShare o a) !(SectionInventoryShare o)
              -> Task () | iTask r & iTask o & iTask a

// move around the map until you return something

moveAround :: !(DrawMapForActor r o a) !User
              !(FocusedSectionInventoryShare o)
              !(SectionStatusShare r) !(UserActorShare o a) !(SectionInventoryShare o)
           -> Task () | iTask r & iTask o & iTask a

// finds all actors currently walking on the map, find all objects in the map

findAllObjects :: !(SectionInventoryMap o) -> [(!Coord3D, !Object o)] | iTask o
findUser :: !User !SectionUsersMap !(UserActorMap o a) -> Maybe (!Coord3D, !Actor o a) | iTask o & iTask a

// update the status of an actor, unique username is used as identification

updActorStatus :: !User !(a -> a) !(UserActorShare o a) -> Task () | iTask a & iTask o

getSectionFromMap :: !Coord3D !Maps2D -> Maybe Section

// shortest path calculation

shortestPath :: !(r -> Weight) !Coord3D !Coord3D !(SectionStatusMap r) !SectionExitLockMap !SectionHopLockMap !Graph
             -> Maybe (![Coord3D], !Distance)

maps2DToGraph :: !Maps2D -> Graph

// auto movement from actors thru the map, fetching. dropping and using objects

autoMove :: !Coord3D !Coord3D
            !(Coord3D Coord3D (SectionStatusMap r) SectionExitLockMap SectionHopLockMap Graph -> Maybe ([Coord3D], Distance))
            !User !(Shared (SectionStatusMap r)) !(UserActorShare o a)
         -> Task Bool | iTask r & iTask o & iTask a
pickupObject :: !Coord3D !(Object o) !User !(UserActorShare o a) !(FocusedSectionInventoryShare o)
             -> Task () | iTask o & iTask a
dropObject :: !Coord3D !(Object o) !User !(UserActorShare o a) !(FocusedSectionInventoryShare o)
           -> Task () | iTask o & iTask a
useObject :: !Coord3D !(Object o) !User !(UserActorShare o a) !(FocusedSectionInventoryShare o)
          -> Task Bool | iTask o & iTask a
getObjectOfType :: !(Actor o a) !o -> Object o | iTask o & iTask a

// given a shortest path algorithm, the current location and the kind of object one searches for
// returns: number of objects found, location of the closest object, distance to that object, shortest path to that object
//pathToClosestObject :: (Coord3D !Coord3D Maps2D -> Maybe ([Coord3D], Distance)) o Coord3D Maps2D 
//															-> (Int, (Coord3D, Distance, Maybe ([Coord3D], Distance)))
															
// given a shortest path algorithm, the current location, the kind of object one searches for, and the destination where the object has to be taken to
// returns: number of objects found, location of the closest object, distance to that object, shortest path to that object
smartPathToClosestObject :: !(Coord3D Coord3D (SectionStatusMap r) SectionExitLockMap SectionHopLockMap Graph -> Maybe ([Coord3D], Distance))
                            !o !Coord3D !Coord3D !(SectionStatusMap r) !(SectionInventoryMap o) !SectionExitLockMap !SectionHopLockMap !Graph
                         -> (!Maybe (Object o), !Int, !Distance, !Int, !(!Coord3D, !Distance, !Maybe [Coord3D])) | iTask o & == o &iTask r

getMap2D			:: !Maps2DIndex !Maps2D -> Maybe Map2D

setMap2D			:: !Maps2DIndex !Map2D !Maps2D -> Maps2D

updMap2D			:: !Maps2DIndex !(Map2D -> Map2D) !Maps2D -> Maps2D

getMapID            :: !Maps2DIndex !Maps2D -> Maybe MapID

getMap2DIndex       :: !MapID !Maps2D -> Maybe Maps2DIndex

getSection			:: !Coord2D !Map2D -> Maybe Section

setSection			:: !Coord2D !Section !Map2D -> Map2D

updSection			:: !Coord2D !(Section -> Section) !Map2D -> Map2D

updSections			:: !(Section -> Section) !Map2D -> Map2D

twin				:: !Dir !Coord2D -> Coord2D

validCoord			:: !Coord2D !Map2D -> Bool

dimension			:: !Map2D -> (!Int,!Int)

getBorder			:: !Dir !Section -> Border

setBorder			:: !Dir !Border !Section -> Section

updBorder			:: !Dir !(Border -> Border) !Section -> Section

opposite			:: !Dir -> Dir

(!!!) infixl 9		:: ![.a] !Int -> Maybe .a

(??) infixl 9		:: ![a] !a -> Int | == a

toggleDoor          :: !Coord3D !Dir -> Task ()

toggleHop           :: !Coord3D !Coord3D -> Task ()
