implementation module C2.Framework.MapEnvironment

import StdArray
import iTasks

import iTasks.UI.Definition
import iTasks.Internal.Tonic
import iTasks.Extensions.Admin.TonicAdmin
import iTasks.Extensions.DateTime
import qualified Data.Map as DM
from Data.Map import :: Map, instance Functor (Map k)
import qualified Data.IntMap.Strict as DIS
from Data.IntMap.Strict import :: IntMap
import qualified Data.Heap as DH
from Data.Heap import :: Heap
import Data.GenLexOrd
from C2.Framework.Logging import addLog
import Data.List
import Data.Eq
import Data.Maybe
import Data.Functor
import Data.Either

import StdMisc

derive class iTask Map2D, Section, Borders, Border, Coord2D, Dir, MapAction

derive class iTask Actor, Object

// small utility functions

instance == (Actor o a)  where (==) a1 a2 = a1.userName == a2.userName

derive gLexOrd Coord2D

instance == (Object obj) | == obj where
  (==) o1 o2 = o1 == o2

instance toString (Object obj) | toString obj where
  toString {Object | objId, objType} = toString objType +++ " " +++ toString objId

instance toString Coord2D where
  toString {col, row} = "(" +++ toString row +++ ", " +++ toString col +++ ")"

instance toString Coord3D where
  toString (l, {col, row}) = "(" +++ toString l +++ ", " +++ toString row +++ ", " +++ toString col +++ ")"

instance toString Dir where
  toString N = "N"
  toString E = "E"
  toString W = "W"
  toString S = "S"

instance == Coord2D where
  (==) l r = l === r

instance < Coord2D where
  (<) l r = case l =?= r of
              LT -> True
              _  -> False

instance zero Coord2D where
  zero = {Coord2D | col=0, row=0}

instance == Dir where
  (==) l r = l === r

instance == Border where
  (==) l r = l === r

infinity =: 67108864

maps2DShare :: SimpleSDSLens Maps2D
maps2DShare = sharedStore "maps2DShare" []

sharedGraph :: SDSLens () Graph ()
sharedGraph = sdsLens "sharedGraph" (const ()) (SDSRead read) (SDSWriteConst write) (SDSNotifyConst notify) Nothing maps2DShare
  where
  read _ m = Ok (maps2DToGraph m)

  write _ _ = Ok Nothing

  notify _ _  = const (const True)

sectionUsersShare :: SectionUsersShare
sectionUsersShare = sharedStore "sectionUsersShare" 'DM'.newMap

sectionForUserShare :: User -> SDSLens () (Maybe Coord3D) SectionUsersMap
sectionForUserShare user = mapRead (sectionForUser user) sectionUsersShare

focusedSectionUsersShare :: FocusedSectionUsersShare
focusedSectionUsersShare = mapLens "focusedSectionUsersShare" sectionUsersShare (Just [])

inventoryForUserSection :: !User !(FocusedSectionInventoryShare o) -> SDSSequence () (IntMap (Object o)) (IntMap (Object o)) | iTask o
inventoryForUserSection user inventoryForSectionShare = sdsSequence ("inventoryForUserSection" +++ toString user) id mkP2 (\_ _ -> Right mkr) (SDSWrite write1) (SDSWrite write2) (sectionForUserShare user) inventoryForSectionShare
  where
  mkP2 p (Just c3d) = c3d
  mkP2 _ _          = (-1, {col = -1, row = -1})
  mkr (_, inv) = inv
  write1 p r1 w = Ok Nothing
  write2 p r2 w = Ok (Just w)

lockedExitsShare :: SimpleSDSLens SectionExitLockMap
lockedExitsShare = sharedStore "lockedExitsShare" 'DM'.newMap

lockStatusForExit :: SDSLens Coord3D [Dir] [Dir]
lockStatusForExit = mapLens "lockStatusForExit" lockedExitsShare (Just [])

lockedHopsShare :: SimpleSDSLens SectionHopLockMap
lockedHopsShare = sharedStore "lockedHopsShare" 'DM'.newMap

lockStatusForHop :: SDSLens Coord3D [Coord3D] [Coord3D]
lockStatusForHop = mapLens "lockStatusForHop" lockedHopsShare (Just [])

maps2DToGraph :: !Maps2D -> Graph
maps2DToGraph maps2D = fst (foldl map2DToGraph ('DM'.newMap, 0) maps2D)

map2DToGraph :: !(!Graph, !Int) !Map2D -> (!Graph, !Int)
map2DToGraph (graph, floorIdx) map2D
  #! (graph, _) = foldl (rowToGraph floorIdx) (graph, 0) map2D.map2D
  = (graph, floorIdx + 1)

rowToGraph :: !Int !(!Graph, !Int) ![Section] -> (!Graph, !Int)
rowToGraph floorIdx (graph, rowIdx) sections
  #! (graph, _) = foldl (colToGraph floorIdx rowIdx) (graph, 0) sections
  = (graph, rowIdx + 1)

colToGraph :: !Int !Int !(!Graph, !Int) !Section -> (!Graph, !Int)
colToGraph floorIdx rowIdx (graph, colIdx) section
  #! currCoord2D = {col = colIdx, row = rowIdx}
  #! graph       = 'DM'.put (floorIdx, currCoord2D) (getCoord3Ds section floorIdx currCoord2D section.borders) graph
  = (graph, colIdx + 1)

getCoord3Ds :: !Section !Int !Coord2D !Borders -> [(!Maybe Dir, !Coord3D)]
getCoord3Ds section floorIdx currCoord2D borders
  #! acc = []
  #! acc = addOnOpening floorIdx borders.n N currCoord2D acc
  #! acc = addOnOpening floorIdx borders.e E currCoord2D acc
  #! acc = addOnOpening floorIdx borders.w W currCoord2D acc
  #! acc = addOnOpening floorIdx borders.s S currCoord2D acc
  #! acc = acc ++ map (\h -> (Nothing, h)) section.hops
  = acc
  where
  addOnOpening :: !Int !Border !Dir !Coord2D ![(!Maybe Dir, !Coord3D)] -> [(!Maybe Dir, !Coord3D)]
  addOnOpening _        Wall _   _       acc = acc
  addOnOpening floorIdx b    dir coord2D acc = [(Just dir, (floorIdx, twin dir coord2D)) : acc]

:: PathMap     :== Map Coord3D Coord3D
:: DistMap     :== Map Coord3D Distance

shortestPath :: !(r -> Weight) !Coord3D !Coord3D !(SectionStatusMap r) !SectionExitLockMap !SectionHopLockMap !Graph
             -> Maybe (![Coord3D], !Distance)
shortestPath costFn startRoomCoord endRoomCoord statusMap locks hopLocks graph
  | startRoomCoord == endRoomCoord = Just ([], 0)
  | otherwise
    #! pathMap = 'DM'.newMap
    #! distMap = 'DM'.put startRoomCoord 0 (fmap (const infinity) graph)
    #! pqueue  = 'DH'.singleton (startRoomCoord, 0)
    = reconstructSP (findSP costFn graph pqueue statusMap locks hopLocks pathMap distMap)
  where
  reconstructSP :: !(!PathMap, !DistMap) -> Maybe (![Coord3D], !Distance)
  reconstructSP (pathMap, distMap)
    =                 reconstructSP` pathMap endRoomCoord []
    >>= \path      -> 'DM'.get endRoomCoord distMap
    >>= \totalDist -> Just (path, totalDist)

  reconstructSP` :: !PathMap !Coord3D ![Coord3D] -> Maybe [Coord3D]
  reconstructSP` pathMap currIdx path
    | currIdx == startRoomCoord = Just path
    | otherwise
      = case 'DM'.get currIdx pathMap of
          Just prevIdx
            #! path` = [currIdx : path]
            | prevIdx == startRoomCoord = Just path`
            | otherwise                 = reconstructSP` pathMap prevIdx path`
          _ = Nothing

  findSP :: !(r -> Weight) !Graph !(Heap (!Coord3D, !Distance)) !(SectionStatusMap r)
            !SectionExitLockMap !SectionHopLockMap !PathMap !DistMap
         -> (!PathMap, !DistMap)
  findSP costFn graph pqueue statusMap locks hopLocks pathMap distMap
    | 'DH'.null pqueue = (pathMap, distMap)
    | otherwise
      = case 'DH'.uncons pqueue of
          Just ((minIdx, minDist), pqueue)
            = case 'DM'.get minIdx graph of
                Just exits
                  #! (pathMap, distMap, pqueue) = foldr (foldExits costFn statusMap locks hopLocks minDist minIdx) (pathMap, distMap, pqueue) exits
                  = findSP costFn graph pqueue statusMap locks hopLocks pathMap distMap
                _ = (pathMap, distMap)
          _ = (pathMap, distMap)
    where
    foldExits :: !(r -> Weight) !(SectionStatusMap r) !SectionExitLockMap !SectionHopLockMap !Distance !Coord3D
                 !(!Maybe Dir, !Coord3D)
                 !(!PathMap, !DistMap, !Heap (!Coord3D, !Distance))
              -> (!PathMap, !DistMap, !Heap (!Coord3D, !Distance))
    foldExits costFn statusMap locks hopLocks minDist minIdx (dir, nextRoom) (pathMap, distMap, pqueue)
      | isLocked dir = (pathMap, distMap, pqueue)
      | otherwise
        = case 'DM'.get nextRoom distMap of
            Just nDist
              #! roomCost = maybe 1 costFn ('DM'.get nextRoom statusMap)
              #! alt      = minDist + roomCost
              | alt < nDist
                = ( 'DM'.put nextRoom minIdx pathMap
                  , 'DM'.put nextRoom alt distMap
                  , 'DH'.insert (nextRoom, alt) pqueue)
              | otherwise = (pathMap, distMap, pqueue)
            _ = (pathMap, distMap, pqueue)
      where
      isLocked :: !(Maybe Dir) -> Bool
      isLocked (Just d) = isMember d (fromMaybe [] ('DM'.get minIdx locks))
      isLocked _        = isMember nextRoom (fromMaybe [] ('DM'.get minIdx hopLocks))

getSectionFromMap :: !Coord3D !Maps2D -> Maybe Section
getSectionFromMap (lvl, c2d) ms2d
  | lvl < length ms2d = getSection c2d (ms2d !! lvl)
  | otherwise         = Nothing

getMap2D			:: !Maps2DIndex !Maps2D -> Maybe Map2D
getMap2D idx ms2d
  | 0 <= idx && idx < length ms2d
  					= Just (ms2d !! idx)
  | otherwise		= Nothing

setMap2D			:: !Maps2DIndex !Map2D !Maps2D -> Maps2D
setMap2D idx m ms2d
  | 0 <= idx && idx < length ms2d
  					= updateAt idx m ms2d
  | otherwise		= ms2d

updMap2D :: !Maps2DIndex !(Map2D -> Map2D) !Maps2D -> Maps2D
updMap2D idx f ms2d
  | 0 <= idx && idx < length ms2d
  					= updateAt idx (f (ms2d !! idx)) ms2d
  | otherwise		= ms2d

getMapID            :: !Maps2DIndex !Maps2D -> Maybe MapID
getMapID idx ms2d
  | 0 <= idx && idx < length ms2d
  					= Just (ms2d !! idx).Map2D.mapId
  | otherwise		= Nothing

getMap2DIndex       :: !MapID !Maps2D -> Maybe Maps2DIndex
getMap2DIndex mapID ms2d
					= listToMaybe [idx \\ {Map2D | mapId} <- ms2d & idx <- [0..] | mapId == mapID]

getSection :: !Coord2D !Map2D -> Maybe Section
getSection {col, row} {Map2D | map2D} = map2D !!! row >>= \cols -> cols !!! col

setSection :: !Coord2D !Section !Map2D -> Map2D
setSection {col, row} s m=:{Map2D | map2D}
  #! map2D` = case map2D !!! row of
                Just cols = case cols !!! col of
                              Just _ = updateAt row (updateAt col s cols) map2D
                              _      = map2D
                _ = map2D
  = {Map2D | m & map2D=map2D`}

updSection			:: !Coord2D !(Section -> Section) !Map2D -> Map2D
updSection c f m	= case getSection c m of
					    Just s   = setSection c (f s) m
					    _        = m

updSections			:: !(Section -> Section) !Map2D -> Map2D
updSections f m=:{Map2D | map2D}
					= {Map2D | m & map2D = map (map f) map2D}

twin :: !Dir !Coord2D -> Coord2D
twin d c=:{Coord2D | col, row}
  = case d of
      S = {Coord2D | c & row = row + 1}
      E = {Coord2D | c & col = col + 1}
      N = {Coord2D | c & row = row - 1}
      W = {Coord2D | c & col = col - 1}

validCoord :: !Coord2D !Map2D -> Bool
validCoord c map
  #! (cols, rows) = dimension map
  = 0 <= c.Coord2D.col && c.Coord2D.col < cols && 0 <= c.Coord2D.row && c.Coord2D.row < rows

dimension			:: !Map2D -> (!Int,!Int)
dimension {Map2D | map2D}
					= (length (hd map2D),length map2D)

getBorder			:: !Dir !Section -> Border
getBorder d {Section | borders={n,e,w,s}}
					= case d of
						N = n
						E = e
						W = w
						S = s

setBorder			:: !Dir !Border !Section -> Section
setBorder d b s=:{Section | borders=bs}
  #! bs` = case d of
             N = {Borders | bs & n=b}
             E = {Borders | bs & e=b}
             W = {Borders | bs & w=b}
             S = {Borders | bs & s=b}
  = {Section | s & borders=bs`}

updBorder			:: !Dir !(Border -> Border) !Section -> Section
updBorder d f s		= setBorder d (f (getBorder d s)) s

opposite			:: !Dir -> Dir
opposite N			= S
opposite S			= N
opposite W			= E
opposite E			= W

(!!!) infixl 9		:: ![.a] !Int -> Maybe .a
(!!!) xs i			= listToMaybe [x \\ x <- xs & j <- [0..] | i==j]

(??) infixl 9		:: ![a] !a -> Int | == a
(??) xs x			= hd ([i \\ i <- [0..] & x` <- xs | x==x`] ++ [-1])

// moving around in the map

addActorToMap :: !(DrawMapForActor r o a) !(Actor o a) !Coord3D
                 !(FocusedSectionInventoryShare o)
                 !(SectionStatusShare r) !(UserActorShare o a) !(SectionInventoryShare o)
              -> Task () | iTask r & iTask o & iTask a
addActorToMap roomViz actor location inventoryForSectionShare shipStatusShare userToActorShare inventoryForAllSectionsShare
  =            get maps2DShare
  >>= \ms2d -> if (existsSection location ms2d)
                 (   upd ('DM'.put actor.userName actor) userToActorShare
                 >>| move (0, {col = 0, row = 0}) location actor.userName
                 >>| moveAround roomViz actor.userName inventoryForSectionShare shipStatusShare userToActorShare inventoryForAllSectionsShare)
                 (viewInformation ("Section with number: " <+++ location <+++ " does not exist") [] () >>| return ())

:: UITag :== [Int]

:: TaskUITree
  = Ed   UITag
  | Par  UITag [TaskUITree]
  | Step UITag [TaskUITree]

uiToRefs :: UI -> TaskUITree
uiToRefs (UI _ _ subs) = case recurse [] subs of
                           [x : _] -> x
                           _       -> Ed []
  where
  uiToRefs` :: [Int] (Int, UI) -> [TaskUITree]
  uiToRefs` path (i, UI UIParallel _ subs)
    # curPath = path ++ [i]
    = [Par curPath (recurse curPath subs)]
  uiToRefs` path (i, UI UIStep _ subs)
    # curPath = path ++ [i]
    = [Step curPath (recurse curPath subs)]
  uiToRefs` path (i, _)
    # curPath = path ++ [i]
    = [Ed curPath]
  recurse curPath subs = flatten (map (uiToRefs` curPath) (zip2 [0..] subs))

getSubTree :: UI [Int] -> Maybe UI
getSubTree ui [] = Just ui
getSubTree (UI _ _ uis) [i : is]
  | i < length uis = getSubTree (uis !! i) is
  | otherwise      = Nothing

:: TaskUILayout
  = UIBeside [TaskUILayout]
  | UIAbove [TaskUILayout]
  | UINode UITag

uiOf :: TaskUITree -> TaskUILayout
uiOf (Ed path)     = UINode path
uiOf (Par path _)  = UINode path
uiOf (Step path _) = UINode path

uiBeside :: [TaskUILayout] -> TaskUILayout
uiBeside refs = UIBeside refs

uiAbove :: [TaskUILayout] -> TaskUILayout
uiAbove refs = UIAbove refs


//layoutSubs_ :: (NodePath UI -> Bool) Layout -> Layout
//layoutSubs_ pred layout = layout`
//where
	//layout` (change,s)
		//| change=:(ReplaceUI _)
			//# (change,eitherState) = layoutChange_ [] pred layout change (NL [])
			//= (change,toJSON eitherState)
		//| otherwise
			//# (change,eitherState) = case fromMaybe (Right (NL [])) (fromJSON s) of
				//(Left state) = appSnd Left (layout (change,state))
				//(Right states) = layoutChange_ [] pred layout change states
			//= (change,toJSON eitherState)



/*
modifyUI :: (TaskUITree -> TaskUILayout) -> Layout
modifyUI f = idLayout
modifyUI f = \(uichange, json) -> case uichange of
                                    ReplaceUI ui -> (ReplaceUI (toLayout ui (f (uiToRefs ui))), json)
                                    _ -> (uichange, json)
  where
  toLayout :: UI TaskUILayout -> UI
  toLayout ui (UIBeside ls) = UI UIParallel ('DM'.singleton "direction" (JSONString "horizontal")) (map (toLayout ui) ls)
  toLayout ui (UIAbove ls)  = UI UIParallel ('DM'.singleton "direction" (JSONString "vertical")) (map (toLayout ui) ls)
  toLayout ui (UINode path) = case getSubTree ui path of
                                Just ui -> ui
                                _       -> UI UIDebug 'DM'.newMap []
*/

moveAround :: !(DrawMapForActor r o a) !User
              !(FocusedSectionInventoryShare o)
              !(SectionStatusShare r) !(UserActorShare o a) !(SectionInventoryShare o)
           -> Task () | iTask r & iTask o & iTask a
moveAround viewDeck user inventoryForSectionShare
           shipStatusShare userToActorShare inventoryForAllSectionsShare
  = forever (    walkAround  -||- changeDecks
            -||- pickUpItems -||- dropItems) //<<@ ApplyLayout (idLayout modifyUI moveAroundUI)
  where
  walkAround :: Task ()
  walkAround
    =   watch (lockedExitsShare |*| roomNoForCurrentUserShare |*| maps2DShare)
    -|| viewDeck user shipStatusShare userToActorShare inventoryForAllSectionsShare
    >>* [ OnAction (Action "Go west") (moveTo W)
        , OnAction (Action "Go north") (moveTo N)
        , OnAction (Action "Go south") (moveTo S)
        , OnAction (Action "Go east") (moveTo E)
        ]

  changeDecks :: Task ()
  changeDecks
    =    watch (lockedHopsShare |*| roomNoForCurrentUserShare)
    -&&- enterChoiceWithShared "Change deck" [prettyPrintHops] nearbyHops
    >>*  [OnAction (Action "Change deck") changeDeck]

  pickUpItems :: Task ()
  pickUpItems
    =    watch roomNoForCurrentUserShare
    -&&- enterChoiceWithShared "Items nearby" [prettyPrintItems] (nearbyItemsShare inventoryForSectionShare)
    >>*  [OnAction (Action "Grab selected item") (withSelectedObject userToActorShare inventoryForSectionShare pickupObject)]

  dropItems :: Task ()
  dropItems
    =    watch roomNoForCurrentUserShare
    -&&- enterChoiceWithShared "Items in inventory" [prettyPrintItems] (inventoryShare userToActorShare)
    >>*  [OnAction (Action "Drop selected item") (withSelectedObject userToActorShare inventoryForSectionShare dropObject)]

  moveAroundUI :: TaskUITree -> TaskUILayout
  moveAroundUI (Par _ [ walkAroundUI
                      , Par _ [ changeDecksUI
                              , Par _ [ pickUpUI
                                      , dropUI ] ] ])
    = uiAbove [ uiOf walkAroundUI
              , uiBeside [uiOf changeDecksUI, uiOf pickUpUI, uiOf dropUI]]
  moveAroundUI us = UINode []

  prettyPrintHops = ChooseFromGrid prettyHop
  prettyPrintItems = ChooseFromGrid prettyItem

  nearbyHops :: SDSLens () [(Coord3D, Coord3D)] ()
  nearbyHops = toReadOnly (mapRead getHops (roomNoForCurrentUserShare |*| maps2DShare))

  roomNoForCurrentUserShare :: SDSLens () (Maybe Coord3D) ()
  roomNoForCurrentUserShare = toReadOnly (sectionForUserShare user)

  inventoryShare :: (UserActorShare o a) -> SDSLens () [Object o] () | iTask o & iTask a
  inventoryShare userToActorShare = toReadOnly (mapRead carriedObjects (sdsFocus user (actorForUserShare userToActorShare)))

  nearbyItemsShare :: (FocusedSectionInventoryShare o) -> SDSLens () [Object o] () | iTask o
  nearbyItemsShare inventoryForSectionShare = toReadOnly (mapRead 'DIS'.elems (inventoryForUserSection user inventoryForSectionShare))

  getHops :: (Maybe Coord3D, Maps2D) -> [(Coord3D, Coord3D)]
  getHops (Just c3d, ms2d)
    = case getSectionFromMap c3d ms2d of
        Just section -> map (\h -> (c3d, h)) section.hops
        _            -> []
  getHops _ = []

  withSelectedObject :: (UserActorShare o a) (FocusedSectionInventoryShare o)
                        (Coord3D (Object o) User (UserActorShare o a) (FocusedSectionInventoryShare o) -> Task ())
                        (TaskValue (Maybe Coord3D, Object o))
                     -> Maybe (Task ()) | iTask o & iTask a
  withSelectedObject userToActorShare inventoryForSectionShare f (Value (Just roomNo, selectedObject) _) = Just (f roomNo selectedObject user userToActorShare inventoryForSectionShare)
  withSelectedObject _ _ _ _ = Nothing

  prettyHop :: !(!Coord3D, !Coord3D) -> String
  prettyHop ((floorIdx, _), hop=:(nextFloor, c2d)) = "Go " +++ (if (nextFloor < floorIdx) "up" "down") +++ " to floor " +++ toString nextFloor +++ ", room " +++ toString c2d

  carriedObjects mactor = maybe [] (\a -> a.carrying) mactor

  prettyItem obj = obj.objType

  moveTo :: Dir (TaskValue ((SectionExitLockMap, Maybe Coord3D), Maps2D)) -> Maybe (Task ())
  moveTo dir (Value ((exitLocks, Just roomNo=:(floor, room2D)), ms2d) _)
    = case getSectionFromMap roomNo ms2d of
        Just section
          | isWall section dir || doorIsLocked roomNo dir exitLocks = Nothing
          | otherwise                                               = Just (move roomNo (floor, twin dir room2D) user)
        _ = Nothing
    where
    isWall :: !Section !Dir -> Bool
    isWall section dir = getBorder dir section == Wall
  moveTo _ _ = Nothing

  changeDeck :: (TaskValue ((SectionHopLockMap, Maybe Coord3D), (Coord3D, Coord3D))) -> Maybe (Task ())
  changeDeck (Value ((hopLocks, Just roomNo), (_, hop)) _)
    | hopIsLocked roomNo hop hopLocks = Nothing
    | otherwise                       = Just (move roomNo hop user)
    where
    hopIsLocked :: !Coord3D !Coord3D !SectionHopLockMap -> Bool
    hopIsLocked currC3d nextC3d hopLocks
      = case 'DM'.get currC3d hopLocks of
          Just locks
            = elem nextC3d locks
          _ = False
  changeDeck _ = Nothing

sectionForSectionNumberShare :: SDSLens Coord3D (Maybe Section) Section
sectionForSectionNumberShare = sdsLens "sectionForSectionNumberShare" (const ()) (SDSRead read) (SDSWrite write) (SDSNotify notify) Nothing maps2DShare
  where
  read :: Coord3D Maps2D -> MaybeError TaskException (Maybe Section)
  read c3d ms2d = Ok (getSectionFromMap c3d ms2d)

  write :: Coord3D Maps2D Section
        -> MaybeError TaskException (Maybe Maps2D)
  write (floorIdx, c2d) ms2d section = Ok (Just (updMap2D floorIdx (setSection c2d section) ms2d))

  notify :: Coord3D Maps2D Section -> SDSNotifyPred Coord3D
  notify c3d _ _ = \_ c3d` -> c3d == c3d`

pickupObject :: !Coord3D !(Object o) !User !(UserActorShare o a) !(FocusedSectionInventoryShare o)
             -> Task () | iTask o & iTask a
pickupObject c3d object user userActorShare shFocusedSectionInventory
  =   upd f userActorShare
  >>| upd (\inv -> 'DIS'.fromList [(obj.objId, obj) \\ obj <- 'DIS'.elems inv | obj.objId /= object.objId]) (sdsFocus c3d shFocusedSectionInventory) @! ()
  where
  f userActorMap = case 'DM'.get user userActorMap of
                     Just actor
                       = 'DM'.put user {actor & carrying = [object:actor.carrying]} userActorMap
                     _ = userActorMap

dropObject :: !Coord3D !(Object o) !User !(UserActorShare o a) !(FocusedSectionInventoryShare o)
           -> Task () | iTask o & iTask a
dropObject c3d object user userActorShare shFocusedSectionInventory
  =   upd f userActorShare
  >>| upd (\inv -> 'DIS'.put object.objId object inv) (sdsFocus c3d shFocusedSectionInventory) @! ()
  where
  f userActorMap = case 'DM'.get user userActorMap of
                     Just actor
                       = 'DM'.put user {actor & carrying = removeObject object actor.carrying} userActorMap
                     _ = userActorMap

useObject :: !Coord3D !(Object o) !User !(UserActorShare o a) !(FocusedSectionInventoryShare o)
          -> Task Bool | iTask o & iTask a
useObject c3d object user userActorShare shFocusedSectionInventory
  =                    get userActorShare
  >>- \userActorMap -> case 'DM'.get user userActorMap of
                         Just actor
                           | hasObject object actor
                           = set ('DM'.put user {actor & carrying = removeObject object actor.carrying} userActorMap) userActorShare @! True
                         _ = return False

hasObject :: !(Object o) !(Actor o a) -> Bool
hasObject obj actor = length [0 \\ obj` <- actor.carrying | obj.objId == obj`.objId] > 0

removeObject :: !(Object o) ![Object o] -> [Object o]
removeObject obj objs = [obj` \\ obj` <- objs | obj.objId /= obj`.objId ]

move :: !Coord3D !Coord3D !User -> Task ()
move fromSection toSection user
  = upd (enterSection user toSection o leaveSection user fromSection) sectionUsersShare @! ()
  where
  leaveSection :: !User !Coord3D !SectionUsersMap -> SectionUsersMap
  leaveSection user roomNo usersMap
    = 'DM'.alter (fmap (\actors -> [a \\ a <- actors | a /= user])) roomNo usersMap

  enterSection :: !User !Coord3D !SectionUsersMap -> SectionUsersMap
  enterSection user roomNo usersMap
    #! actors = 'DM'.findWithDefault [] roomNo usersMap
    = 'DM'.put roomNo (nub [user : actors]) usersMap

getObjectOfType :: !(Actor o a) !o -> Object o | iTask o & iTask a
getObjectOfType {Actor | carrying} objType` = case [obj \\ obj <- carrying | obj.objType === objType`] of
                                                [x : _] -> x

// auto moves around the maze

autoMove :: !Coord3D !Coord3D
            !(Coord3D Coord3D (SectionStatusMap r) SectionExitLockMap SectionHopLockMap Graph -> Maybe ([Coord3D], Distance))
            !User !(Shared sds (SectionStatusMap r)) !(UserActorShare o a)
         -> Task Bool | iTask r & iTask o & iTask a & RWShared sds
autoMove thisSection target pathFun user shipStatusShare userToActorShare
  | thisSection == target = return True
  | otherwise
      =                 get sectionUsersShare
      >>- \actorMap  -> case sectionForUser user actorMap of
                          Just roomCoord
                            =                 get shipStatusShare
                            >>- \statusMap -> get lockedExitsShare
                            >>- \exitLocks -> get lockedHopsShare
                            >>- \hopLocks  -> get sharedGraph
                            >>- \graph     -> case pathFun thisSection target statusMap exitLocks hopLocks graph of
                                                Just (path=:[nextSection:_], _)
                                                  =   waitForTimer 1
                                                  >>| move roomCoord nextSection user
                                                  >>| addLog user "" ("Has moved to Section " <+++ nextSection)
                                                  >>| autoMove nextSection target pathFun user shipStatusShare userToActorShare
                                                _ = return False
                          _ = return False

// room updating

// actor status opdating

updActorStatus :: !User !(a -> a) !(UserActorShare o a) -> Task () | iTask a & iTask o
updActorStatus user upd userToActorShare
  =                    get userToActorShare
  >>= \userActorMap -> case 'DM'.get user userActorMap of
                         Just actor -> set ('DM'.put user {actor & actorStatus = upd actor.actorStatus} userActorMap) userToActorShare @! ()
                         Nothing    -> return ()

sectionForUser :: !User !SectionUsersMap -> Maybe Coord3D
sectionForUser u sectionUsersMap = listToMaybe [k \\ (k, us) <- 'DM'.toList sectionUsersMap, u` <- us | u` == u]

actorsInSectionShare :: (UserActorShare o a) -> SDSLens Coord3D [Actor o a] [Actor o a] | iTask o & iTask a
actorsInSectionShare userActorShare = sdsLens "actorsInSectionShare" (const ()) (SDSRead read) (SDSWrite write) (SDSNotify notify) Nothing (sectionUsersShare >*< userActorShare)
  where
  read :: Coord3D (SectionUsersMap, UserActorMap o a) -> MaybeError TaskException [Actor o a]
  read c3d (sectionUsersMap, userActorMap) = Ok [a \\ Just us <- ['DM'.get c3d sectionUsersMap], u <- us, Just a <- ['DM'.get u userActorMap]]

  write :: Coord3D (SectionUsersMap, UserActorMap o a) [Actor o a]
        -> MaybeError TaskException (Maybe (SectionUsersMap, UserActorMap o a))
  write c3d (sectionUsersMap, userActorMap) actors = Ok (Just ('DM'.put c3d (map (\a -> a.userName) actors) sectionUsersMap, 'DM'.fromList [(a.userName, a) \\ a <- actors]))

  notify :: Coord3D (SectionUsersMap, UserActorMap o a) [Actor o a] -> SDSNotifyPred Coord3D
  notify c3d _ _ = \_ c3d` -> c3d == c3d`

actorForUserShare :: (UserActorShare o a) -> FocusedUserActorShare o a | iTask o & iTask a
actorForUserShare userActorShare = mapMaybeLens "actorForUserShare" userActorShare

findUser :: !User !SectionUsersMap !(UserActorMap o a) -> Maybe (!Coord3D, !Actor o a) | iTask o & iTask a
findUser usr sectionUsersMap userActorMap
  =         'DM'.get usr userActorMap
  >>= \a -> sectionForUser usr sectionUsersMap
  >>= \s -> return (s, a)

// room status updating
toggleDoor :: !Coord3D !Dir -> Task ()
toggleDoor roomNo=:(floorIdx, c2d) exit
  #! twinC2d = twin exit c2d
  #! focus1  = sdsFocus roomNo lockStatusForExit
  #! focus2  = sdsFocus (floorIdx, twinC2d) lockStatusForExit
  =              get focus1
  >>- \locks1 -> get focus2
  >>- \locks2 -> set (newLocks exit locks1) focus1
  >>|            set (newLocks (opposite exit) locks2) focus2 @! ()
  where
  newLocks :: !Dir ![Dir] -> [Dir]
  newLocks dir locks
    #! (lockedDirs, rest) = partition (\l -> l === dir) locks
    | isEmpty lockedDirs = [dir : rest]
    | otherwise          = rest

toggleHop :: !Coord3D !Coord3D -> Task ()
toggleHop fromRoom toRoom
  #! focus1 = sdsFocus fromRoom lockStatusForHop
  #! focus2 = sdsFocus toRoom lockStatusForHop
  =              get focus1
  >>- \locks1 -> get focus2
  >>- \locks2 -> set (newLocks fromRoom locks1) focus1
  >>|            set (newLocks toRoom locks2) focus2 @! ()
  where
  newLocks :: !Coord3D ![Coord3D] -> [Coord3D]
  newLocks c3d locks
    #! (lockedDirs, rest) = partition (\l -> l === c3d) locks
    | isEmpty lockedDirs = [c3d : rest]
    | otherwise          = rest

doorIsLocked :: !Coord3D !Dir !SectionExitLockMap -> Bool
doorIsLocked roomNo exit lockMap
  = case 'DM'.get roomNo lockMap of
      Just locks -> isEmpty [l \\ l <- locks | l == exit]
      _          -> False

// utility functions to find things located in the map

findAllObjects :: !(SectionInventoryMap o) -> [(!Coord3D, !Object o)] | iTask o
findAllObjects objectMap = [ (roomNo, object)
                           \\ (roomNo, objects) <- 'DM'.toList objectMap
                           , object <- 'DIS'.elems objects
                           ]

allSections :: !Maps2D -> [Section]
allSections ms2d = [room \\ floor <- ms2d, layer <- floor.map2D, room <- layer]

existsSection :: !Coord3D !Maps2D -> Bool
existsSection (lvl, c2d) ms2d
  | lvl < length ms2d
    #! m2d = ms2d !! lvl
    | c2d.row < length m2d.map2D
      #! s = m2d.map2D !! c2d.row
      = c2d.col < length s
    | otherwise = False
  | otherwise = False

pathToClosestObject :: !(Coord3D Coord3D (SectionStatusMap r) SectionExitLockMap Maps2D -> Maybe ([Coord3D], Distance))
                       !o !Coord3D !(SectionStatusMap r) !(SectionInventoryMap o) !SectionExitLockMap !Maps2D
                    -> (!Int, !(!Coord3D, !Distance, !Maybe (![Coord3D], !Distance))) | iTask o & == o & iTask r
pathToClosestObject sp kind actorLoc statusMap inventoryMap exitLocks ms2d
  #! spath = sortBy (\(_, i, _) (_, j, _) -> i < j)
                    (filter (\((ol, _), _, _) -> ol >= 0)
                            [case sp actorLoc objectLoc statusMap exitLocks ms2d of
                               path=:(Just (_, dist)) -> (objectLoc, dist, path)
                               _                      -> ((-1, {col = -1, row = -1}), infinity, Nothing)
                            \\ (objectLoc, found) <- findAllObjects inventoryMap | found.objType == kind ])
  = case spath of
      [x=:(_, _, Just (path, _)) :_] -> (length spath, x)
      []                             -> (-1, ((-1, {col = -1, row = -1}), -1, Nothing))

// returns: number of objects found, location of object, distance to object, shortest path to obejct
smartPathToClosestObject :: !(Coord3D Coord3D (SectionStatusMap r) SectionExitLockMap SectionHopLockMap Graph -> Maybe ([Coord3D], Distance))
                            !o !Coord3D !Coord3D !(SectionStatusMap r) !(SectionInventoryMap o) !SectionExitLockMap !SectionHopLockMap !Graph
                         -> (!Maybe (Object o), !Int, !Distance, !Int, !(!Coord3D, !Distance, !Maybe [Coord3D])) | iTask o & == o & iTask r
smartPathToClosestObject spath objectKind actorLoc targetLoc statusMap inventoryMap exitLocks hopLocks graph
  #! foundObjects = [tpl \\ tpl=:(_, found) <- findAllObjects inventoryMap | found.objType == objectKind ]
  | isEmpty foundObjects = (Nothing, infinity, infinity, 0, ((-1, {col = -1, row = -1}), -1, Nothing))
  #! pathsFound = sortBy (\(_, i, _, _) (_, j, _, _) -> i < j)
                         (filter (\(_, d, _, (loc, dist, path)) -> isJust path)
                         [ let (oPath, oDistance) = case spath actorLoc objectLoc statusMap exitLocks hopLocks graph of
                                                      (Just (path, distance)) -> (Just path, distance)
                                                      _                       -> (Nothing, infinity)
                               (tPath, tDistance) = case spath objectLoc targetLoc statusMap exitLocks hopLocks graph of
                                                      (Just (path, distance)) -> (Just path, distance)
                                                      _                       -> (Nothing, infinity)
                               totalPathDist      = case (oPath, tPath) of
                                                      (Just xs, Just ys) -> length xs + length ys
                                                      _                  -> infinity
                           in (obj, oDistance + tDistance, totalPathDist, (objectLoc, oDistance, oPath))
                         \\ (objectLoc, obj) <- foundObjects | objectLoc /= targetLoc
                         ])
  = case pathsFound of
      [(obj, cost, totalDist, x=:(_, _, Just path)) :_] -> (Just obj, cost, totalDist, length pathsFound, x)
      []                                                -> (Nothing, infinity, infinity, -1, ((-1, {col = -1, row = -1}), -1, Nothing))
