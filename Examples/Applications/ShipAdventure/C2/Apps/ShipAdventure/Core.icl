implementation module C2.Apps.ShipAdventure.Core

import iTasks.Extensions.DateTime
from   iTasks.Extensions.SVG.SVGEditor import :: SVGEditor(..), :: TagSource, fromSVGEditor
import qualified Data.List as DL
import qualified Data.Map as DM
import Data.Map.GenJSON
import qualified Data.IntMap.Strict as DIS
import qualified Data.Set as DS
from Data.Func import mapSt
import StdArray
import Data.Data
import Text.HTML
import C2.Framework.Core

import C2.Apps.ShipAdventure.Images
import C2.Apps.ShipAdventure.Types, C2.Framework.Logging, C2.Apps.ShipAdventure.Scripting
import C2.Apps.ShipAdventure.PathFinding, C2.Apps.ShipAdventure.Util

derive class iTask ChoiceGrid, ChoiceRow

// the next function should be placed in the library somewhere
mkTable :: [String] ![a] -> (ChoiceGrid,[Int]) | gText{|*|} a
mkTable header a = ({ChoiceGrid|header=header,rows=[{ChoiceRow|id=i,cells = row r} \\ r <- a & i <- [0..]]},[])
where
	row :: !a -> [HtmlTag] | gText{|*|} a
	row x = [Text cell \\ cell <- gText{|*|} AsRow (Just x)]

myTasks :: [Workflow]
myTasks = [ workflow "Walk around"  "Enter map, walk around, follow instructions of commander" currentUserWalkAround
          , workflow "D-Officer"    "Give instructions to crew members on the map"             giveInstructions
          , workflow "Alter fire script" "Define your own script for handling fires"           changeFireScript
          , workflow "Alter flood script" "Define your own script for handling floods"         changeFloodScript
          , workflow "Alter smoke script" "Define your own script for handling smoke"          changeSmokeScript
          ]

currentUserWalkAround :: Task ()
currentUserWalkAround = get currentUser >>= actorWithInstructions

// initial task to place an actor on the map
// one can only assign tasks to actors on the map
actorWithInstructions :: !User -> Task ()
actorWithInstructions user
  =           get myUserActorMap
  >>= \uam -> case 'DM'.get user uam of
                Nothing
                  =   pickStartRoom
                  >>* [OnValue (hasValue (   \loc -> addLog user "" ("Entered the building starting in room " <+++ loc)
                                         >>|         addActorToMap mkSection (newActor user) loc inventoryInSectionShare myStatusMap myUserActorMap myInventoryMap))]
                Just _ = moveAround mkSection user inventoryInSectionShare myStatusMap myUserActorMap myInventoryMap
  where
  newActor :: !User -> MyActor
  newActor user
    = { userName    = user
      , carrying    = []
      , actorStatus = { occupied = Available
                      , health   = FullHealth
                      , energy   = FullEnergy
                      }
      }

  pickStartRoom :: Task Coord3D
  pickStartRoom
    = Hint "Which room do you want to start in?" @>> updateInformationWithShared 
        [UpdateSharedUsing id (const snd) const editor]
        (maps2DShare |*| myNetwork) NoAction
    >>* [OnValue (\v -> case v of
                          Value (FocusOnSection c3d) _ = Just (return c3d)
                          _                            = Nothing)]
  editor = fromSVGEditor
		{ initView    = \((ms2d, _), cl) -> (ms2d, cl)
		, renderImage = \((_, network), _) (ms2d`, cl`) -> maps2DImage 'DS'.newSet cl` PickRoomMode ms2d` 'DM'.newMap 'DM'.newMap 'DM'.newMap 'DM'.newMap 'DM'.newMap 'DM'.newMap 'DIS'.newMap {network & devices = 'DM'.newMap}
		, updModel    = \((_, network), _) (ms2d`, cl`) -> ((ms2d`, network), cl`)
		}

// given the alarms one has to decide which tasks to assign to handle the situation

spToDistString :: !(Maybe [Coord3D]) -> String
spToDistString (Just es) = toString (length es)
spToDistString _         = "Section unreachable!"

spToDistString2 :: !(Maybe ([Coord3D], Distance)) -> String
spToDistString2 (Just (es,_)) = toString (length es)
spToDistString2 _             = "Section unreachable!"

roomToString :: !Coord3D -> String
roomToString n = toString n

giveInstructions :: Task ()
giveInstructions =
  forever
  (          get currentUser
  >>- \me -> (                        Hint "Choose which alarm to handle: " @>> enterChoiceWithShared  [ChooseFromGrid showAlarm] allActiveAlarms
             >&>                      withSelection (viewInformation [] "No Alarm Selected")
             \(alarmLoc, detector) -> selectSomeOneToHandle (alarmLoc, detector)
             >&>                      withSelection (viewInformation [] "No Crew Member Selected")
             \(actorLoc, actor) ->    scriptDefined detector
             >>- \scriptExists ->     (viewRelativeStatus (actorLoc, actor) (alarmLoc, detector)
                                      ||-
                                      (Hint "Select the Priority : " @>> updateChoice  [ChooseFromCheckGroup id] [Low, Normal, High, Highest] High))
             >>* [ OnAction ActionByHand    (hasValue (\prio -> handleAlarm (me, (alarmLoc, detector), (actorLoc, actor), prio)))
                 , OnAction ActionSimulated (hasValue (\prio -> autoHandleAlarm me actor.userName (alarmLoc, detector) @! ()))
                 //, OnAction ActionScript    (hasValue (\prio -> autoHandleWithScript (me, (alarmLoc, detector), (actorLoc, actor), prio) @! ()))
                 , OnAction ActionScript    (ifValue (\_ -> scriptExists) (\prio -> autoHandleWithScript (me, (alarmLoc, detector), (actorLoc, actor), prio) @! ()))
                 , OnAction ActionCancel    (always (return ()))
                 ]
            ) <<@ ArrangeVertical
  )
  where
  ActionByHand    = Action "By Hand"
  ActionSimulated = Action "Simulate"
  ActionScript    = Action "Simulate with Script"

  showAlarm :: !(!Coord3D, !SectionStatus) -> String
  showAlarm (alarmLoc, detector) = "Section " <+++ alarmLoc <+++ " : " <+++ toString detector <+++ "!"

  selectSomeOneToHandle :: !(!Coord3D, !SectionStatus) -> Task (!Coord3D, !MyActor)
  selectSomeOneToHandle (number, detector)
    = Hint ("Who should handle: " <+++ showAlarm (number, detector)) @>> enterChoiceWithShared 
        [ChooseFromGrid (\(roomNumber,actor) -> (roomNumber, actor.userName, actor.actorStatus))] allAvailableActors

  viewRelativeStatus :: !(!Coord3D, !MyActor) !(!Coord3D, !SectionStatus) -> Task ()
  viewRelativeStatus (actorLoc, actor) (alarmLoc, status)
    #! view = if (hasFire status) mkFireView
                (if (hasSmoke status) mkSmokeView
                   (if (hasWater status) mkWaterView
                      (\_ -> mkTable ["Status"] ["Everything in order"])
                   )
                )
    = viewSharedInformation [ViewUsing view grid] (sharedGraph |*| myStatusMap |*| myInventoryMap |*| lockedExitsShare |*| lockedHopsShare) @! ()
    where
    mkFireView ((((graph, statusMap), inventoryMap), exitLocks), hopLocks)
      #! (_,_,eCost,nrExt, (extLoc, distExt, _))                       = smartShipPathToClosestObject FireExtinguisher inventoryMap actorLoc alarmLoc statusMap exitLocks hopLocks graph
      #! (_,_,bCost,nrFireBlankets, (blanketLoc, distFireBlankets, _)) = smartShipPathToClosestObject FireBlanket      inventoryMap actorLoc alarmLoc statusMap exitLocks hopLocks graph
      #! fireDist                                                      = shipShortestPath actorLoc alarmLoc statusMap exitLocks hopLocks graph
      = mkTable [ "Object Description",                                           "Located in Section" ,   "Distance from " <+++ actor.userName, "Route Length"]
                [ ("Fire Alarm" ,                                                 roomToString alarmLoc,   spToDistString2 fireDist,             spToDistString2 fireDist)
                , ("Closest Extinquisher (" <+++ nrExt <+++ " in reach)",         roomToString extLoc,     toString distExt,                     toString eCost)
                , ("Closest FireBlanket (" <+++ nrFireBlankets <+++ " in reach)", roomToString blanketLoc, toString distFireBlankets,            toString bCost)
                ]
    mkSmokeView ((((graph, statusMap), inventoryMap), exitLocks), hopLocks)
      #! distance = shipShortestPath actorLoc alarmLoc statusMap exitLocks hopLocks graph
      = mkTable [ "Object Description", "Located in Section",     "Distance from " <+++ actor.userName, "Route Length"]
                [ ("Smoke Alarm",   roomToString alarmLoc, spToDistString2 distance, spToDistString2 distance )
                ]
    mkWaterView ((((graph, statusMap), inventoryMap), exitLocks), hopLocks)
      #! (_,_,pCost,nrPlugs, (plugLoc, distPlugs, _)) = smartShipPathToClosestObject Plug inventoryMap actorLoc alarmLoc statusMap exitLocks hopLocks graph
      #! floodDist                                    = shipShortestPath actorLoc alarmLoc statusMap exitLocks hopLocks graph
      = mkTable [ "Object Description",                             "Located in Section",  "Distance from " <+++ actor.userName, "Route Length"]
                [ ("Flood Alarm",                                   roomToString alarmLoc, spToDistString2 floodDist,            spToDistString2 floodDist)
                , ("Closest plug (" <+++ nrPlugs <+++ " in reach)", roomToString plugLoc,  toString distPlugs,                   toString pCost)
                ]

:: MoveSt a
  = MoveDone a
  | MoveFailed String
  | MoveBusy

derive class iTask MoveSt

handleAlarm :: !(!User, !(!Coord3D, !SectionStatus), !(!Coord3D, !MyActor), !Priority) -> Task ()
handleAlarm (me, (alarmLoc, detector), (actorLoc, actor), priority)
  =   updStatusOfActor actor.userName Busy
  >>| addLog ("D-Off " <+++ me) actor.userName (message "Start Handling ")
  >>| appendTopLevelTaskPrioFor me (message "Wait for ") "High" True
        (handleWhileWalking actor (message "Handle ") priority) @! ()
  where
  message :: !String -> String
  message work = (work <+++ toString detector <+++ " in Section " <+++ alarmLoc)

  handleWhileWalking :: !MyActor !String !Priority -> Task ()
  handleWhileWalking actor title priority
    =   addTaskForUser title actor.userName Immediate (const taskToHandle)
    >>* [ OnValue  (ifValue isDone (\x -> Title ("Task " <+++ title <+++ " succeeded, returning:") @>> viewInformation [] x @! ()))
        , OnValue  (ifValue isFailed (\x -> Title ("Task " <+++ title <+++ " failed, returning:") @>> viewInformation  [] x @! ()))
        , OnAction (Action "Cancel task") (always (Title "Canceled" @>> viewInformation [] ("Task " <+++ title <+++ " has been cancelled by you") @! ()))
        ]
    >>| return ()
    where
    taskToHandle
      =   moveAround mkSection actor.userName inventoryInSectionShare myStatusMap myUserActorMap myInventoryMap
      ||- taskToDo (alarmLoc, detector) actor.userName myStatusMap myUserActorMap myInventoryMap
    isDone (MoveDone _) = True
    isDone _            = False
    isFailed (MoveFailed _) = True
    isFailed _              = False

  taskToDo :: !(!Coord3D, !SectionStatus) !User !(Shared sds1 MySectionStatusMap) !(UserActorShare o a) !(Shared sds2 MySectionInventoryMap)
           -> Task (MoveSt String) | RWShared sds1 & RWShared sds2
  taskToDo (alarmLoc, status) user shStatusMap shUserActor shInventoryMap
    =   Hint ("Handle " <+++ toString status <+++ " in Section: " <+++ alarmLoc) @>> viewSharedInformation  [ViewAs todoTable] (sectionForUserShare user |*| myUserActorMap |*| shStatusMap |*| shInventoryMap |*| lockedExitsShare |*| lockedHopsShare |*| sharedGraph)
    >>* [ OnAction (Action "Use Fire Extinguisher") (ifValue (mayUseExtinguisher status) (withUser useExtinquisher))
        , OnAction (Action "Use FireBlanket")       (ifValue (mayUseFireBlanket status)  (withUser useFireBlanket))
        , OnAction (Action "Use Plug")              (ifValue (mayUsePlug status)         (withUser usePlug))
        , OnAction (Action "Smoke Investigated")    (ifValue (mayDetectedSmoke status)   (withUser smokeReport))
        , OnAction (Action "I give up")             (hasValue (withUser giveUp))
        ]
    where
    todoTable ((((((Just curSectionNo, userActorMap), statusMap), inventoryMap), exitLocks), hopLocks), curMap)
      # path                                                                          = shipShortestPath curSectionNo alarmLoc statusMap exitLocks hopLocks curMap
      # (_, _, eCost, nrExt,          (extLoc, distExt, dirExt))                      = smartShipPathToClosestObject FireExtinguisher inventoryMap curSectionNo alarmLoc statusMap exitLocks hopLocks curMap
      # (_, _, bCost, nrFireBlankets, (blanketLoc, distFireBlankets, dirFireBlanket)) = smartShipPathToClosestObject FireBlanket      inventoryMap curSectionNo alarmLoc statusMap exitLocks hopLocks curMap
      # (_, _, pCost, nrPlugs,        (plugLoc, distPlugs, dirPlug))                  = smartShipPathToClosestObject Plug             inventoryMap curSectionNo alarmLoc statusMap exitLocks hopLocks curMap
      = mkTable [ "Object Description",                                           "Located in Section",    "Take Exit",         "Distance from " <+++ user, "Route Length"]
                [ (toString status,                                               roomToString alarmLoc,   goto2 path,          spToDistString2 path,       spToDistString2 path)
                , ("Closest Extinquisher (" <+++ nrExt <+++ " in reach)",         roomToString extLoc,     goto dirExt,         toString distExt,           toString eCost)
                , ("Closest FireBlanket (" <+++ nrFireBlankets <+++ " in reach)", roomToString blanketLoc, goto dirFireBlanket, toString distFireBlankets,  toString bCost)
                , ("Closest plug (" <+++ nrPlugs <+++ " in reach)",               roomToString plugLoc,    goto dirPlug,        toString distPlugs,         toString pCost)
                ]
    todoTable _ = mkTable ["Impossible!"] ["Impossible!"]

    withCurActor ((((((Just curSectionNo, userActorMap), _), _), _), _), _) f = maybe False (f curSectionNo) ('DM'.get user userActorMap)
    withCurActor _ _ = False

    mayUseExtinguisher status r = withCurActor r (\curSectionNo curActor -> hasFire status && curSectionNo == alarmLoc && isCarrying FireExtinguisher curActor)
    mayUseFireBlanket status r = withCurActor r (\curSectionNo curActor -> hasFire status && curSectionNo == alarmLoc && isCarrying FireBlanket curActor)
    mayUsePlug status r = withCurActor r (\curSectionNo curActor -> hasWater status && curSectionNo == alarmLoc && isCarrying Plug curActor)
    mayDetectedSmoke status ((((((Just curSectionNo, _), _), _), _), _), _) = hasSmoke status && curSectionNo == alarmLoc

    withUser f ((((((_, userActorMap), _), _), _), _), _)
      = case 'DM'.get user userActorMap of
          Just curActor -> f curActor
          _             -> return (MoveFailed "Failed to find actor!")

    useExtinquisher curActor
      =   useObject alarmLoc (getObjectOfType curActor FireExtinguisher) user myUserActorMap inventoryInSectionShare
      >>| setAlarm actor.userName (alarmLoc, NormalStatus) myStatusMap
      >>| updStatusOfActor curActor.userName Available
      >>| viewInformation [] "Well Done, Fire Extinguished !" @! ()
      >>| return (MoveDone "Fire Extinguised")

    useFireBlanket curActor
      =   useObject alarmLoc (getObjectOfType curActor FireBlanket) user myUserActorMap inventoryInSectionShare
      >>| setAlarm actor.userName (alarmLoc, NormalStatus) myStatusMap
      >>| updStatusOfActor curActor.userName Available
      >>| viewInformation [] "Well Done, Fire Extinguished !" @! ()
      >>| return (MoveDone "Fire Extinguised")

    usePlug curActor
      =   useObject alarmLoc (getObjectOfType curActor Plug) user myUserActorMap inventoryInSectionShare
      >>| setAlarm actor.userName (alarmLoc, NormalStatus) myStatusMap
      >>| updStatusOfActor curActor.userName Available
      >>| viewInformation [] "Well Done, Flooding Stopped !" @! ()
      >>| return (MoveDone "Flooding Stopped")

    smokeReport curActor
      =   setAlarm actor.userName (alarmLoc, NormalStatus) myStatusMap
      >>| updStatusOfActor curActor.userName Available
      >>| viewInformation [] "Well Done, Reason of Smoke Detected !" @! ()
      >>| return (MoveDone "Don't smoke under a smoke detector!")

    giveUp curActor
      =   updStatusOfActor curActor.userName Available
      >>| return (MoveFailed "I gave up, send somebody else...")

goto :: !(Maybe [Coord3D]) -> String
goto Nothing      = "Unreachable!"
goto (Just [])    = "you are already in the target room"
goto (Just (dir)) = toString (hd dir)

goto2 :: !(Maybe ([Coord3D], a)) -> String
goto2 Nothing        = "Unreachable!"
goto2 (Just ([],_))  = "you are already in the target room"
goto2 (Just (dir,_)) = toString (hd dir)

updStatusOfActor :: !User !Availability -> Task ()
updStatusOfActor user availability
  =   updActorStatus user (\st -> {st & occupied = availability}) myUserActorMap
  >>| addLog user "" ("Has become " <+++ availability)

scriptDefined :: !SectionStatus -> Task Bool
scriptDefined status
  | hasFire status  = get handleFireScript  >>- \script -> return (length script > 0)
  | hasWater status = get handleFloodScript >>- \script -> return (length script > 0)
  | hasSmoke status = get handleSmokeScript >>- \script -> return (length script > 0)
  | otherwise       = return False

autoHandleWithScript :: !(!User, !(!Coord3D, !SectionStatus), !(!Coord3D, !MyActor), !Priority) -> Task ()
autoHandleWithScript (commander, (alarmLoc, status), (actorLoc, actor), prio)
  | hasFire status  = get handleFireScript  >>= continueWithScript
  | hasWater status = get handleFloodScript >>= continueWithScript
  | hasSmoke status = get handleSmokeScript >>= continueWithScript
  | otherwise       = return ()
  where
  continueWithScript :: ![Script] -> Task ()
  continueWithScript script
    = appendTopLevelTaskPrioFor actor.userName ("Auto script " <+++ toString status <+++ " in room " <+++ alarmLoc) "High" True
      (   updStatusOfActor actor.userName Busy
      >>| addLog ("Commander " <+++ commander) actor.userName ("Simulate Handling " <+++ toString status <+++ " detected in " <+++ alarmLoc)
      >>| interperScript (alarmLoc, status) actor.userName script // perform script (actorLoc,actor)
      >>| updStatusOfActor actor.userName Available
      >>| addLog actor.userName commander ("Simulation Handling " <+++ toString status <+++ " in room " <+++ alarmLoc <+++ " Finished " <+++ if True "Succesfully" "Failed")
      ) @! ()

// simulate via auto stuf

autoHandleAlarm :: !User !User !(!Coord3D, !SectionStatus) -> Task ()
autoHandleAlarm commander user (alarmLoc, status)
  = appendTopLevelTaskPrioFor user ("Auto handling " <+++ toString status <+++ " in room " <+++ alarmLoc) "High" True
      (startSimulation commander user (alarmLoc, status)) @! ()

startSimulation :: !User !User !(!Coord3D, !SectionStatus) -> Task Bool
startSimulation commander user (alarmLoc, status)
  =   updStatusOfActor user Busy
  >>| addLog ("Commander " <+++ commander) user ("Simulate Handling " <+++ toString status <+++ " detected in " <+++ alarmLoc)
  >>| get (myStatusMap |*| sectionUsersShare |*| myUserActorMap |*| lockedExitsShare |*| lockedHopsShare |*| myInventoryMap |*| sharedGraph)
  >>- \((((((statusMap, mam), usersActorMap), exitLocks), hopLocks), invMap), graph) ->
        case findUser user mam usersActorMap of
          Just (myLoc, curActor)
            = case findClosestObject status statusMap invMap myLoc alarmLoc exitLocks hopLocks graph of
                (Nothing, _)
                  =   addLog ("Commander " <+++ commander) user "startSimulation: closest object not found"
                  >>| endSimulation False
                (Just loc, mbObj)
                  = (case mbObj of
                       Nothing  = simulateHandling myLoc alarmLoc status curActor
                       Just obj = simulateHandlingWithObject myLoc obj loc alarmLoc status curActor.userName
                    )
                    >>| endSimulation True
          _ =   addLog ("Commander " <+++ commander) user "startSimulation: actor not found"
            >>| endSimulation False
  where
  endSimulation :: !Bool -> Task Bool
  endSimulation ok
    =   updStatusOfActor user Available
    >>| addLog user commander  ("Simulation Handling " <+++ toString status <+++ " in room " <+++ alarmLoc <+++ " Finished " <+++ if True "Succesfully" "Unsuccesfully")
    >>| return ok


simulateHandling :: !Coord3D !Coord3D !SectionStatus !MyActor -> Task Bool
simulateHandling startLoc alarmLoc status actor
  =                     autoMove startLoc alarmLoc shipShortestPath actor.userName myStatusMap myUserActorMap
  >>- \targetReached -> if targetReached
                          (setAlarm actor.userName (alarmLoc, NormalStatus) myStatusMap @! True)
                          (return False)

simulateHandlingWithObject :: !Coord3D !MyObject !Coord3D !Coord3D !SectionStatus !User
                           -> Task Bool
simulateHandlingWithObject startLoc object objectLoc alarmLoc status user
  =                     addLog user "Started Simulation" ("From " <+++ startLoc <+++
                                                          " to " <+++	alarmLoc <+++
                                                          " via " <+++ objectLoc <+++
                                                          " fetching " <+++ object)
  >>|                   autoMove startLoc objectLoc shipShortestPath user myStatusMap myUserActorMap
  >>= \objectReached -> if objectReached (pickupObject objectLoc object user myUserActorMap inventoryInSectionShare
  >>|                                    autoMove objectLoc alarmLoc shipShortestPath user myStatusMap myUserActorMap
  >>= \targetReached -> if targetReached (waitForTimer 1
  >>|                                    useObject alarmLoc object user myUserActorMap inventoryInSectionShare
  >>= \used          -> if used          (setAlarm user (alarmLoc, NormalStatus) myStatusMap @! True)
                                         (return False))
                                         (return False))
                                         (return False)

findClosestObject :: !SectionStatus !MySectionStatusMap !MySectionInventoryMap !Coord3D !Coord3D !SectionExitLockMap !SectionHopLockMap !Graph
                  -> (!Maybe Coord3D, !Maybe MyObject)
findClosestObject status statusMap inventoryMap myLoc alarmLoc exitLocks hopLocks graph
  | hasSmoke status = (Just myLoc, Nothing)
  | hasWater status = case findClosest Plug statusMap inventoryMap myLoc alarmLoc exitLocks hopLocks graph of
                        Nothing -> (Nothing, Nothing)
                        Just (obj, _, roomNo)  -> (Just roomNo, Just obj)
  | hasFire status
    #! fireLoc    = findClosest FireExtinguisher statusMap inventoryMap myLoc alarmLoc exitLocks hopLocks graph
    #! blanketLoc = findClosest FireBlanket      statusMap inventoryMap myLoc alarmLoc exitLocks hopLocks graph
    = case (fireLoc, blanketLoc) of
        (Just (obj, _, roomNo), Nothing) -> (Just roomNo, Just obj)
        (Nothing, Just (obj, _, roomNo)) -> (Just roomNo, Just obj)
        (Just (obj1, dist1, roomNo1), Just (obj2, dist2, roomNo2))
          | less dist1 dist2 -> (Just roomNo1, Just obj1)
          | otherwise        -> (Just roomNo2, Just obj2)
        _                    -> (Nothing, Nothing)
  | otherwise = (Nothing, Nothing)
  where
  less :: Int Int -> Bool
  less c1 c2
  | c1 >= 0 && c2 >= 0 = c1 < c2

findClosest :: !ObjectType !MySectionStatusMap !MySectionInventoryMap !Coord3D !Coord3D !SectionExitLockMap !SectionHopLockMap !Graph -> Maybe (!MyObject, !Int, !Coord3D)
findClosest objectType statusMap inventoryMap myLoc targetLoc exitLocks hopLocks graph
  = case smartShipPathToClosestObject objectType inventoryMap myLoc targetLoc statusMap exitLocks hopLocks graph of
      (Just obj, cost,_,_, (_, distance, Just path))
        = case reverse path of
            []    -> Just (obj, cost, myLoc)
            [x:_] -> Just (obj, cost, x)
      _ = Nothing

mkSection :: MyDrawMapForActor
mkSection
  = \user shStatusMap shUserActor shSectionInventoryMap ->
      Title "Section Status" @>> updateSharedInformation 
		  [UpdateSharedUsing id (\_ _ -> ()) const editor]
          (sectionForUserShare user |*| myNetwork |*| myDevices |*| shStatusMap |*| sectionUsersShare |*| myUserActorMap |*| shSectionInventoryMap |*| lockedExitsShare |*| lockedHopsShare |*| maps2DShare)
          @! ()
  where
  getFloorIdx (Just (floorIdx, _)) = floorIdx
  getFloorIdx _                    = -1

  editor = fromSVGEditor
	{ initView    = const ([], NoAction)
	, renderImage = \(((((((((mc3d, network), allDevices), statusMap), sectionUsersMap), userActorMap), invMap), exitLocks), hopLocks), ms2d) _ _ -> map2DImage 'DS'.newSet NoAction WalkAroundMode exitLocks hopLocks invMap statusMap sectionUsersMap userActorMap allDevices network (getFloorIdx mc3d, ms2d !! getFloorIdx mc3d)
	, updModel = \m v -> m
	}
