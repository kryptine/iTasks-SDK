implementation module C2.Apps.ShipAdventure.Scripting

import iTasks

import C2.Apps.ShipAdventure.Types
import C2.Apps.ShipAdventure.PathFinding
import C2.Apps.ShipAdventure.Util
import qualified Data.IntMap.Strict as DIS
import qualified Data.Map as DM

// scripted simulation

derive class iTask Target, Script, Condition

handleFireScript :: SDSLens () [Script] [Script]
handleFireScript = sharedStore "handleFireScript" []

handleFloodScript :: SDSLens () [Script] [Script]
handleFloodScript = sharedStore "handleFloodScript" []

handleSmokeScript :: SDSLens () [Script] [Script]
handleSmokeScript = sharedStore "handleSmokeScript" []

changeFireScript :: Task ()
changeFireScript = changeScript "Handling Fire" handleFireScript

changeFloodScript :: Task ()
changeFloodScript = changeScript "Handling Flood" handleFloodScript

changeSmokeScript :: Task ()
changeSmokeScript = changeScript "Handling Smoke" handleSmokeScript

changeScript :: !String !(Shared sds [Script]) -> Task () | RWShared sds
changeScript prompt script
  =   viewSharedInformation ("Current Script: " <+++ prompt) [ViewAs (\script -> [toString i +++ " : " +++ line \\ line <- map toSingleLineText script & i <- [1..]])] script
  >>* [ OnAction (Action "Fine") (always (return ()))
      , OnAction (Action "Change") (always (   updateSharedInformation ("Change Script: " <+++ prompt) [] script
                                              >>| changeScript prompt script
                                              ))
      ]

interperScript :: !(!Coord3D, !SectionStatus) !User ![Script] -> Task Bool
interperScript (targetSection, status) user script
  =                get (sectionUsersShare |*| myUserActorMap)
  >>- \(sectionUsersMap, userActorMap) -> case findUser user sectionUsersMap userActorMap of
                     Just user -> perform script user
                     _         -> return False
  where
  perform :: ![Script] !(!Coord3D, !MyActor) -> Task Bool
  perform [] _ = return True

  perform [MoveTo target:next] (actorLoc,actor)
    =   get (myStatusMap |*| myInventoryMap |*| lockedExitsShare |*| lockedHopsShare |*| sharedGraph)
    >>- \((((statusMap, invMap), exitLocks), hopLocks), graph) ->
          let newLoc = whereIs targetSection target actorLoc statusMap invMap exitLocks hopLocks graph
          in      autoMove actorLoc newLoc shipShortestPath actor.userName myStatusMap myUserActorMap
              >>| perform next (newLoc,actor)
  perform [Take objType`:next] (actorLoc,actor)
    =              get myInventoryMap
    >>- \invMap -> case 'DM'.get actorLoc invMap of
                      Just inv
                        = case [obj \\ obj=:{Object | objType } <- 'DIS'.elems inv | objType` == objType] of
                            [obj : _] =   pickupObject actorLoc obj actor.userName myUserActorMap inventoryInSectionShare
                                      >>| perform next (actorLoc,actor)
                            _ = perform next (actorLoc, actor)
                      _ = perform next (actorLoc, actor)
  perform [Drop objType`:next] (actorLoc,actor)
      = case [obj \\ obj=:{Object | objType } <- actor.carrying | objType` == objType] of
          [obj : _]
            =   dropObject actorLoc obj actor.userName myUserActorMap inventoryInSectionShare
            >>| perform next (actorLoc,actor)
          _ = perform next (actorLoc,actor)
  perform [Use objType`:next] (actorLoc,actor)
      = case [obj \\ obj=:{Object | objType } <- actor.carrying | objType` == objType] of
          [obj : _]
            =   useObject actorLoc obj actor.userName myUserActorMap inventoryInSectionShare
            >>| perform next (actorLoc,actor)
          _ = perform next (actorLoc,actor)
  perform [ReSetTargetDetector:next] (actorLoc,actor)
    =   setAlarm actor.userName (targetSection, NormalStatus) myStatusMap
    >>| perform next (actorLoc,actor)
  perform [If condition script1 script2:next] (actorLoc,actor)
    =              get myInventoryMap
    >>= \invMap -> case 'DM'.get actorLoc invMap of
                     Just inv
                       | isTrue ('DIS'.elems inv) condition (actorLoc,actor) = perform (script1 ++ next) (actorLoc, actor)
                     _                                                       = perform (script2 ++ next) (actorLoc, actor)
  isTrue :: ![MyObject] !Condition !(!Coord3D, !MyActor) -> Bool
  isTrue inv (ObjectInCurrentSection object) (actorLoc,actor)
    = objTypeInList object inv
  isTrue inv (CarriesObject object) (actorLoc,actor)
  	= isCarrying object actor
  isTrue inv (ActorStatus status) (actorLoc,actor)
  	= status === actor.actorStatus
  isTrue inv (And cond1 cond2) (actorLoc,actor)
  	= and [isTrue inv cond1 (actorLoc,actor), isTrue inv cond2 (actorLoc,actor)]
  isTrue inv (Or cond1 cond2) (actorLoc,actor)
  	= or [isTrue inv cond1 (actorLoc,actor), isTrue inv cond2 (actorLoc,actor)]

  whereIs :: Coord3D !Target Coord3D MySectionStatusMap MySectionInventoryMap SectionExitLockMap SectionHopLockMap Graph -> Coord3D
  whereIs _ (Section nr) _ _ _ _ _ _                                = nr
  whereIs targetSection (Nearest object) actorLoc statusMap inventoryMap exitLocks hopLocks graph
    #! (_,_,_,_,(objectLoc,_,_)) = smartShipPathToClosestObject object inventoryMap actorLoc targetSection statusMap exitLocks hopLocks graph
    = objectLoc
  whereIs targetSection TargetSection _ _ _ _ _ _                               = targetSection

