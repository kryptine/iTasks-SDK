implementation module C2.Apps.ShipAdventure.PathFinding

import C2.Framework.MapEnvironment
import C2.Apps.ShipAdventure.Types

// returns: distance, number of objects found, location of object, distance to object, shortest path to obejct
//shipPathToClosestObject :: Object Coord3D MyMap -> (Int,(Coord3D,Distance, Maybe ([Exit], Distance)))  
//shipPathToClosestObject kind actorLoc curMap = pathToClosestObject shipShortestPath kind actorLoc curMap

smartShipPathToClosestObject :: !ObjectType !MySectionInventoryMap !Coord3D !Coord3D !MySectionStatusMap !SectionExitLockMap !SectionHopLockMap !Graph
                             -> (!Maybe MyObject, !Int, !Distance, !Int, !(!Coord3D, !Distance, !Maybe [Coord3D]))
smartShipPathToClosestObject kind inventoryMap actorLoc targetLoc statusMap exitLocks hopLocks curMap = smartPathToClosestObject shipShortestPath kind actorLoc targetLoc statusMap inventoryMap exitLocks hopLocks curMap


// shortest path given the alarms set on the ship

shipShortestPath :: !Coord3D !Coord3D !MySectionStatusMap !SectionExitLockMap !SectionHopLockMap !Graph -> Maybe (![Coord3D], !Distance)
shipShortestPath startCoord3D endCoord3D statusMap exitLocks hopLocks graph = shortestPath cost startCoord3D endCoord3D statusMap exitLocks hopLocks graph
  where
  cost :: !SectionStatus -> Int
  cost status = 1 + statusCost status
  statusCost :: !SectionStatus -> Int
  statusCost HasSomeWater  = 500
  statusCost IsFlooded     = 1000
  statusCost HasSmoke      = 400
  statusCost HasSmallFire  = 500
  statusCost HasMediumFire = 750
  statusCost HasBigFire    = 1000
  statusCost _             = 0

