definition module C2.Apps.ShipAdventure.PathFinding

import C2.Apps.ShipAdventure.Types


// given object to search for, current location and current map
smartShipPathToClosestObject :: !ObjectType !MySectionInventoryMap !Coord3D !Coord3D !MySectionStatusMap !SectionExitLockMap !SectionHopLockMap !Graph
                             -> (!Maybe MyObject, !Int, !Distance, !Int, !(!Coord3D, !Distance, !Maybe [Coord3D]))

// given object to search for, current location, target room to move to with object, and current map
shipShortestPath :: !Coord3D !Coord3D !MySectionStatusMap !SectionExitLockMap !SectionHopLockMap !Graph
                 -> Maybe (![Coord3D], !Distance)

// shortest path given the alarms set on the ship

