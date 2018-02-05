implementation module C2.Navy.Roles.HVU

import iTasks
import C2.Framework.Entity, C2.Framework.Common
import C2.Apps.ShipAdventure.Scripting, C2.Apps.ShipAdventure.Core, C2.Apps.ShipAdventure.Types, C2.Framework.Logging
import Math.Geometry

hvuRegisterEntity  :: [User -> Task Entity]
hvuRegisterEntity = [\_ -> registerEntity (\idx -> newMovingEntityWithSpeedAndDirection idx (deg 1.61, deg 45.27) 500.0 (deg 45.0))
                    ]

hvuContinuousTasks :: [User [Entity] -> Task ()]
hvuContinuousTasks = [ \_ ses -> allTasks (map (\{Entity|e_id} -> periodicallyUpdateEntity e_id) ses) @! ()
                     ]

hvuAlwaysOnTasks :: [(String, User [Entity] -> Task ())]
hvuAlwaysOnTasks = [("Map", mapView myNetwork (const True))]

hvuOptionalTasks :: [(String, User [Entity] -> Task ())]
hvuOptionalTasks = [("Show log", \_ _ -> showLog @! ())]
