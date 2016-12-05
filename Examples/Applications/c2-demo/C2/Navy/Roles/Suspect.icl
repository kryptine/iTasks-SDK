implementation module C2.Navy.Roles.Suspect

import iTasks
import C2.Framework.Entity, C2.Framework.Common
import C2.Apps.ShipAdventure.Scripting, C2.Apps.ShipAdventure.Core, C2.Apps.ShipAdventure.Types, C2.Framework.Logging
import Math.Geometry

suspectRegisterEntity  :: [User -> Task Entity]
suspectRegisterEntity = [\_ -> registerEntity (\idx -> newMovingEntityWithSpeedAndDirection idx (deg 2.32, deg 46.34) 500.0 (deg 225.0))
                        ]

suspectContinuousTasks :: [User [Entity] -> Task ()]
suspectContinuousTasks = [ \_ ses -> allTasks (map (\{Entity|e_id} -> periodicallyUpdateEntity e_id) ses) @! ()
                         ]

suspectAlwaysOnTasks :: [(String, User [Entity] -> Task ())]
suspectAlwaysOnTasks = [("Map", mapView myNetwork (const True))]

suspectOptionalTasks :: [(String, User [Entity] -> Task ())]
suspectOptionalTasks = [("Show log", \_ _ -> showLog @! ())]
