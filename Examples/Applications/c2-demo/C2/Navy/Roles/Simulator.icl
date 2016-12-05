implementation module C2.Navy.Roles.Simulator

import iTasks
import C2.Framework.Entity, C2.Framework.Common
import C2.Apps.ShipAdventure.Scripting, C2.Apps.ShipAdventure.Core, C2.Apps.ShipAdventure.Types, C2.Framework.Logging

simulatorRegisterEntity  :: [User -> Task Entity]
simulatorRegisterEntity = []

simulatorContinuousTasks :: [User [Entity] -> Task ()]
simulatorContinuousTasks = []

simulatorAlwaysOnTasks :: [(String, User [Entity] -> Task ())]
simulatorAlwaysOnTasks = [ ("Manage alarms", \_ _ -> setSectionDetectors)
                         ]

simulatorOptionalTasks :: [(String, User [Entity] -> Task ())]
simulatorOptionalTasks = [("Show log", \_ _ -> showLog @! ())]
