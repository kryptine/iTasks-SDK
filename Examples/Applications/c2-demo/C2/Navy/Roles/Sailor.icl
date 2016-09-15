implementation module C2.Navy.Roles.Sailor

import iTasks
import C2.Framework.Entity, C2.Framework.Common
import C2.Apps.ShipAdventure.Scripting, C2.Apps.ShipAdventure.Core, C2.Apps.ShipAdventure.Types, C2.Framework.Logging

sailorRegisterEntity  :: [User -> Task Entity]
sailorRegisterEntity = []

sailorContinuousTasks :: [User [Entity] -> Task ()]
sailorContinuousTasks = []

sailorAlwaysOnTasks :: [(String, User [Entity] -> Task ())]
sailorAlwaysOnTasks = [("Walk around", \_ _ -> currentUserWalkAround)]

sailorOptionalTasks :: [(String, User [Entity] -> Task ())]
sailorOptionalTasks = []
