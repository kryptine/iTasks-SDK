definition module C2.Navy.Roles.Simulator

import iTasks
import C2.Framework.Entity

simulatorRegisterEntity  :: [User -> Task Entity]
simulatorContinuousTasks :: [         User [Entity] -> Task ()]
simulatorAlwaysOnTasks   :: [(String, User [Entity] -> Task ())]
simulatorOptionalTasks   :: [(String, User [Entity] -> Task ())]
