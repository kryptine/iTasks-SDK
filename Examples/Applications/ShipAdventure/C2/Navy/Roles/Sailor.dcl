definition module C2.Navy.Roles.Sailor

import iTasks
import C2.Framework.Entity

sailorRegisterEntity  :: [User -> Task Entity]
sailorContinuousTasks :: [         User [Entity] -> Task ()]
sailorAlwaysOnTasks   :: [(String, User [Entity] -> Task ())]
sailorOptionalTasks   :: [(String, User [Entity] -> Task ())]
