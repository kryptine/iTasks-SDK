definition module C2.Navy.Roles.Commander

import iTasks
import C2.Framework.Entity

commanderRegisterEntity  :: [User -> Task Entity]
commanderContinuousTasks :: [         User [Entity] -> Task ()]
commanderAlwaysOnTasks   :: [(String, User [Entity] -> Task ())]
commanderOptionalTasks   :: [(String, User [Entity] -> Task ())]
