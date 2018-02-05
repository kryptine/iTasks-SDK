definition module C2.Navy.Roles.HVU

import iTasks
import C2.Framework.Entity

hvuRegisterEntity  :: [User -> Task Entity]
hvuContinuousTasks :: [         User [Entity] -> Task ()]
hvuAlwaysOnTasks   :: [(String, User [Entity] -> Task ())]
hvuOptionalTasks   :: [(String, User [Entity] -> Task ())]
