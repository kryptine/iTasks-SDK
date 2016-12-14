definition module C2.Navy.Roles.Suspect

import iTasks
import C2.Framework.Entity

suspectRegisterEntity  :: [User -> Task Entity]
suspectContinuousTasks :: [         User [Entity] -> Task ()]
suspectAlwaysOnTasks   :: [(String, User [Entity] -> Task ())]
suspectOptionalTasks   :: [(String, User [Entity] -> Task ())]
