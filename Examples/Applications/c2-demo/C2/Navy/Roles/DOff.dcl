definition module C2.Navy.Roles.DOff

import iTasks
import C2.Framework.Entity

dOffRegisterEntity  :: [User -> Task Entity]
dOffContinuousTasks :: [         User [Entity] -> Task ()]
dOffAlwaysOnTasks   :: [(String, User [Entity] -> Task ())]
dOffOptionalTasks   :: [(String, User [Entity] -> Task ())]
dOffMap             :: Task ()
