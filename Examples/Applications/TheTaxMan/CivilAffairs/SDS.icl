implementation module CivilAffairs.SDS

import CivilAffairs.UoD

citizens :: Shared [Citizen]
citizens = sharedStore "citizens" []
