implementation module CivilAffairs.SDS

import CivilAffairs.UoD

citizens :: SDSLens () [Citizen] [Citizen]
citizens = sharedStore "citizens" []
