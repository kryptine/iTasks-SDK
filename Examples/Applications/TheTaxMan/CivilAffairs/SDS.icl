implementation module CivilAffairs.SDS

import CivilAffairs.UoD

citizens :: SimpleSDSLens [Citizen]
citizens = sharedStore "citizens" []
