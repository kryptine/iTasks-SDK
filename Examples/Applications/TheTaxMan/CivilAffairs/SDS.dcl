definition module CivilAffairs.SDS

import CivilAffairs.UoD

/** citizens:
		this shared data source keeps track of all registered citizens.
*/
citizens :: SimpleSDSLens [Citizen]
