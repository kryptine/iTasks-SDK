definition module ChamberOfCommerce.SDS

import ChamberOfCommerce.UoD

/** companies:
		this shared data source keeps track of all registered companies.
*/
companies :: SDSLens () [Company] [Company]

/** companiesOfType t:
		this shared data source is the subset of *companies* of type @t.
*/
companiesOfType :: CompanyType -> SDSLens () [Company] [Company]
