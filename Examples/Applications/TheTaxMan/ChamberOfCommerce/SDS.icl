implementation module ChamberOfCommerce.SDS

import ChamberOfCommerce.UoD

companies :: SDSLens () [Company] [Company] 
companies = sharedStore "companies" []

companiesOfType :: CompanyType -> SDSLens () [Company] [Company]
companiesOfType type = mapRead (filter (companyHasType type)) companies
