implementation module ChamberOfCommerce.SDS

import ChamberOfCommerce.UoD

companies :: Shared [Company]
companies = sharedStore "companies" []

companiesOfType :: CompanyType -> Shared [Company]
companiesOfType type = mapRead (filter (companyHasType type)) companies 
