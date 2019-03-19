implementation module ChamberOfCommerce.SDS

import ChamberOfCommerce.UoD

companies :: SimpleSDSLens [Company]
companies = sharedStore "companies" []

companiesOfType :: CompanyType -> SimpleSDSLens [Company]
companiesOfType type = mapRead (filter (companyHasType type)) companies
