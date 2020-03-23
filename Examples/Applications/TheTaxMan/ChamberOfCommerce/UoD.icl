implementation module ChamberOfCommerce.UoD

import iTasks

derive class iTask Company

companyHasType :: CompanyType Company -> Bool
companyHasType ct {Company | type} = isMember ct type

instance == Company where (==) a1 a2 = a1.Company.cocNo == a2.Company.cocNo
instance <  Company where (<)  a1 a2 = a1.Company.cocNo <  a2.Company.cocNo
