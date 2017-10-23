definition module ChamberOfCommerce.UoD

import iTasks

/** Company:
		keep track of company information in the Chamber of Commerce Register.
*/
:: Company
	= 	{ cocNo   :: COCN			/** the unique Chamber of Commerce number of the company (serves as key) */
		, cocName :: String			/** the official company name */
		, type    :: [CompanyType]	/** the type of the company */
		}
/** COCN:
		the Chamber of Commerce number, uniquely distributed to all registered companies
*/
:: COCN
	:== String

/** CompanyType:
		the type of the company (for simplicity, a single string that serves as key in its entirety)
*/
:: CompanyType
	:== String

derive class iTask Company

/** companyHasType type company:
		returns True only if one of the elements of @company.type is @type.
*/
companyHasType :: CompanyType Company -> Bool

/** == c1 c2:
		yields True only if @c1.cocNo == @c2.cocNo
*/
instance == Company

/** < c1 c2:
		yields True only if @c1.cocNo < @c2.cocNo
*/
instance <  Company
