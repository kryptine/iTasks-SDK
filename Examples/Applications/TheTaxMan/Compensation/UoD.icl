implementation module Compensation.UoD

import Cadastre.UoD
import iTasks.Extensions.DateTime, Data.Maybe
from Data.Either import :: Either(..)

decisionsAfter :: SSN (DecisionStatus -> Bool) Date [Decision] -> [Decision]
decisionsAfter ssn cond date decisions
	= [decision \\ decision <- decisions | ssn == decision.Decision.ssn && cond (decision.Decision.status) && decision.Decision.date >= date]

collectionsAfter :: SSN Date [Collection] -> [Collection]
collectionsAfter ssn date collections
	= [collection \\ collection <- collections | collection.Collection.ssn == ssn && collection.Collection.date >= date]

realEstatesOfCitizen :: Citizen [CadastreRealEstate] -> [OwnedRealEstate]
realEstatesOfCitizen citizen cadastre
	= [{ OwnedRealEstate
	   | isHomeAddress = citizen.Citizen.homeAddress == Just address
	   , postcode      = address.Address.postcode
	   , houseNumber   = address.Address.houseNumber
	   }
	  \\ entry <- cadastre
	   , let address = entry.CadastreRealEstate.address
	   | isMember citizen.Citizen.ssn [ssn \\ Left ssn <- [entry.mainOwner : entry.subOwners]]
	  ]

clientReminderDate :: Date -> Date
clientReminderDate date = shiftDate (Months 2) date

clientDeadlineDate :: Date -> Date
clientDeadlineDate date = shiftDate (Months 3) date

/** TimePeriod:
		is an indication of a time period. The values are allowed to be negative.
*/
::	TimePeriod
	= Years  Int
	| Months Int
	| Days   Int

shiftDate :: TimePeriod Date -> Date
shiftDate (Years n) date=:{Date | year}
	= {Date | date & year = year + n}
shiftDate (Months n) date=:{Date | year,mon,day}
| years <> 0					= shiftDate (Months months) (shiftDate (Years years) date)
| 1 <= month` && month` <= 12	= {Date | date &                mon = month`,        day = min day (daysInMonth  year     month`)}
| month` < 1					= {Date | date & year = year-1, mon = 12-abs month`, day = min day (daysInMonth (year-1) (12-abs month`))}
| otherwise						= {Date | date & year = year+1, mon = month`-12,     day = min day (daysInMonth (year+1) (month`-12))}
where
	years						= sign n * (abs n / 12)
	months						= sign n * (abs n rem 12)
	month`						= mon + n
shiftDate (Days n) date=:{Date | year,mon,day}
| n > 0							= {Date | shiftDate (Months future_months)  date & day=n-last future_days}
| n < 0							= {Date | shiftDate (Months (~past_months)) date & day=(tillBigBangComes date !! past_months)-n}
| otherwise						= date
where
	future_months				= length future_days
	future_days					= takeWhile (\no_of_days = no_of_days <= n) doom_days
	doom_days					= scan (+) 0 (tillDoomsdayComes date)
	past_months					= length past_days
	past_days					= takeWhile (\no_of_days = no_of_days <= abs n) big_bang_days
	big_bang_days				= scan (+) 0 (tillBigBangComes date)

isLeapYear :: Int -> Bool
isLeapYear year
| year rem 4 <> 0				= False
| year rem 100 == 0				= year rem 400 == 0
| otherwise						= True

daysInMonth :: Int Int -> Int
daysInMonth year month
| isMember month [1,3,5,7,8,10,12]	= 31
| month == 2
	| isLeapYear year				= 29
	| otherwise						= 28
| otherwise							= 30

monthDays :: Int -> [Int]
monthDays year						= map (daysInMonth year) [1..12]

tillDoomsdayComes :: Date -> [Int]
tillDoomsdayComes {Date | year,mon,day}
	= remaining_days ++ tillDoomsdayComes {Date | year=year+1,mon=1,day=1}
where
	remaining_days	= case drop (mon-1) (monthDays year) of
						[d : ds] = [d-day+1 : ds]

tillBigBangComes :: Date -> [Int]
tillBigBangComes {Date | year,mon,day}
	= remaining_days ++ tillBigBangComes {Date | year=year-1,mon=12,day=31}
where
	remaining_days	= case reverse (take mon (monthDays year)) of
						[d : ds] = [d-day+1 : ds]

derive class iTask 	DecisionStatus,
					Decision,
					TaxSolarPanelDossier,
					Collection,
					RealEstateOwner,
					CompanyDeclaration,
					TaxCompensationCitizenRequest,
					TaxCompensationDocuments,
					OwnedRealEstate
instance == DecisionStatus          where == a1 a2 = a1 === a2
instance == TaxSolarPanelDossier    where == a1 a2 = a1 === a2
instance == Decision                where == a1 a2 = a1 === a2
instance == Collection              where == a1 a2 = a1 === a2
instance == RealEstateOwner         where == a1 a2 = a1 === a2

instance <  TaxSolarPanelDossier    where <  a1 a2 = a1.TaxSolarPanelDossier.date < a2.TaxSolarPanelDossier.date
instance <  Decision                where <  a1 a2 = a1.Decision.ssn              < a2.Decision.ssn
instance <  Collection              where <  a1 a2 = a1.Collection.date           < a2.Collection.date
instance <  RealEstateOwner         where <  a1 a2 = a1.RealEstateOwner.ownerID   < a2.RealEstateOwner.ownerID
