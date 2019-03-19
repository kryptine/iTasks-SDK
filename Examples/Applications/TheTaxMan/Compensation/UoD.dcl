definition module Compensation.UoD

import Cadastre.UoD
import iTasks.Extensions.DateTime

/** RealEstateOwner:
		keeps track of all property (identified via its *Address*) of a given owner (identified via *Owner*).
*/
:: RealEstateOwner
	=	{ ownerID				:: Owner							/** the owner (serves as key) */
		, addresses				:: [Address]						/** the property owned by *ownerID* */
		}
/** OwnedRealEstate:
		keeps track of whether a property also serves as home address.
*/
:: OwnedRealEstate
	=	{ isHomeAddress			:: Bool								/** property (identified via *postcode* and *houseNumber* is home address */
		, postcode				:: Postcode							/** postcode of real estate */
		, houseNumber			:: Int								/** house number of real estate */
		}
/** Decision:
		keeps track of all information regarding the request for tax compensation.
*/
:: Decision
	=	{ ssn					:: SSN								/** the *SSN* of the *Citizen* who requested the tax compensation */
		, date					:: Date								/** the date when the request was entered */
		, description			:: String							/** the description of the request as entered by the *Citizen* */
		, status				:: DecisionStatus					/** the status of the decision as entered by the tax officer */
		, invoiceAmount			:: Amount							/** the expenses as proven by the *Citizen* and *Company* */
		, compensation			:: Amount							/** the amount of compensation the *Citizen* is entitled to receive */
		}
/** DecisionStatus:
		keeps track of the status of a *Decision*.
*/
:: DecisionStatus
	=	Approved													/** approved without further ado */
	| 	Rejected Reason												/** rejected with given explanation (*Reason*) */
/** Reason:
		a textual explanation in natural language.
*/
:: Reason
	:== String
/** Collection:
		keeps track of a tax collection.
*/
:: Collection
	=	{ ssn					:: SSN								/** the *SSN* of the *Citizen* to whom the collection is directed */
		, description			:: String							/** an explanation of the collection */
		, date					:: Date								/** the date of issuing the collection */
		, amount				:: Amount							/** the amount of money to be payed (positive) or claimed (negative) from the *Citizen* */
		}
/** TaxSolarPanelDossier:
		keeps track of a *Citizen* request for tax compensation regarding the installation of solar panels.
*/
:: TaxSolarPanelDossier
	=	{ request				:: TaxCompensationCitizenRequest	/** the request of the *Citizen* */
		, declarationCompany	:: CompanyDeclaration				/** the proof of the *Company* */
		, date					:: Date								/** the date of issueing the request */
		}
/** TaxCompensationCitizenRequest:
		keeps track of all data provided by the *Citizen* when filing a request for solar panel tax compensation.
*/
:: TaxCompensationCitizenRequest
	=	{ ssn					:: SSN								/** the *SSN* of the *Citizen* who filed the request */
		, applicant				:: NameHomeAddress					/** the name and address of the *Citizen* */
		, documents				:: TaxCompensationDocuments			/** the proof provided by the *Citizen* */
		, company				:: Company							/** the *Company* that installed the solar panels */
		}
/** TaxCompensationDocuments:
		keeps track of the proof provided by the *Citizen* when filing a request for solar panel tax compensation.
*/
:: TaxCompensationDocuments
	=	{ invoiceAmount			:: Amount							/** the amount of money payed to the company */
		, invoiceDate			:: Date								/** the company invoice date */
		, invoiceProof			:: Document							/** the company invoice */
		, proofOfPayment		:: Document							/** the proof of payment to the company */
		, roofPhotos			:: [Photo]							/** photos of the roof */
		}
/** CompanyDeclaration:
		keeps track of the proof provided by the *Company* that installed the solar panels.
*/
:: CompanyDeclaration
	=	{ solarPanelType		:: AcceptedSolarPanel				/** the type of solar panels that have been used */
		, roofPhotos			:: [Photo]							/** photos of the roof */
		, roofAreaCovered		:: RoofAreaCovered					/** the area of the roof that is newly covered (rounded up) */
		, date					:: Date								/** the date of providing this proof */
		}
/** AcceptedSolarPanels:
		the officially accepted solar panel types for which tax compensation is valid.
*/
:: AcceptedSolarPanels
	= SolarPanels [AcceptedSolarPanel]
:: AcceptedSolarPanel
	:== String
/** RoofAreaCovered:
		the area of the roof that is covered (in square metres, rounded up).
*/
:: RoofAreaCovered
	:== Int

/**	decisionsAfter ssn cond date all_decisions = decisions:
		@decisions is the subset of @all_decisions of a citizen with SSN @ssn after given @date for which @cond is valid.
*/
decisionsAfter			:: SSN (DecisionStatus -> Bool) Date [Decision] -> [Decision]

/**	collectionsAfter ssn date all_collections = collections:
		@collections is the subset of @all_collections of a citizen with SSN @ssn after given @date.
*/
collectionsAfter		:: SSN Date [Collection] -> [Collection]

/** realEstatesOfCitizen citizen cadastre = information:
		@information is the list of all owned real estate by @citizen.
		Of each property p in @information, p.isHomeAddress is True only if @citizen lives at that address.
*/
realEstatesOfCitizen	:: Citizen [CadastreRealEstate] -> [OwnedRealEstate]

/**	clientReminderDate date:
		returns the date at which a reminder should be sent to a client for filling in documents.
*/
clientReminderDate :: Date -> Date

/** clientDeadlineDate date:
		returns the deadline of filling in documents for clients.
*/
clientDeadlineDate :: Date -> Date

/** TimePeriod:
		is an indication of a time period. The values are allowed to be negative.
*/
::	TimePeriod
	= Years  Int	/** number of years  ahead (positive) or in the past (negative) */
	| Months Int	/** number of months ahead (positive) or in the past (negative)  */
	| Days   Int	/** number of days   ahead (positive) or in the past (negative) */

/**	shiftDate period date:
		moves @date ahead in time if the value in @period is positive, and backwards in time if the value in @period is negative.
		In case @period concerns months, the day in @date is matched with the number of days in the new month (for instance,
		shiftDate (Months 1) {Date | year=2000,mon=1,day=31} = {Date | year=2000,mon=2,day=29}).
*/
shiftDate :: TimePeriod Date -> Date

derive class iTask 	DecisionStatus,
					Decision,
					TaxSolarPanelDossier,
					Collection,
					RealEstateOwner,
					CompanyDeclaration,
					TaxCompensationCitizenRequest,
					TaxCompensationDocuments,
					OwnedRealEstate
instance ==		 	DecisionStatus,
					Decision,
					TaxSolarPanelDossier,
					Collection,
					RealEstateOwner
instance <		 	TaxSolarPanelDossier,
					Decision,
					Collection,
					RealEstateOwner
