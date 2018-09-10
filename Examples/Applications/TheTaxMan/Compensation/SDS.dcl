definition module Compensation.SDS

import Compensation.UoD

/** realEstateOwners:
		this shared data source keeps track of the real estate that is owned by a registered *Owner*.
		The *Owner* serves as key.
*/
realEstateOwners :: SDSLens () [RealEstateOwner] [RealEstateOwner]

/** decisions:
		this shared data source keeps track of all decisions regarding tax compensation requests.
*/
decisions :: SDSLens () [Decision] [Decision]

/** collectionPayments:
		this shared data source keeps track of all pending collections that need to be payed.
*/
collectionPayments :: SDSLens () [Collection] [Collection]

/** collectionClaims:
		this shared data source keeps track of all pending collections that need to be claimed at citizens.
*/
collectionClaims :: SDSLens () [Collection] [Collection]

/** collectionsProcessed:
		this shared data source keeps track of all collections that have payed or claimed.
*/
collectionsProcessed :: SDSLens () [Collection] [Collection]


/** acceptedSolarPanels:
		this shared data source keeps track of all types of solar panels for which citizens can enter
		a tax compensation.
*/
acceptedSolarPanels :: SDSLens () [AcceptedSolarPanel] [AcceptedSolarPanel]

/** solarPanelSubsidyRequests:
		this shared data source keeps track of all tax subsidy requests for solar panels.
*/
solarPanelSubsidyRequests :: SDSLens () [TaxSolarPanelDossier] [TaxSolarPanelDossier]

/** currentDecisions ssn pred date:
		this shared data source is a subset of all *decisions* that have been requested by the citizen
		identified by @ssn after date @date, and for which @pred is valid, when applied to the status
		of the decision.
*/
currentDecisions :: SSN (DecisionStatus -> Bool) Date -> SDSLens () [Decision] [Decision]

/** currentPayments ssn date:
		this shared data source is a subset of all *collectionPayments* that have been requested by
		the citizen identified by @ssn after date @date.
*/
currentPayments :: SSN Date -> SDSLens () [Collection] [Collection]

/** currentClaims ssn date:
		this shared data source is a subset of all *collectionClaims* that have been requested by
		the citizen identified by @ssn after date @date.
*/
currentClaims :: SSN Date -> SDSLens () [Collection] [Collection]

/** currentProcessed ssn date:
		this shared date source is a subset of all *collectionsProcessed* that have been requested by
		the citizen identified by @ssn after date @date.
*/
currentProcessed :: SSN Date -> SDSLens () [Collection] [Collection]

/** currentRealEstate citizen:
		this shared data source extracts all registered owned properties of @citizen that is
		registered in *cadastreRealEstate*.
*/
currentRealEstate :: Citizen  -> SDSLens () [OwnedRealEstate] [CadastreRealEstate]
