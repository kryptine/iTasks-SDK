definition module Compensation.Tasks

import iTasks
import Compensation.UoD

/** requestSolarPanelCompensation citizen:
		if @citizen qualifies for requesting tax compensation regarding the placement of solar panels on his home,
		then this request can be placed in this task. The installation company is contacted to provide proof.
*/
requestSolarPanelCompensation :: Citizen -> Task ()

/** editRealEstateOwners:
		this task allows you to view and alter the currently registered *realEstateOwners*.
*/
editRealEstateOwners :: Task ()

/** editDecisions:
		this task allows you to view and alter the currently registered *decisions*.
*/
editDecisions :: Task ()

/** editCollectionPayments:
		this task allows you to view and alter the currently registered *collectionPayments*.
*/
editCollectionPayments :: Task ()

/** editCollectionClaims:
		this task allows you to view and alter the currently registered *collectionClaims*.
*/
editCollectionClaims :: Task ()

/** editProcessedCollections:
		this task allows you to view and alter the currently registered *collectionsProcessed*.
*/
editProcessedCollections :: Task ()

/** editAcceptedSolarPanels:
		this task allows you to view and alter the currently registered *acceptedSolarPanels*.
*/
editAcceptedSolarPanels :: Task ()

/** editSubsidyRequest:
		this task allows you to view and alter the currently registered *solarPanelSubsidyRequests*.
*/
editSubsidyRequest :: Task ()

/** addToCollectionStore collection:
		file @collection to *collectionPayments* if @collection.amount is positive, and 
		file @collection to *collectionClaims* if @collection.amount is negative.
		If @collection.amount is zero, nothing happens.
*/
addToCollectionStore :: !Collection -> Task ()
