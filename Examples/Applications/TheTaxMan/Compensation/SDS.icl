implementation module Compensation.SDS

import Compensation.UoD
import Cadastre.SDS
import Task.Extensions
//import iTasks.API.Extensions.Admin.TonicAdmin
//import iTasks._Framework.Tonic

realEstateOwners		:: SimpleSDSLens [RealEstateOwner]
realEstateOwners		= sharedStore "realEstateOwners" []

decisions 				:: SimpleSDSLens [Decision]
decisions				= sharedStore "decisions" []

collectionsProcessed	:: SimpleSDSLens [Collection]
collectionsProcessed	= sharedStore "collectionsProcessed" []

collectionClaims		:: SimpleSDSLens [Collection]
collectionClaims		= sharedStore "collectionClaims" []

collectionPayments		:: SimpleSDSLens [Collection]
collectionPayments		= sharedStore "collectionPayments" []

acceptedSolarPanels		:: SimpleSDSLens [AcceptedSolarPanel]
acceptedSolarPanels		= sharedStore "acceptedSolarPanels" ["monocrystalline","polycrystalline","amorphous"]

solarPanelSubsidyRequests :: SimpleSDSLens [TaxSolarPanelDossier]
solarPanelSubsidyRequests = sharedStore "SolarPanelSubsidyRequests" []

currentDecisions :: SSN (DecisionStatus -> Bool) Date -> SimpleSDSLens [Decision]
currentDecisions ssn pred date = mapRead (decisionsAfter ssn pred date) decisions

currentPayments :: SSN Date -> SimpleSDSLens [Collection]
currentPayments ssn date = mapReadCollections ssn date collectionPayments

currentClaims :: SSN Date -> SimpleSDSLens [Collection]
currentClaims ssn date = mapReadCollections ssn date collectionClaims

currentProcessed :: SSN Date -> SimpleSDSLens [Collection]
currentProcessed ssn date = mapReadCollections ssn date collectionsProcessed

mapReadCollections :: SSN Date (SimpleSDSLens [Collection]) -> SimpleSDSLens [Collection]
mapReadCollections ssn date collectionStore = mapRead (collectionsAfter ssn date) collectionStore

currentRealEstate :: Citizen -> SDSLens () [OwnedRealEstate] [CadastreRealEstate]
currentRealEstate citizen = mapRead (realEstatesOfCitizen citizen) cadastreRealEstate
