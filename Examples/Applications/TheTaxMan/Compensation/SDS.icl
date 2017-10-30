implementation module Compensation.SDS

import Compensation.UoD
import Cadastre.SDS
import Task.Extensions
//import iTasks.API.Extensions.Admin.TonicAdmin
//import iTasks._Framework.Tonic

realEstateOwners		:: Shared [RealEstateOwner]
realEstateOwners		= sharedStore "realEstateOwners" []

decisions 				:: Shared [Decision]
decisions				= sharedStore "decisions" []

collectionsProcessed	:: Shared [Collection]
collectionsProcessed	= sharedStore "collectionsProcessed" []

collectionClaims		:: Shared [Collection]
collectionClaims		= sharedStore "collectionClaims" []

collectionPayments		:: Shared [Collection]
collectionPayments		= sharedStore "collectionPayments" []

acceptedSolarPanels		:: Shared [AcceptedSolarPanel]
acceptedSolarPanels		= sharedStore "acceptedSolarPanels" ["monocrystalline","polycrystalline","amorphous"]

solarPanelSubsidyRequests :: Shared [TaxSolarPanelDossier]
solarPanelSubsidyRequests = sharedStore "SolarPanelSubsidyRequests" []

currentDecisions :: SSN (DecisionStatus -> Bool) Date -> Shared [Decision]
currentDecisions ssn pred date = mapRead (decisionsAfter ssn pred date) decisions

currentPayments :: SSN Date -> Shared [Collection]
currentPayments ssn date = mapReadCollections ssn date collectionPayments

currentClaims :: SSN Date -> Shared [Collection]
currentClaims ssn date = mapReadCollections ssn date collectionClaims

currentProcessed :: SSN Date -> Shared [Collection]
currentProcessed ssn date = mapReadCollections ssn date collectionsProcessed

mapReadCollections :: SSN Date (Shared [Collection]) -> Shared [Collection]
mapReadCollections ssn date collectionStore = mapRead (collectionsAfter ssn date) collectionStore

currentRealEstate :: Citizen -> ReadWriteShared [OwnedRealEstate] [CadastreRealEstate]
currentRealEstate citizen = mapRead (realEstatesOfCitizen citizen) cadastreRealEstate
