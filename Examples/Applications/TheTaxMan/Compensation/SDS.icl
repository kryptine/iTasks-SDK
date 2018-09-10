implementation module Compensation.SDS

import Compensation.UoD
import Cadastre.SDS
import Task.Extensions
//import iTasks.API.Extensions.Admin.TonicAdmin
//import iTasks._Framework.Tonic

realEstateOwners		:: SDSLens () [RealEstateOwner] [RealEstateOwner]
realEstateOwners		= sharedStore "realEstateOwners" []

decisions 				:: SDSLens () [Decision] [Decision]
decisions				= sharedStore "decisions" []

collectionsProcessed	:: SDSLens () [Collection] [Collection]
collectionsProcessed	= sharedStore "collectionsProcessed" []

collectionClaims		:: SDSLens () [Collection] [Collection]
collectionClaims		= sharedStore "collectionClaims" []

collectionPayments		:: SDSLens () [Collection] [Collection]
collectionPayments		= sharedStore "collectionPayments" []

acceptedSolarPanels		:: SDSLens () [AcceptedSolarPanel] [AcceptedSolarPanel]
acceptedSolarPanels		= sharedStore "acceptedSolarPanels" ["monocrystalline","polycrystalline","amorphous"]

solarPanelSubsidyRequests :: SDSLens () [TaxSolarPanelDossier] [TaxSolarPanelDossier]
solarPanelSubsidyRequests = sharedStore "SolarPanelSubsidyRequests" []

currentDecisions :: SSN (DecisionStatus -> Bool) Date -> SDSLens () [Decision] [Decision]
currentDecisions ssn pred date = mapRead (decisionsAfter ssn pred date) decisions

currentPayments :: SSN Date -> SDSLens () [Collection] [Collection]
currentPayments ssn date = mapReadCollections ssn date collectionPayments

currentClaims :: SSN Date -> SDSLens () [Collection] [Collection]
currentClaims ssn date = mapReadCollections ssn date collectionClaims

currentProcessed :: SSN Date -> SDSLens () [Collection] [Collection]
currentProcessed ssn date = mapReadCollections ssn date collectionsProcessed

mapReadCollections :: SSN Date (SDSLens () [Collection] [Collection]) -> SDSLens () [Collection] [Collection]
mapReadCollections ssn date collectionStore = mapRead (collectionsAfter ssn date) collectionStore

currentRealEstate :: Citizen -> SDSLens () [OwnedRealEstate] [CadastreRealEstate]
currentRealEstate citizen = mapRead (realEstatesOfCitizen citizen) cadastreRealEstate
