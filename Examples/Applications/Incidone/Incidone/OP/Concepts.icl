implementation module Incidone.OP.Concepts

import iTasks, Database.SQL, Incidone.OP.ConceptsTOP, Incidone.OP.ConceptsSQL, Incidone.ContactPosition, Incidone.Util.AIS

instance mbToSQL Int
where
	mbToSQL (Just x) = [SQLVInteger x]
	mbToSQL Nothing  = [SQLVNull]
instance mbToSQL String
where
	mbToSQL (Just x) = [SQLVVarchar x]
	mbToSQL Nothing  = [SQLVNull]
instance mbToSQL Bool
where
	mbToSQL (Just x) = [SQLVInteger (if x 1 0)]
	mbToSQL Nothing = [SQLVNull]
instance mbFromSQL Int
where
	mbFromSQL [SQLVInteger x] = Just x
	mbFromSQL _               = Nothing
instance mbFromSQL String
where
	mbFromSQL [SQLVChar x]    = Just x
	mbFromSQL [SQLVVarchar x] = Just x
	mbFromSQL [SQLVText x]    = Just x
	mbFromSQL _               = Nothing
instance mbFromSQL Bool
where
	mbFromSQL [SQLVInteger x] = Just (x <> 0)
	mbFromSQL _               = Nothing

instance mbToSQL VesselType where
	mbToSQL :: !(Maybe VesselType) -> [SQLValue]
	mbToSQL (Just Yacht) = [SQLVEnum "Yacht"]
	mbToSQL (Just FishingVessel) = [SQLVEnum "Fishing vessel"]
	mbToSQL (Just CargoVessel) = [SQLVEnum "Cargo vessel"]
	mbToSQL (Just TowingVessel) = [SQLVEnum "Towing vessel"]
	mbToSQL (Just PatrolVessel) = [SQLVEnum "Patrol vessel"]
	mbToSQL (Just RescueVessel) = [SQLVEnum "Rescue vessel"]
	mbToSQL (Just OtherVessel) = [SQLVEnum "Other"]
	mbToSQL Nothing = [SQLVNull]

instance mbFromSQL VesselType where
	mbFromSQL :: ![SQLValue] -> Maybe VesselType
	mbFromSQL [SQLVEnum "Yacht"] = Just Yacht
	mbFromSQL [SQLVText "Yacht"] = Just Yacht
	mbFromSQL [SQLVEnum "Fishing vessel"] = Just FishingVessel
	mbFromSQL [SQLVText "Fishing vessel"] = Just FishingVessel
	mbFromSQL [SQLVEnum "Cargo vessel"] = Just CargoVessel
	mbFromSQL [SQLVText "Cargo vessel"] = Just CargoVessel
	mbFromSQL [SQLVEnum "Towing vessel"] = Just TowingVessel
	mbFromSQL [SQLVText "Towing vessel"] = Just TowingVessel
	mbFromSQL [SQLVEnum "Patrol vessel"] = Just PatrolVessel
	mbFromSQL [SQLVText "Patrol vessel"] = Just PatrolVessel
	mbFromSQL [SQLVEnum "Rescue vessel"] = Just RescueVessel
	mbFromSQL [SQLVText "Rescue vessel"] = Just RescueVessel
	mbFromSQL [SQLVEnum "Other"] = Just OtherVessel
	mbFromSQL [SQLVText "Other"] = Just OtherVessel
	mbFromSQL _ = Nothing

instance mbToSQL Gender where
	mbToSQL :: !(Maybe Gender) -> [SQLValue]
	mbToSQL (Just Male) = [SQLVEnum "Male"]
	mbToSQL (Just Female) = [SQLVEnum "Female"]
	mbToSQL Nothing = [SQLVNull]

instance mbFromSQL Gender where
	mbFromSQL :: ![SQLValue] -> Maybe Gender
	mbFromSQL [SQLVEnum "Male"] = Just Male
	mbFromSQL [SQLVText "Male"] = Just Male
	mbFromSQL [SQLVEnum "Female"] = Just Female
	mbFromSQL [SQLVText "Female"] = Just Female
	mbFromSQL _ = Nothing

instance mbToSQL CommunicationMeanType where
	mbToSQL :: !(Maybe CommunicationMeanType) -> [SQLValue]
	mbToSQL (Just CMPhone) = [SQLVEnum "Telephone"]
	mbToSQL (Just CMVHF) = [SQLVEnum "VHF"]
	mbToSQL (Just CMEmail) = [SQLVEnum "E-mail"]
	mbToSQL (Just CMP2000) = [SQLVEnum "P2000"]
	mbToSQL Nothing = [SQLVNull]

instance mbFromSQL CommunicationMeanType where
	mbFromSQL :: ![SQLValue] -> Maybe CommunicationMeanType
	mbFromSQL [SQLVEnum "Telephone"] = Just CMPhone
	mbFromSQL [SQLVText "Telephone"] = Just CMPhone
	mbFromSQL [SQLVEnum "VHF"] = Just CMVHF
	mbFromSQL [SQLVText "VHF"] = Just CMVHF
	mbFromSQL [SQLVEnum "E-mail"] = Just CMEmail
	mbFromSQL [SQLVText "E-mail"] = Just CMEmail
	mbFromSQL [SQLVEnum "P2000"] = Just CMP2000
	mbFromSQL [SQLVText "P2000"] = Just CMP2000
	mbFromSQL _ = Nothing

instance mbToSQL ContactAccessLevel where
	mbToSQL :: !(Maybe ContactAccessLevel) -> [SQLValue]
	mbToSQL (Just WOAccess) = [SQLVEnum "Watch officer"]
	mbToSQL (Just PartnerAccess) = [SQLVEnum "Partner"]
	mbToSQL Nothing = [SQLVNull]

instance mbFromSQL ContactAccessLevel where
	mbFromSQL :: ![SQLValue] -> Maybe ContactAccessLevel
	mbFromSQL [SQLVEnum "Watch officer"] = Just WOAccess
	mbFromSQL [SQLVText "Watch officer"] = Just WOAccess
	mbFromSQL [SQLVEnum "Partner"] = Just PartnerAccess
	mbFromSQL [SQLVText "Partner"] = Just PartnerAccess
	mbFromSQL _ = Nothing

instance mbToSQL ContactStatus where
	mbToSQL :: !(Maybe ContactStatus) -> [SQLValue]
	mbToSQL (Just Alerted) = [SQLVEnum "Alerted"]
	mbToSQL (Just Briefed) = [SQLVEnum "Briefed"]
	mbToSQL (Just Proceeding) = [SQLVEnum "Proceeding"]
	mbToSQL (Just OnScene) = [SQLVEnum "On Scene"]
	mbToSQL (Just StandbyOnLocation) = [SQLVEnum "Standby On Location"]
	mbToSQL (Just OnTask) = [SQLVEnum "On Task"]
	mbToSQL (Just StandBy) = [SQLVEnum "Standby"]
	mbToSQL (Just Transfer) = [SQLVEnum "Transfer"]
	mbToSQL (Just Released) = [SQLVEnum "Released"]
	mbToSQL (Just Returned) = [SQLVEnum "Returned"]
	mbToSQL (Just LimitedAvailability) = [SQLVEnum "Limited Availability"]
	mbToSQL (Just UnAvailable) = [SQLVEnum "Unavailable"]
	mbToSQL (Just Training) = [SQLVEnum "Training"]
	mbToSQL (Just PreAlert) = [SQLVEnum "Pre-Alert"]
	mbToSQL Nothing = [SQLVNull]

instance mbFromSQL ContactStatus where
	mbFromSQL :: ![SQLValue] -> Maybe ContactStatus
	mbFromSQL [SQLVEnum "Alerted"] = Just Alerted
	mbFromSQL [SQLVText "Alerted"] = Just Alerted
	mbFromSQL [SQLVEnum "Briefed"] = Just Briefed
	mbFromSQL [SQLVText "Briefed"] = Just Briefed
	mbFromSQL [SQLVEnum "Proceeding"] = Just Proceeding
	mbFromSQL [SQLVText "Proceeding"] = Just Proceeding
	mbFromSQL [SQLVEnum "On Scene"] = Just OnScene
	mbFromSQL [SQLVText "On Scene"] = Just OnScene
	mbFromSQL [SQLVEnum "Standby On Location"] = Just StandbyOnLocation
	mbFromSQL [SQLVText "Standby On Location"] = Just StandbyOnLocation
	mbFromSQL [SQLVEnum "On Task"] = Just OnTask
	mbFromSQL [SQLVText "On Task"] = Just OnTask
	mbFromSQL [SQLVEnum "Standby"] = Just StandBy
	mbFromSQL [SQLVText "Standby"] = Just StandBy
	mbFromSQL [SQLVEnum "Transfer"] = Just Transfer
	mbFromSQL [SQLVText "Transfer"] = Just Transfer
	mbFromSQL [SQLVEnum "Released"] = Just Released
	mbFromSQL [SQLVText "Released"] = Just Released
	mbFromSQL [SQLVEnum "Returned"] = Just Returned
	mbFromSQL [SQLVText "Returned"] = Just Returned
	mbFromSQL [SQLVEnum "Limited Availability"] = Just LimitedAvailability
	mbFromSQL [SQLVText "Limited Availability"] = Just LimitedAvailability
	mbFromSQL [SQLVEnum "Unavailable"] = Just UnAvailable
	mbFromSQL [SQLVText "Unavailable"] = Just UnAvailable
	mbFromSQL [SQLVEnum "Training"] = Just Training
	mbFromSQL [SQLVText "Training"] = Just Training
	mbFromSQL [SQLVEnum "Pre-Alert"] = Just PreAlert
	mbFromSQL [SQLVText "Pre-Alert"] = Just PreAlert
	mbFromSQL _ = Nothing

instance mbToSQL ContactType where
	mbToSQL :: !(Maybe ContactType) -> [SQLValue]
	mbToSQL (Just Person) = [SQLVEnum "Person"]
	mbToSQL (Just Vessel) = [SQLVEnum "Vessel"]
	mbToSQL (Just Surfer) = [SQLVEnum "Surfer"]
	mbToSQL (Just Diver) = [SQLVEnum "Diver"]
	mbToSQL (Just Airplane) = [SQLVEnum "Airplane"]
	mbToSQL (Just Helicopter) = [SQLVEnum "Helicopter"]
	mbToSQL (Just OtherContact) = [SQLVEnum "Other"]
	mbToSQL Nothing = [SQLVNull]

instance mbFromSQL ContactType where
	mbFromSQL :: ![SQLValue] -> Maybe ContactType
	mbFromSQL [SQLVEnum "Person"] = Just Person
	mbFromSQL [SQLVText "Person"] = Just Person
	mbFromSQL [SQLVEnum "Vessel"] = Just Vessel
	mbFromSQL [SQLVText "Vessel"] = Just Vessel
	mbFromSQL [SQLVEnum "Surfer"] = Just Surfer
	mbFromSQL [SQLVText "Surfer"] = Just Surfer
	mbFromSQL [SQLVEnum "Diver"] = Just Diver
	mbFromSQL [SQLVText "Diver"] = Just Diver
	mbFromSQL [SQLVEnum "Airplane"] = Just Airplane
	mbFromSQL [SQLVText "Airplane"] = Just Airplane
	mbFromSQL [SQLVEnum "Helicopter"] = Just Helicopter
	mbFromSQL [SQLVText "Helicopter"] = Just Helicopter
	mbFromSQL [SQLVEnum "Other"] = Just OtherContact
	mbFromSQL [SQLVText "Other"] = Just OtherContact
	mbFromSQL _ = Nothing

instance mbToSQL WeatherType where
	mbToSQL :: !(Maybe WeatherType) -> [SQLValue]
	mbToSQL (Just Rain) = [SQLVEnum "Rain"]
	mbToSQL (Just Drizzle) = [SQLVEnum "Drizzle"]
	mbToSQL (Just Mist) = [SQLVEnum "Mist"]
	mbToSQL (Just Fog) = [SQLVEnum "Fog"]
	mbToSQL (Just Snow) = [SQLVEnum "Snow"]
	mbToSQL (Just Hail) = [SQLVEnum "Hail"]
	mbToSQL (Just Sunny) = [SQLVEnum "Sunny"]
	mbToSQL (Just Haze) = [SQLVEnum "Haze"]
	mbToSQL (Just Cloudy) = [SQLVEnum "Cloudy"]
	mbToSQL (Just Showers) = [SQLVEnum "Showers"]
	mbToSQL (Just Thunderstorms) = [SQLVEnum "Thunderstorms"]
	mbToSQL Nothing = [SQLVNull]

instance mbFromSQL WeatherType where
	mbFromSQL :: ![SQLValue] -> Maybe WeatherType
	mbFromSQL [SQLVEnum "Rain"] = Just Rain
	mbFromSQL [SQLVText "Rain"] = Just Rain
	mbFromSQL [SQLVEnum "Drizzle"] = Just Drizzle
	mbFromSQL [SQLVText "Drizzle"] = Just Drizzle
	mbFromSQL [SQLVEnum "Mist"] = Just Mist
	mbFromSQL [SQLVText "Mist"] = Just Mist
	mbFromSQL [SQLVEnum "Fog"] = Just Fog
	mbFromSQL [SQLVText "Fog"] = Just Fog
	mbFromSQL [SQLVEnum "Snow"] = Just Snow
	mbFromSQL [SQLVText "Snow"] = Just Snow
	mbFromSQL [SQLVEnum "Hail"] = Just Hail
	mbFromSQL [SQLVText "Hail"] = Just Hail
	mbFromSQL [SQLVEnum "Sunny"] = Just Sunny
	mbFromSQL [SQLVText "Sunny"] = Just Sunny
	mbFromSQL [SQLVEnum "Haze"] = Just Haze
	mbFromSQL [SQLVText "Haze"] = Just Haze
	mbFromSQL [SQLVEnum "Cloudy"] = Just Cloudy
	mbFromSQL [SQLVText "Cloudy"] = Just Cloudy
	mbFromSQL [SQLVEnum "Showers"] = Just Showers
	mbFromSQL [SQLVText "Showers"] = Just Showers
	mbFromSQL [SQLVEnum "Thunderstorms"] = Just Thunderstorms
	mbFromSQL [SQLVText "Thunderstorms"] = Just Thunderstorms
	mbFromSQL _ = Nothing

instance mbToSQL IncidentType where
	mbToSQL :: !(Maybe IncidentType) -> [SQLValue]
	mbToSQL (Just AircraftAppearsInDifficulties) = [SQLVEnum "Aircraft appears in difficulties"]
	mbToSQL (Just AircraftCollision) = [SQLVEnum "Aircraft collision"]
	mbToSQL (Just AircraftCrash) = [SQLVEnum "Aircraft crash"]
	mbToSQL (Just AircraftDitch) = [SQLVEnum "Aircraft ditch"]
	mbToSQL (Just AircraftEmergency) = [SQLVEnum "Aircraft emergency"]
	mbToSQL (Just AircraftOverdue) = [SQLVEnum "Aircraft overdue"]
	mbToSQL (Just DiverInProblems) = [SQLVEnum "Diver in problems"]
	mbToSQL (Just DiverMissing) = [SQLVEnum "Diver missing"]
	mbToSQL (Just DSCAlertInsideNLSRR) = [SQLVEnum "DSC alert in NLSSR"]
	mbToSQL (Just DSCAlertOutsideNLSRR) = [SQLVEnum "DSC alert outside NLSSR"]
	mbToSQL (Just FalseAlertGoodIntent) = [SQLVEnum "False alert good intent"]
	mbToSQL (Just FalseAlertMaliciousIntent) = [SQLVEnum "False alert malicious intent"]
	mbToSQL (Just FamilyMessage) = [SQLVEnum "Family message"]
	mbToSQL (Just FlareSighted) = [SQLVEnum "Flare sighted"]
	mbToSQL (Just INMARSATAlertInsideNLSRR) = [SQLVEnum "INMARSAT alert in NLSRR"]
	mbToSQL (Just INMARSATAlertOutsideNLSRR) = [SQLVEnum "INMARSAT alert outside NLSRR"]
	mbToSQL (Just Medevac) = [SQLVEnum "Medevac"]
	mbToSQL (Just MedicalAdvice) = [SQLVEnum "Medical advice"]
	mbToSQL (Just NRBTest) = [SQLVEnum "NRB test"]
	mbToSQL (Just PersonAppearsInDifficulties) = [SQLVEnum "Person appears in difficulties"]
	mbToSQL (Just PersonBodyRecovery) = [SQLVEnum "Person body recovery"]
	mbToSQL (Just PersonsInProblems) = [SQLVEnum "Person in problems"]
	mbToSQL (Just PersonsMissing) = [SQLVEnum "Person missing"]
	mbToSQL (Just RigIncident) = [SQLVEnum "Rig incident"]
	mbToSQL (Just RigManOverboard) = [SQLVEnum "Rig man overboard"]
	mbToSQL (Just SARHeliUnavailable) = [SQLVEnum "SAR HELI unavailable"]
	mbToSQL (Just SARTAlert) = [SQLVEnum "SART alert"]
	mbToSQL (Just SurferAppearsInDifficulties) = [SQLVEnum "Surfer appears in difficulties"]
	mbToSQL (Just SurferInProblems) = [SQLVEnum "Surfer in problems"]
	mbToSQL (Just SurferMissing) = [SQLVEnum "Surfer missing"]
	mbToSQL (Just VesselAppearsInDifficulties) = [SQLVEnum "Vessel appears in difficulties"]
	mbToSQL (Just VesselCapsized) = [SQLVEnum "Vessel capsized"]
	mbToSQL (Just VesselCollision) = [SQLVEnum "Vessel collision"]
	mbToSQL (Just VesselEngineOrSteeringProblems) = [SQLVEnum "Vessel engine or steering problems"]
	mbToSQL (Just VesselFireOrExplosion) = [SQLVEnum "Vessel fire or explosion"]
	mbToSQL (Just VesselManOverboard) = [SQLVEnum "Vessel man overboard"]
	mbToSQL (Just VesselOverdue) = [SQLVEnum "Vessel overdue"]
	mbToSQL (Just VesselPiracyOrHijack) = [SQLVEnum "Vessel piracy or hijack"]
	mbToSQL (Just VesselSunk) = [SQLVEnum "Vessel sunk"]
	mbToSQL (Just VesselTakingWater) = [SQLVEnum "Vessel taking water"]
	mbToSQL (Just VesselUnsureOfPosition) = [SQLVEnum "Vessel unsure of position"]
	mbToSQL (Just YachtAground) = [SQLVEnum "Yacht aground"]
	mbToSQL (Just YachtAppearsInDiffulties) = [SQLVEnum "Yacht appears in difficulties"]
	mbToSQL (Just YachtCapsized) = [SQLVEnum "Yacht capsized"]
	mbToSQL (Just YachtCollision) = [SQLVEnum "Yacht collision"]
	mbToSQL (Just YachtEngineProblems) = [SQLVEnum "Yacht engine problems"]
	mbToSQL (Just YachtFireOrExplosion) = [SQLVEnum "Yacht fire or explosion"]
	mbToSQL (Just YachtGearFouled) = [SQLVEnum "Yacht gear fouled"]
	mbToSQL (Just YachtManOverboard) = [SQLVEnum "Yacht man overboard"]
	mbToSQL Nothing = [SQLVNull]

instance mbFromSQL IncidentType where
	mbFromSQL :: ![SQLValue] -> Maybe IncidentType
	mbFromSQL [SQLVEnum "Aircraft appears in difficulties"] = Just AircraftAppearsInDifficulties
	mbFromSQL [SQLVText "Aircraft appears in difficulties"] = Just AircraftAppearsInDifficulties
	mbFromSQL [SQLVEnum "Aircraft collision"] = Just AircraftCollision
	mbFromSQL [SQLVText "Aircraft collision"] = Just AircraftCollision
	mbFromSQL [SQLVEnum "Aircraft crash"] = Just AircraftCrash
	mbFromSQL [SQLVText "Aircraft crash"] = Just AircraftCrash
	mbFromSQL [SQLVEnum "Aircraft ditch"] = Just AircraftDitch
	mbFromSQL [SQLVText "Aircraft ditch"] = Just AircraftDitch
	mbFromSQL [SQLVEnum "Aircraft emergency"] = Just AircraftEmergency
	mbFromSQL [SQLVText "Aircraft emergency"] = Just AircraftEmergency
	mbFromSQL [SQLVEnum "Aircraft overdue"] = Just AircraftOverdue
	mbFromSQL [SQLVText "Aircraft overdue"] = Just AircraftOverdue
	mbFromSQL [SQLVEnum "Diver in problems"] = Just DiverInProblems
	mbFromSQL [SQLVText "Diver in problems"] = Just DiverInProblems
	mbFromSQL [SQLVEnum "Diver missing"] = Just DiverMissing
	mbFromSQL [SQLVText "Diver missing"] = Just DiverMissing
	mbFromSQL [SQLVEnum "DSC alert in NLSSR"] = Just DSCAlertInsideNLSRR
	mbFromSQL [SQLVText "DSC alert in NLSSR"] = Just DSCAlertInsideNLSRR
	mbFromSQL [SQLVEnum "DSC alert outside NLSSR"] = Just DSCAlertOutsideNLSRR
	mbFromSQL [SQLVText "DSC alert outside NLSSR"] = Just DSCAlertOutsideNLSRR
	mbFromSQL [SQLVEnum "False alert good intent"] = Just FalseAlertGoodIntent
	mbFromSQL [SQLVText "False alert good intent"] = Just FalseAlertGoodIntent
	mbFromSQL [SQLVEnum "False alert malicious intent"] = Just FalseAlertMaliciousIntent
	mbFromSQL [SQLVText "False alert malicious intent"] = Just FalseAlertMaliciousIntent
	mbFromSQL [SQLVEnum "Family message"] = Just FamilyMessage
	mbFromSQL [SQLVText "Family message"] = Just FamilyMessage
	mbFromSQL [SQLVEnum "Flare sighted"] = Just FlareSighted
	mbFromSQL [SQLVText "Flare sighted"] = Just FlareSighted
	mbFromSQL [SQLVEnum "INMARSAT alert in NLSRR"] = Just INMARSATAlertInsideNLSRR
	mbFromSQL [SQLVText "INMARSAT alert in NLSRR"] = Just INMARSATAlertInsideNLSRR
	mbFromSQL [SQLVEnum "INMARSAT alert outside NLSRR"] = Just INMARSATAlertOutsideNLSRR
	mbFromSQL [SQLVText "INMARSAT alert outside NLSRR"] = Just INMARSATAlertOutsideNLSRR
	mbFromSQL [SQLVEnum "Medevac"] = Just Medevac
	mbFromSQL [SQLVText "Medevac"] = Just Medevac
	mbFromSQL [SQLVEnum "Medical advice"] = Just MedicalAdvice
	mbFromSQL [SQLVText "Medical advice"] = Just MedicalAdvice
	mbFromSQL [SQLVEnum "NRB test"] = Just NRBTest
	mbFromSQL [SQLVText "NRB test"] = Just NRBTest
	mbFromSQL [SQLVEnum "Person appears in difficulties"] = Just PersonAppearsInDifficulties
	mbFromSQL [SQLVText "Person appears in difficulties"] = Just PersonAppearsInDifficulties
	mbFromSQL [SQLVEnum "Person body recovery"] = Just PersonBodyRecovery
	mbFromSQL [SQLVText "Person body recovery"] = Just PersonBodyRecovery
	mbFromSQL [SQLVEnum "Person in problems"] = Just PersonsInProblems
	mbFromSQL [SQLVText "Person in problems"] = Just PersonsInProblems
	mbFromSQL [SQLVEnum "Person missing"] = Just PersonsMissing
	mbFromSQL [SQLVText "Person missing"] = Just PersonsMissing
	mbFromSQL [SQLVEnum "Rig incident"] = Just RigIncident
	mbFromSQL [SQLVText "Rig incident"] = Just RigIncident
	mbFromSQL [SQLVEnum "Rig man overboard"] = Just RigManOverboard
	mbFromSQL [SQLVText "Rig man overboard"] = Just RigManOverboard
	mbFromSQL [SQLVEnum "SAR HELI unavailable"] = Just SARHeliUnavailable
	mbFromSQL [SQLVText "SAR HELI unavailable"] = Just SARHeliUnavailable
	mbFromSQL [SQLVEnum "SART alert"] = Just SARTAlert
	mbFromSQL [SQLVText "SART alert"] = Just SARTAlert
	mbFromSQL [SQLVEnum "Surfer appears in difficulties"] = Just SurferAppearsInDifficulties
	mbFromSQL [SQLVText "Surfer appears in difficulties"] = Just SurferAppearsInDifficulties
	mbFromSQL [SQLVEnum "Surfer in problems"] = Just SurferInProblems
	mbFromSQL [SQLVText "Surfer in problems"] = Just SurferInProblems
	mbFromSQL [SQLVEnum "Surfer missing"] = Just SurferMissing
	mbFromSQL [SQLVText "Surfer missing"] = Just SurferMissing
	mbFromSQL [SQLVEnum "Vessel appears in difficulties"] = Just VesselAppearsInDifficulties
	mbFromSQL [SQLVText "Vessel appears in difficulties"] = Just VesselAppearsInDifficulties
	mbFromSQL [SQLVEnum "Vessel capsized"] = Just VesselCapsized
	mbFromSQL [SQLVText "Vessel capsized"] = Just VesselCapsized
	mbFromSQL [SQLVEnum "Vessel collision"] = Just VesselCollision
	mbFromSQL [SQLVText "Vessel collision"] = Just VesselCollision
	mbFromSQL [SQLVEnum "Vessel engine or steering problems"] = Just VesselEngineOrSteeringProblems
	mbFromSQL [SQLVText "Vessel engine or steering problems"] = Just VesselEngineOrSteeringProblems
	mbFromSQL [SQLVEnum "Vessel fire or explosion"] = Just VesselFireOrExplosion
	mbFromSQL [SQLVText "Vessel fire or explosion"] = Just VesselFireOrExplosion
	mbFromSQL [SQLVEnum "Vessel man overboard"] = Just VesselManOverboard
	mbFromSQL [SQLVText "Vessel man overboard"] = Just VesselManOverboard
	mbFromSQL [SQLVEnum "Vessel overdue"] = Just VesselOverdue
	mbFromSQL [SQLVText "Vessel overdue"] = Just VesselOverdue
	mbFromSQL [SQLVEnum "Vessel piracy or hijack"] = Just VesselPiracyOrHijack
	mbFromSQL [SQLVText "Vessel piracy or hijack"] = Just VesselPiracyOrHijack
	mbFromSQL [SQLVEnum "Vessel sunk"] = Just VesselSunk
	mbFromSQL [SQLVText "Vessel sunk"] = Just VesselSunk
	mbFromSQL [SQLVEnum "Vessel taking water"] = Just VesselTakingWater
	mbFromSQL [SQLVText "Vessel taking water"] = Just VesselTakingWater
	mbFromSQL [SQLVEnum "Vessel unsure of position"] = Just VesselUnsureOfPosition
	mbFromSQL [SQLVText "Vessel unsure of position"] = Just VesselUnsureOfPosition
	mbFromSQL [SQLVEnum "Yacht aground"] = Just YachtAground
	mbFromSQL [SQLVText "Yacht aground"] = Just YachtAground
	mbFromSQL [SQLVEnum "Yacht appears in difficulties"] = Just YachtAppearsInDiffulties
	mbFromSQL [SQLVText "Yacht appears in difficulties"] = Just YachtAppearsInDiffulties
	mbFromSQL [SQLVEnum "Yacht capsized"] = Just YachtCapsized
	mbFromSQL [SQLVText "Yacht capsized"] = Just YachtCapsized
	mbFromSQL [SQLVEnum "Yacht collision"] = Just YachtCollision
	mbFromSQL [SQLVText "Yacht collision"] = Just YachtCollision
	mbFromSQL [SQLVEnum "Yacht engine problems"] = Just YachtEngineProblems
	mbFromSQL [SQLVText "Yacht engine problems"] = Just YachtEngineProblems
	mbFromSQL [SQLVEnum "Yacht fire or explosion"] = Just YachtFireOrExplosion
	mbFromSQL [SQLVText "Yacht fire or explosion"] = Just YachtFireOrExplosion
	mbFromSQL [SQLVEnum "Yacht gear fouled"] = Just YachtGearFouled
	mbFromSQL [SQLVText "Yacht gear fouled"] = Just YachtGearFouled
	mbFromSQL [SQLVEnum "Yacht man overboard"] = Just YachtManOverboard
	mbFromSQL [SQLVText "Yacht man overboard"] = Just YachtManOverboard
	mbFromSQL _ = Nothing

instance mbToSQL EmergencyPhase where
	mbToSQL :: !(Maybe EmergencyPhase) -> [SQLValue]
	mbToSQL (Just INCERFA) = [SQLVEnum "INCERFA"]
	mbToSQL (Just ALERFA) = [SQLVEnum "ALERFA"]
	mbToSQL (Just DETRESFA) = [SQLVEnum "DETRESFA"]
	mbToSQL Nothing = [SQLVNull]

instance mbFromSQL EmergencyPhase where
	mbFromSQL :: ![SQLValue] -> Maybe EmergencyPhase
	mbFromSQL [SQLVEnum "INCERFA"] = Just INCERFA
	mbFromSQL [SQLVText "INCERFA"] = Just INCERFA
	mbFromSQL [SQLVEnum "ALERFA"] = Just ALERFA
	mbFromSQL [SQLVText "ALERFA"] = Just ALERFA
	mbFromSQL [SQLVEnum "DETRESFA"] = Just DETRESFA
	mbFromSQL [SQLVText "DETRESFA"] = Just DETRESFA
	mbFromSQL _ = Nothing

instance mbToSQL DSCCategory where
	mbToSQL :: !(Maybe DSCCategory) -> [SQLValue]
	mbToSQL (Just DSCSafety) = [SQLVEnum "Safety"]
	mbToSQL (Just DSCRoutine) = [SQLVEnum "Routine"]
	mbToSQL (Just DSCUrgency) = [SQLVEnum "Urgency"]
	mbToSQL (Just DSCDistress) = [SQLVEnum "Distress"]
	mbToSQL (Just DSCInvalid) = [SQLVEnum "Invalid"]
	mbToSQL Nothing = [SQLVNull]

instance mbFromSQL DSCCategory where
	mbFromSQL :: ![SQLValue] -> Maybe DSCCategory
	mbFromSQL [SQLVEnum "Safety"] = Just DSCSafety
	mbFromSQL [SQLVText "Safety"] = Just DSCSafety
	mbFromSQL [SQLVEnum "Routine"] = Just DSCRoutine
	mbFromSQL [SQLVText "Routine"] = Just DSCRoutine
	mbFromSQL [SQLVEnum "Urgency"] = Just DSCUrgency
	mbFromSQL [SQLVText "Urgency"] = Just DSCUrgency
	mbFromSQL [SQLVEnum "Distress"] = Just DSCDistress
	mbFromSQL [SQLVText "Distress"] = Just DSCDistress
	mbFromSQL [SQLVEnum "Invalid"] = Just DSCInvalid
	mbFromSQL [SQLVText "Invalid"] = Just DSCInvalid
	mbFromSQL _ = Nothing

instance mbToSQL CommunicationStatus where
	mbToSQL :: !(Maybe CommunicationStatus) -> [SQLValue]
	mbToSQL (Just Pending) = [SQLVEnum "Pending"]
	mbToSQL (Just Ringing) = [SQLVEnum "Ringing"]
	mbToSQL (Just Connected) = [SQLVEnum "Connected"]
	mbToSQL (Just Answered) = [SQLVEnum "Answered"]
	mbToSQL (Just Missed) = [SQLVEnum "Missed"]
	mbToSQL (Just Sent) = [SQLVEnum "Sent"]
	mbToSQL Nothing = [SQLVNull]

instance mbFromSQL CommunicationStatus where
	mbFromSQL :: ![SQLValue] -> Maybe CommunicationStatus
	mbFromSQL [SQLVEnum "Pending"] = Just Pending
	mbFromSQL [SQLVText "Pending"] = Just Pending
	mbFromSQL [SQLVEnum "Ringing"] = Just Ringing
	mbFromSQL [SQLVText "Ringing"] = Just Ringing
	mbFromSQL [SQLVEnum "Connected"] = Just Connected
	mbFromSQL [SQLVText "Connected"] = Just Connected
	mbFromSQL [SQLVEnum "Answered"] = Just Answered
	mbFromSQL [SQLVText "Answered"] = Just Answered
	mbFromSQL [SQLVEnum "Missed"] = Just Missed
	mbFromSQL [SQLVText "Missed"] = Just Missed
	mbFromSQL [SQLVEnum "Sent"] = Just Sent
	mbFromSQL [SQLVText "Sent"] = Just Sent
	mbFromSQL _ = Nothing

instance mbToSQL CommunicationDirection where
	mbToSQL :: !(Maybe CommunicationDirection) -> [SQLValue]
	mbToSQL (Just In) = [SQLVEnum "In"]
	mbToSQL (Just Out) = [SQLVEnum "Out"]
	mbToSQL Nothing = [SQLVNull]

instance mbFromSQL CommunicationDirection where
	mbFromSQL :: ![SQLValue] -> Maybe CommunicationDirection
	mbFromSQL [SQLVEnum "In"] = Just In
	mbFromSQL [SQLVText "In"] = Just In
	mbFromSQL [SQLVEnum "Out"] = Just Out
	mbFromSQL [SQLVText "Out"] = Just Out
	mbFromSQL _ = Nothing

instance mbToSQL CommunicationType where
	mbToSQL :: !(Maybe CommunicationType) -> [SQLValue]
	mbToSQL (Just PhoneCall) = [SQLVEnum "Phone call"]
	mbToSQL (Just RadioCall) = [SQLVEnum "Radio call"]
	mbToSQL (Just EmailMessage) = [SQLVEnum "E-mail message"]
	mbToSQL (Just P2000Message) = [SQLVEnum "P2000 message"]
	mbToSQL (Just DSCMessage) = [SQLVEnum "DSC message"]
	mbToSQL Nothing = [SQLVNull]

instance mbFromSQL CommunicationType where
	mbFromSQL :: ![SQLValue] -> Maybe CommunicationType
	mbFromSQL [SQLVEnum "Phone call"] = Just PhoneCall
	mbFromSQL [SQLVText "Phone call"] = Just PhoneCall
	mbFromSQL [SQLVEnum "Radio call"] = Just RadioCall
	mbFromSQL [SQLVText "Radio call"] = Just RadioCall
	mbFromSQL [SQLVEnum "E-mail message"] = Just EmailMessage
	mbFromSQL [SQLVText "E-mail message"] = Just EmailMessage
	mbFromSQL [SQLVEnum "P2000 message"] = Just P2000Message
	mbFromSQL [SQLVText "P2000 message"] = Just P2000Message
	mbFromSQL [SQLVEnum "DSC message"] = Just DSCMessage
	mbFromSQL [SQLVText "DSC message"] = Just DSCMessage
	mbFromSQL _ = Nothing

instance fromSQLRow EmailMessage where
	fromSQLRow :: ![SQLValue] -> EmailMessage // EmailMessage
	fromSQLRow [recipient,sender,subject,body]
	  = {EmailMessage|recipient=fromSQL [recipient],sender=fromSQL [sender],subject=fromSQL [subject],body=fromSQL [body]}
instance toSQLRow EmailMessage where
	toSQLRow :: !EmailMessage -> [SQLValue] // EmailMessage
	toSQLRow {EmailMessage|recipient,sender,subject,body}
	  =  (toSQL recipient ++ toSQL sender ++ toSQL subject ++ toSQL body)

instance fromSQLRow P2000Message where
	fromSQLRow :: ![SQLValue] -> P2000Message // P2000Message
	fromSQLRow [capCode,prio,body]
	  = {P2000Message|capCode=fromSQL [capCode],prio=fromSQL [prio],body=fromSQL [body]}
instance toSQLRow P2000Message where
	toSQLRow :: !P2000Message -> [SQLValue] // P2000Message
	toSQLRow {P2000Message|capCode,prio,body}
	  =  (toSQL capCode ++ toSQL prio ++ toSQL body)

instance fromSQLRow WeatherData where
	fromSQLRow :: ![SQLValue] -> WeatherData // WeatherData
	fromSQLRow [weatherType,windDirection,windSpeed,visibility,seaState,swellDirection,waveHeight,airTemp,seaTemp,cloudBase,barometric]
	  = {WeatherData|weatherType=mbFromSQL [weatherType],windDirection=mbFromSQL [windDirection],windSpeed=mbFromSQL [windSpeed],visibility=mbFromSQL [visibility],seaState=mbFromSQL [seaState],swellDirection=mbFromSQL [swellDirection],waveHeight=mbFromSQL [waveHeight],airTemp=mbFromSQL [airTemp],seaTemp=mbFromSQL [seaTemp],cloudBase=mbFromSQL [cloudBase],barometric=mbFromSQL [barometric]}
instance toSQLRow WeatherData where
	toSQLRow :: !WeatherData -> [SQLValue] // WeatherData
	toSQLRow {WeatherData|weatherType,windDirection,windSpeed,visibility,seaState,swellDirection,waveHeight,airTemp,seaTemp,cloudBase,barometric}
	  =  (mbToSQL weatherType ++ mbToSQL windDirection ++ mbToSQL windSpeed ++ mbToSQL visibility ++ mbToSQL seaState ++ mbToSQL swellDirection ++ mbToSQL waveHeight ++ mbToSQL airTemp ++ mbToSQL seaTemp ++ mbToSQL cloudBase ++ mbToSQL barometric)

instance fromSQLRow TelephoneDetails where
	fromSQLRow :: ![SQLValue] -> TelephoneDetails // Telephone
	fromSQLRow [phoneNo]
	  = {TelephoneDetails|phoneNo=mbFromSQL [phoneNo]}
instance toSQLRow TelephoneDetails where
	toSQLRow :: !TelephoneDetails -> [SQLValue] // Telephone
	toSQLRow {TelephoneDetails|phoneNo}
	  =  (mbToSQL phoneNo)

instance fromSQLRow VHFRadioDetails where
	fromSQLRow :: ![SQLValue] -> VHFRadioDetails // VHFRadio
	fromSQLRow [callSign,mmsi]
	  = {VHFRadioDetails|callSign=mbFromSQL [callSign],mmsi=mbFromSQL [mmsi]}
instance toSQLRow VHFRadioDetails where
	toSQLRow :: !VHFRadioDetails -> [SQLValue] // VHFRadio
	toSQLRow {VHFRadioDetails|callSign,mmsi}
	  =  (mbToSQL callSign ++ mbToSQL mmsi)

instance fromSQLRow EmailAccountDetails where
	fromSQLRow :: ![SQLValue] -> EmailAccountDetails // EmailAccount
	fromSQLRow [emailAddress]
	  = {EmailAccountDetails|emailAddress=mbFromSQL [emailAddress]}
instance toSQLRow EmailAccountDetails where
	toSQLRow :: !EmailAccountDetails -> [SQLValue] // EmailAccount
	toSQLRow {EmailAccountDetails|emailAddress}
	  =  (mbToSQL emailAddress)

instance fromSQLRow P2000ReceiverDetails where
	fromSQLRow :: ![SQLValue] -> P2000ReceiverDetails // P2000Receiver
	fromSQLRow [capCode]
	  = {P2000ReceiverDetails|capCode=mbFromSQL [capCode]}
instance toSQLRow P2000ReceiverDetails where
	toSQLRow :: !P2000ReceiverDetails -> [SQLValue] // P2000Receiver
	toSQLRow {P2000ReceiverDetails|capCode}
	  =  (mbToSQL capCode)

instance fromSQLRow ContactPhoto where
	fromSQLRow :: ![SQLValue] -> ContactPhoto // ContactPhoto
	fromSQLRow [original,thumb,avatar]
	  = {ContactPhoto|original=fromSQL [original],thumb=fromSQL [thumb],avatar=fromSQL [avatar]}
instance toSQLRow ContactPhoto where
	toSQLRow :: !ContactPhoto -> [SQLValue] // ContactPhoto
	toSQLRow {ContactPhoto|original,thumb,avatar}
	  =  (toSQL original ++ toSQL thumb ++ toSQL avatar)

instance fromSQLRow PersonDetails where
	fromSQLRow :: ![SQLValue] -> PersonDetails // Person
	fromSQLRow [age,gender,nationality,injuries,stateOfMind,medicalHistory]
	  = {PersonDetails|age=mbFromSQL [age],gender=mbFromSQL [gender],nationality=mbFromSQL [nationality],injuries=mbFromSQL [injuries],stateOfMind=mbFromSQL [stateOfMind],medicalHistory=mbFromSQL [medicalHistory]}
instance toSQLRow PersonDetails where
	toSQLRow :: !PersonDetails -> [SQLValue] // Person
	toSQLRow {PersonDetails|age,gender,nationality,injuries,stateOfMind,medicalHistory}
	  =  (mbToSQL age ++ mbToSQL gender ++ mbToSQL nationality ++ mbToSQL injuries ++ mbToSQL stateOfMind ++ mbToSQL medicalHistory)

instance fromSQLRow VesselDetails where
	fromSQLRow :: ![SQLValue] -> VesselDetails // Vessel
	fromSQLRow [vesselType,imo,inmarsatNo,description,pob,engineType,fuel,destination,course,speed,lpc,npc,range,lseOnBoard,navaidsOnBoard]
	  = {VesselDetails|vesselType=mbFromSQL [vesselType],imo=mbFromSQL [imo],inmarsatNo=mbFromSQL [inmarsatNo],description=mbFromSQL [description],pob=mbFromSQL [pob],engineType=mbFromSQL [engineType],fuel=mbFromSQL [fuel],destination=mbFromSQL [destination],course=mbFromSQL [course],speed=mbFromSQL [speed],lpc=mbFromSQL [lpc],npc=mbFromSQL [npc],range=mbFromSQL [range],lseOnBoard=fromSQL [lseOnBoard],navaidsOnBoard=fromSQL [navaidsOnBoard]}
instance toSQLRow VesselDetails where
	toSQLRow :: !VesselDetails -> [SQLValue] // Vessel
	toSQLRow {VesselDetails|vesselType,imo,inmarsatNo,description,pob,engineType,fuel,destination,course,speed,lpc,npc,range,lseOnBoard,navaidsOnBoard}
	  =  (mbToSQL vesselType ++ mbToSQL imo ++ mbToSQL inmarsatNo ++ mbToSQL description ++ mbToSQL pob ++ mbToSQL engineType ++ mbToSQL fuel ++ mbToSQL destination ++ mbToSQL course ++ mbToSQL speed ++ mbToSQL lpc ++ mbToSQL npc ++ mbToSQL range ++ toSQL lseOnBoard ++ toSQL navaidsOnBoard)

instance fromSQLRow SurferDetails where
	fromSQLRow :: ![SQLValue] -> SurferDetails // Surfer
	fromSQLRow [surfboardDescription,suitDescription,age,gender,nationality,injuries,stateOfMind,medicalHistory]
	  = {SurferDetails|surfboardDescription=mbFromSQL [surfboardDescription],suitDescription=mbFromSQL [suitDescription],age=mbFromSQL [age],gender=mbFromSQL [gender],nationality=mbFromSQL [nationality],injuries=mbFromSQL [injuries],stateOfMind=mbFromSQL [stateOfMind],medicalHistory=mbFromSQL [medicalHistory]}
instance toSQLRow SurferDetails where
	toSQLRow :: !SurferDetails -> [SQLValue] // Surfer
	toSQLRow {SurferDetails|surfboardDescription,suitDescription,age,gender,nationality,injuries,stateOfMind,medicalHistory}
	  =  (mbToSQL surfboardDescription ++ mbToSQL suitDescription ++ mbToSQL age ++ mbToSQL gender ++ mbToSQL nationality ++ mbToSQL injuries ++ mbToSQL stateOfMind ++ mbToSQL medicalHistory)

instance fromSQLRow DiverDetails where
	fromSQLRow :: ![SQLValue] -> DiverDetails // Diver
	fromSQLRow [description]
	  = {DiverDetails|description=mbFromSQL [description]}
instance toSQLRow DiverDetails where
	toSQLRow :: !DiverDetails -> [SQLValue] // Diver
	toSQLRow {DiverDetails|description}
	  =  (mbToSQL description)

instance fromSQLRow AirplaneDetails where
	fromSQLRow :: ![SQLValue] -> AirplaneDetails // Airplane
	fromSQLRow [callsign,planeType]
	  = {AirplaneDetails|callsign=mbFromSQL [callsign],planeType=mbFromSQL [planeType]}
instance toSQLRow AirplaneDetails where
	toSQLRow :: !AirplaneDetails -> [SQLValue] // Airplane
	toSQLRow {AirplaneDetails|callsign,planeType}
	  =  (mbToSQL callsign ++ mbToSQL planeType)

instance fromSQLRow HelicopterDetails where
	fromSQLRow :: ![SQLValue] -> HelicopterDetails // Helicopter
	fromSQLRow [callsign,helicopterType]
	  = {HelicopterDetails|callsign=mbFromSQL [callsign],helicopterType=mbFromSQL [helicopterType]}
instance toSQLRow HelicopterDetails where
	toSQLRow :: !HelicopterDetails -> [SQLValue] // Helicopter
	toSQLRow {HelicopterDetails|callsign,helicopterType}
	  =  (mbToSQL callsign ++ mbToSQL helicopterType)

instance fromSQLRow IncidentShort where
	fromSQLRow :: ![SQLValue] -> IncidentShort // Incident
	fromSQLRow [incidentNo,title]
	  = {IncidentShort|incidentNo=fromSQL [incidentNo],title=mbFromSQL [title]}
instance toSQLRow IncidentShort where
	toSQLRow :: !IncidentShort -> [SQLValue] // Incident
	toSQLRow {IncidentShort|incidentNo,title}
	  =  (toSQL incidentNo ++ mbToSQL title)

instance fromSQLRow IncidentBasic where
	fromSQLRow :: ![SQLValue] -> IncidentBasic // Incident
	fromSQLRow [title,summary,type,phase]
	  = {IncidentBasic|title=mbFromSQL [title],summary=mbFromSQL [summary],type=mbFromSQL [type],phase=mbFromSQL [phase]}
instance toSQLRow IncidentBasic where
	toSQLRow :: !IncidentBasic -> [SQLValue] // Incident
	toSQLRow {IncidentBasic|title,summary,type,phase}
	  =  (mbToSQL title ++ mbToSQL summary ++ mbToSQL type ++ mbToSQL phase)

instance fromSQLRow IncidentDetails where
	fromSQLRow :: ![SQLValue] -> IncidentDetails // Incident
	fromSQLRow [incidentNo,title,summary,type,phase]
	  = {IncidentDetails|incidentNo=fromSQL [incidentNo],title=mbFromSQL [title],summary=mbFromSQL [summary],type=mbFromSQL [type],phase=mbFromSQL [phase]}
instance toSQLRow IncidentDetails where
	toSQLRow :: !IncidentDetails -> [SQLValue] // Incident
	toSQLRow {IncidentDetails|incidentNo,title,summary,type,phase}
	  =  (toSQL incidentNo ++ mbToSQL title ++ mbToSQL summary ++ mbToSQL type ++ mbToSQL phase)

instance fromSQLRow ContactShort where
	fromSQLRow :: ![SQLValue] -> ContactShort // Contact
	fromSQLRow [contactNo,type,name,group]
	  = {ContactShort|contactNo=fromSQL [contactNo],type=mbFromSQL [type],name=mbFromSQL [name],group=mbFromSQL [group]}
instance toSQLRow ContactShort where
	toSQLRow :: !ContactShort -> [SQLValue] // Contact
	toSQLRow {ContactShort|contactNo,type,name,group}
	  =  (toSQL contactNo ++ mbToSQL type ++ mbToSQL name ++ mbToSQL group)

instance fromSQLRow ContactAccess where
	fromSQLRow :: ![SQLValue] -> ContactAccess // Contact
	fromSQLRow [account,access]
	  = {ContactAccess|account=mbFromSQL [account],access=mbFromSQL [access]}
instance toSQLRow ContactAccess where
	toSQLRow :: !ContactAccess -> [SQLValue] // Contact
	toSQLRow {ContactAccess|account,access}
	  =  (mbToSQL account ++ mbToSQL access)

instance fromSQLRow NewIncident where
	fromSQLRow :: ![SQLValue] -> NewIncident // Incident
	fromSQLRow [title,summary,type]
	  = {NewIncident|title=mbFromSQL [title],summary=mbFromSQL [summary],type=mbFromSQL [type]}
instance toSQLRow NewIncident where
	toSQLRow :: !NewIncident -> [SQLValue] // Incident
	toSQLRow {NewIncident|title,summary,type}
	  =  (mbToSQL title ++ mbToSQL summary ++ mbToSQL type)
IncidoneDB :: SQLSchema
IncidoneDB = [{SQLTable|name = "AISContact",columns = [{SQLColumn|name = "mmsi",type = SQLTInteger ,null = True,autoIncrement = False},{SQLColumn|name = "position_lat",type = SQLTReal ,null = True,autoIncrement = False},{SQLColumn|name = "position_lon",type = SQLTReal ,null = True,autoIncrement = False},{SQLColumn|name = "position_desc",type = SQLTText ,null = True,autoIncrement = False},{SQLColumn|name = "heading",type = SQLTText ,null = True,autoIncrement = False},{SQLColumn|name = "track",type = SQLTText ,null = True,autoIncrement = False},{SQLColumn|name = "lastPositionMsg",type = SQLTText ,null = True,autoIncrement = False},{SQLColumn|name = "lastInfoMsg",type = SQLTText ,null = True,autoIncrement = False},{SQLColumn|name = "positionUpdated",type = SQLTDatetime ,null = True,autoIncrement = False},{SQLColumn|name = "infoUpdated",type = SQLTDatetime ,null = True,autoIncrement = False}],primaryKey = ["mmsi"],foreignKeys = []},{SQLTable|name = "Contact",columns = [{SQLColumn|name = "contactNo",type = SQLTInteger ,null = True,autoIncrement = True},{SQLColumn|name = "type",type = SQLTEnum ["Person","Vessel","Surfer","Diver","Airplane","Helicopter","Other"],null = True,autoIncrement = False},{SQLColumn|name = "name",type = SQLTText ,null = True,autoIncrement = False},{SQLColumn|name = "group",type = SQLTText ,null = True,autoIncrement = False},{SQLColumn|name = "position_lat",type = SQLTReal ,null = True,autoIncrement = False},{SQLColumn|name = "position_lon",type = SQLTReal ,null = True,autoIncrement = False},{SQLColumn|name = "position_desc",type = SQLTText ,null = True,autoIncrement = False},{SQLColumn|name = "heading",type = SQLTText ,null = True,autoIncrement = False},{SQLColumn|name = "track",type = SQLTText ,null = True,autoIncrement = False},{SQLColumn|name = "positionUpdated",type = SQLTDatetime ,null = True,autoIncrement = False},{SQLColumn|name = "needsHelp",type = SQLTInteger ,null = True,autoIncrement = False},{SQLColumn|name = "providesHelp",type = SQLTInteger ,null = True,autoIncrement = False},{SQLColumn|name = "notes",type = SQLTText ,null = True,autoIncrement = False},{SQLColumn|name = "account",type = SQLTText ,null = True,autoIncrement = False},{SQLColumn|name = "access",type = SQLTEnum ["Watch officer","Partner"],null = True,autoIncrement = False},{SQLColumn|name = "status",type = SQLTEnum ["Alerted","Briefed","Proceeding","On Scene","Standby On Location","On Task","Standby","Transfer","Released","Returned","Limited Availability","Unavailable","Training","Pre-Alert"],null = True,autoIncrement = False}],primaryKey = ["contactNo"],foreignKeys = []},{SQLTable|name = "Helicopter",columns = [{SQLColumn|name = "callsign",type = SQLTText ,null = True,autoIncrement = False},{SQLColumn|name = "helicopterType",type = SQLTText ,null = True,autoIncrement = False},{SQLColumn|name = "contactNo",type = SQLTInteger ,null = True,autoIncrement = False}],primaryKey = ["contactNo"],foreignKeys = [(["contactNo"],"Contact",["contactNo"])]},{SQLTable|name = "Airplane",columns = [{SQLColumn|name = "callsign",type = SQLTText ,null = True,autoIncrement = False},{SQLColumn|name = "planeType",type = SQLTText ,null = True,autoIncrement = False},{SQLColumn|name = "contactNo",type = SQLTInteger ,null = True,autoIncrement = False}],primaryKey = ["contactNo"],foreignKeys = [(["contactNo"],"Contact",["contactNo"])]},{SQLTable|name = "Diver",columns = [{SQLColumn|name = "description",type = SQLTText ,null = True,autoIncrement = False},{SQLColumn|name = "contactNo",type = SQLTInteger ,null = True,autoIncrement = False}],primaryKey = ["contactNo"],foreignKeys = [(["contactNo"],"Contact",["contactNo"])]},{SQLTable|name = "Surfer",columns = [{SQLColumn|name = "surfboardDescription",type = SQLTText ,null = True,autoIncrement = False},{SQLColumn|name = "suitDescription",type = SQLTText ,null = True,autoIncrement = False},{SQLColumn|name = "age",type = SQLTInteger ,null = True,autoIncrement = False},{SQLColumn|name = "gender",type = SQLTEnum ["Male","Female"],null = True,autoIncrement = False},{SQLColumn|name = "nationality",type = SQLTText ,null = True,autoIncrement = False},{SQLColumn|name = "injuries",type = SQLTText ,null = True,autoIncrement = False},{SQLColumn|name = "stateOfMind",type = SQLTText ,null = True,autoIncrement = False},{SQLColumn|name = "medicalHistory",type = SQLTText ,null = True,autoIncrement = False},{SQLColumn|name = "contactNo",type = SQLTInteger ,null = True,autoIncrement = False}],primaryKey = ["contactNo"],foreignKeys = [(["contactNo"],"Contact",["contactNo"])]},{SQLTable|name = "Vessel",columns = [{SQLColumn|name = "vesselType",type = SQLTEnum ["Yacht","Fishing vessel","Cargo vessel","Towing vessel","Patrol vessel","Rescue vessel","Other"],null = True,autoIncrement = False},{SQLColumn|name = "imo",type = SQLTInteger ,null = True,autoIncrement = False},{SQLColumn|name = "inmarsatNo",type = SQLTText ,null = True,autoIncrement = False},{SQLColumn|name = "description",type = SQLTText ,null = True,autoIncrement = False},{SQLColumn|name = "pob",type = SQLTInteger ,null = True,autoIncrement = False},{SQLColumn|name = "engineType",type = SQLTText ,null = True,autoIncrement = False},{SQLColumn|name = "fuel",type = SQLTText ,null = True,autoIncrement = False},{SQLColumn|name = "destination",type = SQLTText ,null = True,autoIncrement = False},{SQLColumn|name = "course",type = SQLTInteger ,null = True,autoIncrement = False},{SQLColumn|name = "speed",type = SQLTInteger ,null = True,autoIncrement = False},{SQLColumn|name = "lpc",type = SQLTText ,null = True,autoIncrement = False},{SQLColumn|name = "npc",type = SQLTText ,null = True,autoIncrement = False},{SQLColumn|name = "range",type = SQLTText ,null = True,autoIncrement = False},{SQLColumn|name = "lseOnBoard",type = SQLTInteger ,null = True,autoIncrement = False},{SQLColumn|name = "navaidsOnBoard",type = SQLTInteger ,null = True,autoIncrement = False},{SQLColumn|name = "contactNo",type = SQLTInteger ,null = True,autoIncrement = False}],primaryKey = ["contactNo"],foreignKeys = [(["contactNo"],"Contact",["contactNo"])]},{SQLTable|name = "Person",columns = [{SQLColumn|name = "age",type = SQLTInteger ,null = True,autoIncrement = False},{SQLColumn|name = "gender",type = SQLTEnum ["Male","Female"],null = True,autoIncrement = False},{SQLColumn|name = "nationality",type = SQLTText ,null = True,autoIncrement = False},{SQLColumn|name = "injuries",type = SQLTText ,null = True,autoIncrement = False},{SQLColumn|name = "stateOfMind",type = SQLTText ,null = True,autoIncrement = False},{SQLColumn|name = "medicalHistory",type = SQLTText ,null = True,autoIncrement = False},{SQLColumn|name = "contactNo",type = SQLTInteger ,null = True,autoIncrement = False}],primaryKey = ["contactNo"],foreignKeys = [(["contactNo"],"Contact",["contactNo"])]},{SQLTable|name = "ContactPhoto",columns = [{SQLColumn|name = "original",type = SQLTText ,null = True,autoIncrement = False},{SQLColumn|name = "thumb",type = SQLTText ,null = True,autoIncrement = False},{SQLColumn|name = "avatar",type = SQLTText ,null = True,autoIncrement = False}],primaryKey = [],foreignKeys = []},{SQLTable|name = "CommunicationMean",columns = [{SQLColumn|name = "id",type = SQLTInteger ,null = True,autoIncrement = True},{SQLColumn|name = "type",type = SQLTEnum ["Telephone","VHF","E-mail","P2000"],null = True,autoIncrement = False}],primaryKey = ["id"],foreignKeys = []},{SQLTable|name = "P2000Receiver",columns = [{SQLColumn|name = "capCode",type = SQLTText ,null = True,autoIncrement = False},{SQLColumn|name = "id",type = SQLTInteger ,null = True,autoIncrement = False}],primaryKey = ["id"],foreignKeys = [(["id"],"CommunicationMean",["id"])]},{SQLTable|name = "EmailAccount",columns = [{SQLColumn|name = "emailAddress",type = SQLTText ,null = True,autoIncrement = False},{SQLColumn|name = "id",type = SQLTInteger ,null = True,autoIncrement = False}],primaryKey = ["id"],foreignKeys = [(["id"],"CommunicationMean",["id"])]},{SQLTable|name = "VHFRadio",columns = [{SQLColumn|name = "callSign",type = SQLTText ,null = True,autoIncrement = False},{SQLColumn|name = "mmsi",type = SQLTInteger ,null = True,autoIncrement = False},{SQLColumn|name = "id",type = SQLTInteger ,null = True,autoIncrement = False}],primaryKey = ["id"],foreignKeys = [(["id"],"CommunicationMean",["id"])]},{SQLTable|name = "Telephone",columns = [{SQLColumn|name = "phoneNo",type = SQLTText ,null = True,autoIncrement = False},{SQLColumn|name = "id",type = SQLTInteger ,null = True,autoIncrement = False}],primaryKey = ["id"],foreignKeys = [(["id"],"CommunicationMean",["id"])]},{SQLTable|name = "Incident",columns = [{SQLColumn|name = "incidentNo",type = SQLTInteger ,null = True,autoIncrement = True},{SQLColumn|name = "title",type = SQLTText ,null = True,autoIncrement = False},{SQLColumn|name = "summary",type = SQLTText ,null = True,autoIncrement = False},{SQLColumn|name = "type",type = SQLTEnum ["Aircraft appears in difficulties","Aircraft collision","Aircraft crash","Aircraft ditch","Aircraft emergency","Aircraft overdue","Diver in problems","Diver missing","DSC alert in NLSSR","DSC alert outside NLSSR","False alert good intent","False alert malicious intent","Family message","Flare sighted","INMARSAT alert in NLSRR","INMARSAT alert outside NLSRR","Medevac","Medical advice","NRB test","Person appears in difficulties","Person body recovery","Person in problems","Person missing","Rig incident","Rig man overboard","SAR HELI unavailable","SART alert","Surfer appears in difficulties","Surfer in problems","Surfer missing","Vessel appears in difficulties","Vessel capsized","Vessel collision","Vessel engine or steering problems","Vessel fire or explosion","Vessel man overboard","Vessel overdue","Vessel piracy or hijack","Vessel sunk","Vessel taking water","Vessel unsure of position","Yacht aground","Yacht appears in difficulties","Yacht capsized","Yacht collision","Yacht engine problems","Yacht fire or explosion","Yacht gear fouled","Yacht man overboard"],null = True,autoIncrement = False},{SQLColumn|name = "phase",type = SQLTEnum ["INCERFA","ALERFA","DETRESFA"],null = True,autoIncrement = False},{SQLColumn|name = "closed",type = SQLTInteger ,null = True,autoIncrement = False}],primaryKey = ["incidentNo"],foreignKeys = []},{SQLTable|name = "WeatherData",columns = [{SQLColumn|name = "weatherType",type = SQLTEnum ["Rain","Drizzle","Mist","Fog","Snow","Hail","Sunny","Haze","Cloudy","Showers","Thunderstorms"],null = True,autoIncrement = False},{SQLColumn|name = "windDirection",type = SQLTText ,null = True,autoIncrement = False},{SQLColumn|name = "windSpeed",type = SQLTText ,null = True,autoIncrement = False},{SQLColumn|name = "visibility",type = SQLTText ,null = True,autoIncrement = False},{SQLColumn|name = "seaState",type = SQLTText ,null = True,autoIncrement = False},{SQLColumn|name = "swellDirection",type = SQLTText ,null = True,autoIncrement = False},{SQLColumn|name = "waveHeight",type = SQLTText ,null = True,autoIncrement = False},{SQLColumn|name = "airTemp",type = SQLTText ,null = True,autoIncrement = False},{SQLColumn|name = "seaTemp",type = SQLTText ,null = True,autoIncrement = False},{SQLColumn|name = "cloudBase",type = SQLTText ,null = True,autoIncrement = False},{SQLColumn|name = "barometric",type = SQLTInteger ,null = True,autoIncrement = False},{SQLColumn|name = "incidentNo",type = SQLTInteger ,null = True,autoIncrement = False}],primaryKey = ["incidentNo"],foreignKeys = [(["incidentNo"],"Incident",["incidentNo"])]},{SQLTable|name = "LogEntry",columns = [{SQLColumn|name = "incident",type = SQLTInteger ,null = True,autoIncrement = False},{SQLColumn|name = "eventAt",type = SQLTDatetime ,null = True,autoIncrement = False},{SQLColumn|name = "loggedAt",type = SQLTDatetime ,null = True,autoIncrement = False},{SQLColumn|name = "loggedBy",type = SQLTInteger ,null = True,autoIncrement = False},{SQLColumn|name = "message",type = SQLTText ,null = True,autoIncrement = False}],primaryKey = [],foreignKeys = []},{SQLTable|name = "INMARSATCMessage",columns = [{SQLColumn|name = "inmarsatNo",type = SQLTText ,null = True,autoIncrement = False},{SQLColumn|name = "toCes",type = SQLTInteger ,null = True,autoIncrement = False},{SQLColumn|name = "position_lat",type = SQLTReal ,null = True,autoIncrement = False},{SQLColumn|name = "position_lon",type = SQLTReal ,null = True,autoIncrement = False},{SQLColumn|name = "position_desc",type = SQLTText ,null = True,autoIncrement = False},{SQLColumn|name = "positionUpdated",type = SQLTTime ,null = True,autoIncrement = False},{SQLColumn|name = "course",type = SQLTInteger ,null = True,autoIncrement = False},{SQLColumn|name = "speed",type = SQLTInteger ,null = True,autoIncrement = False},{SQLColumn|name = "activation",type = SQLTText ,null = True,autoIncrement = False},{SQLColumn|name = "positionActivated",type = SQLTInteger ,null = True,autoIncrement = False},{SQLColumn|name = "courseSpeedActivated",type = SQLTInteger ,null = True,autoIncrement = False}],primaryKey = [],foreignKeys = []},{SQLTable|name = "DSCMessage",columns = [{SQLColumn|name = "callTime",type = SQLTDatetime ,null = True,autoIncrement = False},{SQLColumn|name = "category",type = SQLTEnum ["Safety","Routine","Urgency","Distress","Invalid"],null = True,autoIncrement = False},{SQLColumn|name = "fromMmsi",type = SQLTInteger ,null = True,autoIncrement = False},{SQLColumn|name = "fromCountry",type = SQLTText ,null = True,autoIncrement = False},{SQLColumn|name = "no",type = SQLTInteger ,null = True,autoIncrement = False},{SQLColumn|name = "rqi",type = SQLTInteger ,null = True,autoIncrement = False},{SQLColumn|name = "format",type = SQLTInteger ,null = True,autoIncrement = False},{SQLColumn|name = "distressId",type = SQLTInteger ,null = True,autoIncrement = False},{SQLColumn|name = "nature",type = SQLTText ,null = True,autoIncrement = False},{SQLColumn|name = "position_lat",type = SQLTReal ,null = True,autoIncrement = False},{SQLColumn|name = "position_lon",type = SQLTReal ,null = True,autoIncrement = False},{SQLColumn|name = "position_desc",type = SQLTText ,null = True,autoIncrement = False},{SQLColumn|name = "workstation",type = SQLTText ,null = True,autoIncrement = False},{SQLColumn|name = "teleCmd",type = SQLTText ,null = True,autoIncrement = False}],primaryKey = [],foreignKeys = []},{SQLTable|name = "Communication",columns = [{SQLColumn|name = "communicationNo",type = SQLTInteger ,null = True,autoIncrement = True},{SQLColumn|name = "time",type = SQLTDatetime ,null = True,autoIncrement = False},{SQLColumn|name = "type",type = SQLTEnum ["Phone call","Radio call","E-mail message","P2000 message","DSC message"],null = True,autoIncrement = False},{SQLColumn|name = "direction",type = SQLTEnum ["In","Out"],null = True,autoIncrement = False},{SQLColumn|name = "handledBy",type = SQLTInteger ,null = True,autoIncrement = False},{SQLColumn|name = "status",type = SQLTEnum ["Pending","Ringing","Connected","Answered","Missed","Sent"],null = True,autoIncrement = False},{SQLColumn|name = "withContact",type = SQLTInteger ,null = True,autoIncrement = False}],primaryKey = ["communicationNo"],foreignKeys = []},{SQLTable|name = "P2000Message",columns = [{SQLColumn|name = "capCode",type = SQLTText ,null = True,autoIncrement = False},{SQLColumn|name = "prio",type = SQLTInteger ,null = True,autoIncrement = False},{SQLColumn|name = "body",type = SQLTText ,null = True,autoIncrement = False},{SQLColumn|name = "communicationNo",type = SQLTInteger ,null = True,autoIncrement = False}],primaryKey = ["communicationNo"],foreignKeys = [(["communicationNo"],"Communication",["communicationNo"])]},{SQLTable|name = "EmailMessage",columns = [{SQLColumn|name = "recipient",type = SQLTText ,null = True,autoIncrement = False},{SQLColumn|name = "sender",type = SQLTText ,null = True,autoIncrement = False},{SQLColumn|name = "subject",type = SQLTText ,null = True,autoIncrement = False},{SQLColumn|name = "body",type = SQLTText ,null = True,autoIncrement = False},{SQLColumn|name = "communicationNo",type = SQLTInteger ,null = True,autoIncrement = False}],primaryKey = ["communicationNo"],foreignKeys = [(["communicationNo"],"Communication",["communicationNo"])]},{SQLTable|name = "Message",columns = [{SQLColumn|name = "communicationNo",type = SQLTInteger ,null = True,autoIncrement = False}],primaryKey = ["communicationNo"],foreignKeys = [(["communicationNo"],"Communication",["communicationNo"])]},{SQLTable|name = "RadioCall",columns = [{SQLColumn|name = "channel",type = SQLTInteger ,null = True,autoIncrement = False},{SQLColumn|name = "callNotes",type = SQLTText ,null = True,autoIncrement = False},{SQLColumn|name = "communicationNo",type = SQLTInteger ,null = True,autoIncrement = False}],primaryKey = ["communicationNo"],foreignKeys = [(["communicationNo"],"Communication",["communicationNo"])]},{SQLTable|name = "PhoneCall",columns = [{SQLColumn|name = "externalNo",type = SQLTText ,null = True,autoIncrement = False},{SQLColumn|name = "externalRef",type = SQLTText ,null = True,autoIncrement = False},{SQLColumn|name = "callNotes",type = SQLTText ,null = True,autoIncrement = False},{SQLColumn|name = "communicationNo",type = SQLTInteger ,null = True,autoIncrement = False}],primaryKey = ["communicationNo"],foreignKeys = [(["communicationNo"],"Communication",["communicationNo"])]},{SQLTable|name = "communicationMeans1_communicationMeans2",columns = [{SQLColumn|name = "communicationMeans1",type = SQLTInteger ,null = True,autoIncrement = False},{SQLColumn|name = "communicationMeans2",type = SQLTInteger ,null = True,autoIncrement = False}],primaryKey = [],foreignKeys = []},{SQLTable|name = "photos1_photos2",columns = [{SQLColumn|name = "photos1",type = SQLTInteger ,null = True,autoIncrement = False}],primaryKey = [],foreignKeys = []},{SQLTable|name = "contacts_incidents",columns = [{SQLColumn|name = "contacts",type = SQLTInteger ,null = True,autoIncrement = False},{SQLColumn|name = "incidents",type = SQLTInteger ,null = True,autoIncrement = False}],primaryKey = [],foreignKeys = []},{SQLTable|name = "communications_aboutIncidents",columns = [{SQLColumn|name = "communications",type = SQLTInteger ,null = True,autoIncrement = False},{SQLColumn|name = "aboutIncidents",type = SQLTInteger ,null = True,autoIncrement = False}],primaryKey = [],foreignKeys = []}]