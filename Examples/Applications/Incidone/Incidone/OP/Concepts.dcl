definition module Incidone.OP.Concepts

import iTasks, Database.SQL, Incidone.OP.ConceptsTOP, Incidone.OP.ConceptsSQL, Incidone.ContactPosition, Incidone.Util.AIS


:: AISStaticInfo :== AIVDM5
:: AISPosition :== AIVDMCNB
:: HelicopterType :== String
:: AirplaneType :== String
:: DiverDescription :== Note
:: SurfSuitDescription :== Note
:: SurfboardDescription :== Note
:: VesselRange :== Miles
:: NextPortOfCall :== String
:: LastPortOfCall :== String
:: VesselSpeed :== Int
:: VesselCourse :== Int
:: VesselDestination :== String
:: VesselFuel :== String
:: VesselEngineType :== String
:: VesselPob :== Int
:: VesselDescription :== Note
:: VesselType = Yacht | FishingVessel | CargoVessel | TowingVessel | PatrolVessel | RescueVessel | OtherVessel
:: MedicalHistory :== Note
:: StateOfMind :== Note
:: Injuries :== Note
:: Nationality :== String
:: Gender = Male | Female
:: PersonAge :== Int
:: ContactPhotoAvatar :== Document
:: ContactPhotoThumb :== Document
:: ContactPhotoOriginal :== Document
:: CommunicationMeanType = CMPhone | CMVHF | CMEmail | CMP2000
:: CommunicationMeanId :== Int
:: ContactAccessLevel = WOAccess | PartnerAccess
:: ContactStatus = Alerted | Briefed | Proceeding | OnScene | StandbyOnLocation | OnTask | StandBy | Transfer | Released | Returned | LimitedAvailability | UnAvailable | Training | PreAlert
:: ContactNotes :== Note
:: ContactHeading :== Degrees
:: ContactGroup :== String
:: ContactName :== String
:: ContactType = Person | Vessel | Surfer | Diver | Airplane | Helicopter | OtherContact
:: ContactNo :== Int
:: Barometric :== Int
:: CloudBase :== Feet
:: SeaTemperature :== Temperature
:: AirTemperature :== Temperature
:: WaveHeight :== Meters
:: SwellDirection :== Degrees
:: SeaState :== String
:: Visibility :== Feet
:: WindSpeed :== Knots
:: WindDirection :== String
:: WeatherType = Rain | Drizzle | Mist | Fog | Snow | Hail | Sunny | Haze | Cloudy | Showers | Thunderstorms
:: LogMessage :== Note
:: IncidentType = AircraftAppearsInDifficulties | AircraftCollision | AircraftCrash | AircraftDitch | AircraftEmergency | AircraftOverdue | DiverInProblems | DiverMissing | DSCAlertInsideNLSRR | DSCAlertOutsideNLSRR | FalseAlertGoodIntent | FalseAlertMaliciousIntent | FamilyMessage | FlareSighted | INMARSATAlertInsideNLSRR | INMARSATAlertOutsideNLSRR | Medevac | MedicalAdvice | NRBTest | PersonAppearsInDifficulties | PersonBodyRecovery | PersonsInProblems | PersonsMissing | RigIncident | RigManOverboard | SARHeliUnavailable | SARTAlert | SurferAppearsInDifficulties | SurferInProblems | SurferMissing | VesselAppearsInDifficulties | VesselCapsized | VesselCollision | VesselEngineOrSteeringProblems | VesselFireOrExplosion | VesselManOverboard | VesselOverdue | VesselPiracyOrHijack | VesselSunk | VesselTakingWater | VesselUnsureOfPosition | YachtAground | YachtAppearsInDiffulties | YachtCapsized | YachtCollision | YachtEngineProblems | YachtFireOrExplosion | YachtGearFouled | YachtManOverboard
:: EmergencyPhase = INCERFA | ALERFA | DETRESFA
:: IncidentSummary :== Note
:: IncidentTitle :== String
:: IncidentNo :== Int
:: INMARSATCActivation :== String
:: INMARSATStation :== Int
:: INMARSATNumber :== String
:: DSCCategory = DSCSafety | DSCRoutine | DSCUrgency | DSCDistress | DSCInvalid
:: DSCTeleCommand :== String
:: DSCWorkstation :== String
:: DSCNature :== String
:: DSCDistressId :== Int
:: DSCFormat :== Int
:: DSCRQI :== Int
:: DSCNumber :== Int
:: DSCCountry :== String
:: P2000Prio :== Int
:: P2000Body :== Note
:: EmailBody :== Note
:: EmailSubject :== String
:: EmailSender :== EmailAddress
:: EmailRecipient :== EmailAddress
:: PhoneCallReference :== String
:: CallNotes :== Note
:: CommunicationStatus = Pending | Ringing | Connected | Answered | Missed | Sent
:: CommunicationDirection = In | Out
:: CommunicationType = PhoneCall | RadioCall | EmailMessage | P2000Message | DSCMessage
:: CommunicationNo :== Int
:: Degrees = Degrees !Int
:: Knots = Knots !Int
:: Miles = Miles !Int
:: Feet = Feet !Int
:: Meters = Meters !Int
:: Temperature = Temperature !Int
:: RadioChannel :== Int
:: CallSign :== String
:: PhoneNo :== String
:: HeadingInDegrees :== Int
:: SpeedInKnots :== Int
:: UTCDateTime :== DateTime
:: UTCTime :== Time
:: IMONumber :== Int
:: CapCode :== String
:: MMSI :== Int

:: EmailMessage =
	{recipient :: EmailRecipient
	,sender :: EmailSender
	,subject :: EmailSubject
	,body :: EmailBody
	}
:: P2000Message =
	{capCode :: CapCode
	,prio :: P2000Prio
	,body :: P2000Body
	}
:: DSCMessage =
	{callTime :: UTCDateTime
	,category :: DSCCategory
	,fromMmsi :: MMSI
	,fromCountry :: Maybe DSCCountry
	,no :: Maybe DSCNumber
	,rqi :: Maybe DSCRQI
	,format :: Maybe DSCFormat
	,distressId :: Maybe DSCDistressId
	,nature :: Maybe DSCNature
	,position :: Maybe ContactPosition
	,workstation :: Maybe DSCWorkstation
	,teleCmd :: Maybe DSCTeleCommand
	}
:: INMARSATCMessage =
	{inmarsatNo :: INMARSATNumber
	,toCes :: INMARSATStation
	,position :: ContactPosition
	,positionUpdated :: UTCTime
	,course :: Maybe HeadingInDegrees
	,speed :: Maybe SpeedInKnots
	,activation :: INMARSATCActivation
	,positionActivated :: Bool
	,courseSpeedActivated :: Bool
	}
:: PhoneCall =
	{externalNo :: Maybe PhoneNo
	,externalRef :: Maybe PhoneCallReference
	,callNotes :: Maybe CallNotes
	,communicationNo :: CommunicationNo
	}
:: RadioCall =
	{channel :: Maybe RadioChannel
	,callNotes :: Maybe CallNotes
	,communicationNo :: CommunicationNo
	}
:: Communication =
	{communicationNo :: CommunicationNo
	,time :: UTCDateTime
	,type :: CommunicationType
	,direction :: CommunicationDirection
	,handledBy :: Maybe ContactNo
	,status :: Maybe CommunicationStatus
	,withContact :: Maybe ContactNo
	,aboutIncidents :: [IncidentNo]
	}
:: CommunicationDetails =
	{communicationNo :: CommunicationNo
	,time :: UTCDateTime
	,type :: CommunicationType
	,direction :: CommunicationDirection
	,handledBy :: Maybe ContactShort
	,status :: Maybe CommunicationStatus
	,withContact :: Maybe ContactShort
	,aboutIncidents :: [IncidentShort]
	,externalNo :: Maybe PhoneNo
	}
:: Incident =
	{incidentNo :: IncidentNo
	,title :: Maybe IncidentTitle
	,summary :: Maybe IncidentSummary
	,type :: Maybe IncidentType
	,phase :: Maybe EmergencyPhase
	,weather :: WeatherData
	,log :: [LogEntry]
	,closed :: Bool
	,contacts :: [ContactNo]
	,communications :: [CommunicationNo]
	}
:: LogEntry =
	{incident :: IncidentNo
	,eventAt :: UTCDateTime
	,loggedAt :: UTCDateTime
	,loggedBy :: Maybe ContactAvatar
	,message :: LogMessage
	}
:: WeatherData =
	{weatherType :: Maybe WeatherType
	,windDirection :: Maybe WindDirection
	,windSpeed :: Maybe WindSpeed
	,visibility :: Maybe Visibility
	,seaState :: Maybe SeaState
	,swellDirection :: Maybe SwellDirection
	,waveHeight :: Maybe WaveHeight
	,airTemp :: Maybe AirTemperature
	,seaTemp :: Maybe SeaTemperature
	,cloudBase :: Maybe CloudBase
	,barometric :: Maybe Barometric
	}
:: Contact =
	{contactNo :: ContactNo
	,type :: Maybe ContactType
	,name :: Maybe ContactName
	,group :: Maybe ContactGroup
	,position :: Maybe ContactPosition
	,heading :: Maybe ContactHeading
	,track :: Maybe ContactTrack
	,positionUpdated :: Maybe UTCDateTime
	,needsHelp :: Bool
	,providesHelp :: Bool
	,communicationMeans :: [CommunicationMeanId]
	,photos :: [ContactPhoto]
	,notes :: Maybe ContactNotes
	,account :: Maybe Credentials
	,access :: Maybe ContactAccessLevel
	,status :: Maybe ContactStatus
	,communicationsWith :: [CommunicationNo]
	,incidents :: [IncidentNo]
	}
:: CommunicationMean =
	{id :: CommunicationMeanId
	,type :: CommunicationMeanType
	,capCode :: Maybe CapCode
	,emailAddress :: Maybe EmailAddress
	,callSign :: Maybe CallSign
	,mmsi :: Maybe MMSI
	,phoneNo :: Maybe PhoneNo
	}
:: TelephoneDetails =
	{phoneNo :: Maybe PhoneNo
	}
:: VHFRadioDetails =
	{callSign :: Maybe CallSign
	,mmsi :: Maybe MMSI
	}
:: EmailAccountDetails =
	{emailAddress :: Maybe EmailAddress
	}
:: P2000ReceiverDetails =
	{capCode :: Maybe CapCode
	}
:: ContactPhoto =
	{original :: ContactPhotoOriginal
	,thumb :: ContactPhotoThumb
	,avatar :: ContactPhotoAvatar
	}
:: PersonDetails =
	{age :: Maybe PersonAge
	,gender :: Maybe Gender
	,nationality :: Maybe Nationality
	,injuries :: Maybe Injuries
	,stateOfMind :: Maybe StateOfMind
	,medicalHistory :: Maybe MedicalHistory
	}
:: VesselDetails =
	{vesselType :: Maybe VesselType
	,imo :: Maybe IMONumber
	,inmarsatNo :: Maybe INMARSATNumber
	,description :: Maybe VesselDescription
	,pob :: Maybe VesselPob
	,engineType :: Maybe VesselEngineType
	,fuel :: Maybe VesselFuel
	,destination :: Maybe VesselDestination
	,course :: Maybe VesselCourse
	,speed :: Maybe VesselSpeed
	,lpc :: Maybe LastPortOfCall
	,npc :: Maybe NextPortOfCall
	,range :: Maybe VesselRange
	,lseOnBoard :: Bool
	,navaidsOnBoard :: Bool
	}
:: SurferDetails =
	{surfboardDescription :: Maybe SurfboardDescription
	,suitDescription :: Maybe SurfSuitDescription
	,age :: Maybe PersonAge
	,gender :: Maybe Gender
	,nationality :: Maybe Nationality
	,injuries :: Maybe Injuries
	,stateOfMind :: Maybe StateOfMind
	,medicalHistory :: Maybe MedicalHistory
	}
:: DiverDetails =
	{description :: Maybe DiverDescription
	}
:: AirplaneDetails =
	{callsign :: Maybe CallSign
	,planeType :: Maybe AirplaneType
	}
:: HelicopterDetails =
	{callsign :: Maybe CallSign
	,helicopterType :: Maybe HelicopterType
	}
:: AISContact =
	{mmsi :: MMSI
	,position :: Maybe ContactPosition
	,heading :: Maybe ContactHeading
	,track :: Maybe ContactTrack
	,lastPositionMsg :: Maybe AISPosition
	,lastInfoMsg :: Maybe AISStaticInfo
	,positionUpdated :: Maybe UTCDateTime
	,infoUpdated :: Maybe UTCDateTime
	}
:: IncidentShort =
	{incidentNo :: IncidentNo
	,title :: Maybe IncidentTitle
	}
:: IncidentBasic =
	{title :: Maybe IncidentTitle
	,summary :: Maybe IncidentSummary
	,type :: Maybe IncidentType
	,phase :: Maybe EmergencyPhase
	}
:: IncidentDetails =
	{incidentNo :: IncidentNo
	,title :: Maybe IncidentTitle
	,summary :: Maybe IncidentSummary
	,type :: Maybe IncidentType
	,phase :: Maybe EmergencyPhase
	}
:: ContactShort =
	{contactNo :: ContactNo
	,type :: Maybe ContactType
	,name :: Maybe ContactName
	,group :: Maybe ContactGroup
	}
:: ContactShortWithIncidents =
	{contactNo :: ContactNo
	,type :: Maybe ContactType
	,name :: Maybe ContactName
	,group :: Maybe ContactGroup
	,incidents :: [IncidentShort]
	}
:: ContactNameTypePosition =
	{type :: Maybe ContactType
	,name :: Maybe ContactName
	,position :: Maybe ContactPosition
	}
:: ContactBasic =
	{type :: Maybe ContactType
	,name :: Maybe ContactName
	,group :: Maybe ContactGroup
	,position :: Maybe ContactPosition
	,heading :: Maybe ContactHeading
	,needsHelp :: Bool
	,providesHelp :: Bool
	,notes :: Maybe ContactNotes
	,status :: Maybe ContactStatus
	}
:: ContactDetails =
	{contactNo :: ContactNo
	,type :: Maybe ContactType
	,name :: Maybe ContactName
	,position :: Maybe ContactPosition
	,notes :: Maybe ContactNotes
	}
:: ContactAvatar =
	{contactNo :: ContactNo
	,type :: Maybe ContactType
	,name :: Maybe ContactName
	,photos :: [ContactPhoto]
	}
:: ContactGeo =
	{contactNo :: ContactNo
	,type :: Maybe ContactType
	,name :: Maybe ContactName
	,group :: Maybe ContactGroup
	,position :: Maybe ContactPosition
	,heading :: Maybe ContactHeading
	,track :: Maybe ContactTrack
	,positionUpdated :: Maybe UTCDateTime
	,needsHelp :: Bool
	,providesHelp :: Bool
	}
:: ContactAccess =
	{account :: Maybe Credentials
	,access :: Maybe ContactAccessLevel
	}
:: NewContact =
	{type :: Maybe ContactType
	,name :: Maybe ContactName
	,position :: Maybe ContactPosition
	,needsHelp :: Bool
	}
:: NewIncident =
	{title :: Maybe IncidentTitle
	,summary :: Maybe IncidentSummary
	,type :: Maybe IncidentType
	}
:: NewCommunicationMean =
	{type :: CommunicationMeanType
	,capCode :: Maybe CapCode
	,emailAddress :: Maybe EmailAddress
	,callSign :: Maybe CallSign
	,mmsi :: Maybe MMSI
	,phoneNo :: Maybe PhoneNo
	}
class mbToSQL a where mbToSQL :: !(Maybe a) -> [SQLValue]
class mbFromSQL a where mbFromSQL :: ![SQLValue] -> Maybe a
class toSQLRow a where toSQLRow :: !a -> [SQLValue]
class fromSQLRow a where fromSQLRow :: ![SQLValue] -> a
instance mbToSQL Int
instance mbToSQL String
instance mbToSQL Bool
instance mbFromSQL Int
instance mbFromSQL String
instance mbFromSQL Bool

instance mbToSQL VesselType where
	mbToSQL :: !(Maybe VesselType) -> [SQLValue]
instance mbFromSQL VesselType where
	mbFromSQL :: ![SQLValue] -> Maybe VesselType

instance mbToSQL Gender where
	mbToSQL :: !(Maybe Gender) -> [SQLValue]
instance mbFromSQL Gender where
	mbFromSQL :: ![SQLValue] -> Maybe Gender

instance mbToSQL CommunicationMeanType where
	mbToSQL :: !(Maybe CommunicationMeanType) -> [SQLValue]
instance mbFromSQL CommunicationMeanType where
	mbFromSQL :: ![SQLValue] -> Maybe CommunicationMeanType

instance mbToSQL ContactAccessLevel where
	mbToSQL :: !(Maybe ContactAccessLevel) -> [SQLValue]
instance mbFromSQL ContactAccessLevel where
	mbFromSQL :: ![SQLValue] -> Maybe ContactAccessLevel

instance mbToSQL ContactStatus where
	mbToSQL :: !(Maybe ContactStatus) -> [SQLValue]
instance mbFromSQL ContactStatus where
	mbFromSQL :: ![SQLValue] -> Maybe ContactStatus

instance mbToSQL ContactType where
	mbToSQL :: !(Maybe ContactType) -> [SQLValue]
instance mbFromSQL ContactType where
	mbFromSQL :: ![SQLValue] -> Maybe ContactType

instance mbToSQL WeatherType where
	mbToSQL :: !(Maybe WeatherType) -> [SQLValue]
instance mbFromSQL WeatherType where
	mbFromSQL :: ![SQLValue] -> Maybe WeatherType

instance mbToSQL IncidentType where
	mbToSQL :: !(Maybe IncidentType) -> [SQLValue]
instance mbFromSQL IncidentType where
	mbFromSQL :: ![SQLValue] -> Maybe IncidentType

instance mbToSQL EmergencyPhase where
	mbToSQL :: !(Maybe EmergencyPhase) -> [SQLValue]
instance mbFromSQL EmergencyPhase where
	mbFromSQL :: ![SQLValue] -> Maybe EmergencyPhase

instance mbToSQL DSCCategory where
	mbToSQL :: !(Maybe DSCCategory) -> [SQLValue]
instance mbFromSQL DSCCategory where
	mbFromSQL :: ![SQLValue] -> Maybe DSCCategory

instance mbToSQL CommunicationStatus where
	mbToSQL :: !(Maybe CommunicationStatus) -> [SQLValue]
instance mbFromSQL CommunicationStatus where
	mbFromSQL :: ![SQLValue] -> Maybe CommunicationStatus

instance mbToSQL CommunicationDirection where
	mbToSQL :: !(Maybe CommunicationDirection) -> [SQLValue]
instance mbFromSQL CommunicationDirection where
	mbFromSQL :: ![SQLValue] -> Maybe CommunicationDirection

instance mbToSQL CommunicationType where
	mbToSQL :: !(Maybe CommunicationType) -> [SQLValue]
instance mbFromSQL CommunicationType where
	mbFromSQL :: ![SQLValue] -> Maybe CommunicationType

instance fromSQLRow EmailMessage where
	fromSQLRow :: ![SQLValue] -> EmailMessage // EmailMessage
instance toSQLRow EmailMessage where
	toSQLRow :: !EmailMessage -> [SQLValue] // EmailMessage

instance fromSQLRow P2000Message where
	fromSQLRow :: ![SQLValue] -> P2000Message // P2000Message
instance toSQLRow P2000Message where
	toSQLRow :: !P2000Message -> [SQLValue] // P2000Message

instance fromSQLRow WeatherData where
	fromSQLRow :: ![SQLValue] -> WeatherData // WeatherData
instance toSQLRow WeatherData where
	toSQLRow :: !WeatherData -> [SQLValue] // WeatherData

instance fromSQLRow TelephoneDetails where
	fromSQLRow :: ![SQLValue] -> TelephoneDetails // Telephone
instance toSQLRow TelephoneDetails where
	toSQLRow :: !TelephoneDetails -> [SQLValue] // Telephone

instance fromSQLRow VHFRadioDetails where
	fromSQLRow :: ![SQLValue] -> VHFRadioDetails // VHFRadio
instance toSQLRow VHFRadioDetails where
	toSQLRow :: !VHFRadioDetails -> [SQLValue] // VHFRadio

instance fromSQLRow EmailAccountDetails where
	fromSQLRow :: ![SQLValue] -> EmailAccountDetails // EmailAccount
instance toSQLRow EmailAccountDetails where
	toSQLRow :: !EmailAccountDetails -> [SQLValue] // EmailAccount

instance fromSQLRow P2000ReceiverDetails where
	fromSQLRow :: ![SQLValue] -> P2000ReceiverDetails // P2000Receiver
instance toSQLRow P2000ReceiverDetails where
	toSQLRow :: !P2000ReceiverDetails -> [SQLValue] // P2000Receiver

instance fromSQLRow ContactPhoto where
	fromSQLRow :: ![SQLValue] -> ContactPhoto // ContactPhoto
instance toSQLRow ContactPhoto where
	toSQLRow :: !ContactPhoto -> [SQLValue] // ContactPhoto

instance fromSQLRow PersonDetails where
	fromSQLRow :: ![SQLValue] -> PersonDetails // Person
instance toSQLRow PersonDetails where
	toSQLRow :: !PersonDetails -> [SQLValue] // Person

instance fromSQLRow VesselDetails where
	fromSQLRow :: ![SQLValue] -> VesselDetails // Vessel
instance toSQLRow VesselDetails where
	toSQLRow :: !VesselDetails -> [SQLValue] // Vessel

instance fromSQLRow SurferDetails where
	fromSQLRow :: ![SQLValue] -> SurferDetails // Surfer
instance toSQLRow SurferDetails where
	toSQLRow :: !SurferDetails -> [SQLValue] // Surfer

instance fromSQLRow DiverDetails where
	fromSQLRow :: ![SQLValue] -> DiverDetails // Diver
instance toSQLRow DiverDetails where
	toSQLRow :: !DiverDetails -> [SQLValue] // Diver

instance fromSQLRow AirplaneDetails where
	fromSQLRow :: ![SQLValue] -> AirplaneDetails // Airplane
instance toSQLRow AirplaneDetails where
	toSQLRow :: !AirplaneDetails -> [SQLValue] // Airplane

instance fromSQLRow HelicopterDetails where
	fromSQLRow :: ![SQLValue] -> HelicopterDetails // Helicopter
instance toSQLRow HelicopterDetails where
	toSQLRow :: !HelicopterDetails -> [SQLValue] // Helicopter

instance fromSQLRow IncidentShort where
	fromSQLRow :: ![SQLValue] -> IncidentShort // Incident
instance toSQLRow IncidentShort where
	toSQLRow :: !IncidentShort -> [SQLValue] // Incident

instance fromSQLRow IncidentBasic where
	fromSQLRow :: ![SQLValue] -> IncidentBasic // Incident
instance toSQLRow IncidentBasic where
	toSQLRow :: !IncidentBasic -> [SQLValue] // Incident

instance fromSQLRow IncidentDetails where
	fromSQLRow :: ![SQLValue] -> IncidentDetails // Incident
instance toSQLRow IncidentDetails where
	toSQLRow :: !IncidentDetails -> [SQLValue] // Incident

instance fromSQLRow ContactShort where
	fromSQLRow :: ![SQLValue] -> ContactShort // Contact
instance toSQLRow ContactShort where
	toSQLRow :: !ContactShort -> [SQLValue] // Contact

instance fromSQLRow ContactAccess where
	fromSQLRow :: ![SQLValue] -> ContactAccess // Contact
instance toSQLRow ContactAccess where
	toSQLRow :: !ContactAccess -> [SQLValue] // Contact

instance fromSQLRow NewIncident where
	fromSQLRow :: ![SQLValue] -> NewIncident // Incident
instance toSQLRow NewIncident where
	toSQLRow :: !NewIncident -> [SQLValue] // Incident

IncidoneDB :: SQLSchema
