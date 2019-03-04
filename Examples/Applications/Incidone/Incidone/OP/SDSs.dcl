definition module Incidone.OP.SDSs
import iTasks
import Incidone.OP.Concepts
/**
* Shared data sources that provide access to the operational picture
*/

//Filters
:: ContactFilter =
    {filterByName :: Maybe String
    }
derive class iTask ContactFilter

allContactPhotos                :: SimpleSDSLens (Map ContactNo [ContactPhoto])
lastAISImport                   :: SimpleSDSLens (Maybe (DateTime,String,Int))

allCommunications			    :: SDSLens () 				[CommunicationDetails] 	()
allIncidents				    :: SDSLens () 				[Incident] 				()
allIncidentsShort			    :: SDSLens () 				[IncidentShort] 		()
allContacts					    :: SDSLens () 				[Contact] 				()
allContactsShort			    :: SDSLens () 				[ContactShort] 			()
filteredContactsShort           :: SDSLens ContactFilter 	[ContactShort] 			()

allAISContacts                  :: SDSLens () 				[AISContact] 			()
boundedAISContacts              :: SDSLens ContactBounds 	[AISContact] 			()

communicationByNo		        :: SDSLens CommunicationNo 	Communication 			Communication
communicationDetailsByNo		:: SDSParallel CommunicationNo CommunicationDetails CommunicationDetails
phoneCallByNo                   :: SDSLens CommunicationNo PhoneCall PhoneCall
phoneCallByReference            :: SDSLens PhoneCallReference PhoneCall PhoneCall
radioCallByNo                   :: SDSLens CommunicationNo RadioCall RadioCall
emailMessageByNo			    :: SDSLens CommunicationNo EmailMessage EmailMessage
p2000MessageByNo			    :: SDSLens CommunicationNo P2000Message P2000Message

incidentByNo                    :: SDSLens IncidentNo Incident Incident
incidentTitleByNo               :: SDSLens IncidentNo String String
incidentWeather                 :: SDSLens IncidentNo WeatherData WeatherData
incidentLog                     :: SDSParallel IncidentNo [LogEntry] LogEntry
incidentOverview                :: SDSLens IncidentNo IncidentOverview ()

contactByNo                     :: SDSLens ContactNo Contact Contact
contactByMMSI                   :: SDSSequence MMSI (Maybe Contact) (Maybe Contact)
contactByCredentials            :: SDSLens Credentials (Maybe Contact) ()

contactCommunicationMeans       :: SDSLens ContactNo [CommunicationMean] ()
contactCommunications           :: SDSLens ContactNo [CommunicationDetails] ()
contactPhotos                   :: SDSLens ContactNo [ContactPhoto] [ContactPhoto]
contactMMSI                     :: SDSLens ContactNo (Maybe MMSI) ()
contactAIS                      :: SDSSequence ContactNo (Maybe AISContact) ()
contactAccess                   :: SDSLens ContactNo ContactAccess ContactAccess
contactAvatar                   :: SDSLens ContactNo ContactAvatar ()

personDetailsByNo               :: SDSLens ContactNo PersonDetails PersonDetails
vesselDetailsByNo               :: SDSLens ContactNo VesselDetails VesselDetails
surferDetailsByNo               :: SDSLens ContactNo SurferDetails SurferDetails
diverDetailsByNo                :: SDSLens ContactNo DiverDetails DiverDetails
airplaneDetailsByNo             :: SDSLens ContactNo AirplaneDetails AirplaneDetails
helicopterDetailsByNo           :: SDSLens ContactNo HelicopterDetails HelicopterDetails

communicationMeanById           :: SDSSequence CommunicationMeanId CommunicationMean CommunicationMean

openIncidents				    :: SDSLens () [Incident] ()
openIncidentsShort              :: SDSLens () [IncidentShort] ()
openIncidentsDetails		    :: SDSLens () [IncidentDetails] ()
recentIncidents                 :: SDSLens () [Incident] ()
recentIncidentsDetails          :: SDSLens () [IncidentDetails] ()

incidentsByContactShort	        :: SDSSequence ContactNo [IncidentShort] [IncidentNo]
incidentsByContactDetails       :: SDSSequence ContactNo [IncidentDetails] [IncidentNo]
incidentsByCommunicationShort   :: SDSSequence CommunicationNo [IncidentShort] [IncidentNo]
incidentsByNosShort             :: SDSLens [IncidentNo] [IncidentShort] ()

contactsOfOpenIncidents         :: SDSLens () 		[Contact] ()
contactsOfOpenIncidentsShort    :: SDSSequence () 	[ContactShortWithIncidents] ()
contactsOfOpenIncidentsGeo      :: SDSLens () 		[ContactGeo] ()
contactsWithGroupShort          :: SDSLens String 	[ContactShort] ()
contactsNeedingHelpShort        :: SDSLens () 		[ContactShort] ()
contactsProvidingHelpShort      :: SDSLens () 		[ContactShort] ()
contactsProvidingHelpGeo        :: SDSLens () 		[ContactGeo] ()

contactsByNos                   :: SDSLens [ContactNo] [Contact] ()
contactsByNosShort              :: SDSLens [ContactNo] [ContactShort] ()

contactsByIncident              :: SDSSequence IncidentNo [Contact] [ContactNo]
contactsByIncidentShort		    :: SDSSequence IncidentNo [ContactShort] [ContactNo]
contactsByIncidentGeo           :: SDSLens IncidentNo [ContactGeo] ()

currentUserContactNo            :: SDSLens () 		ContactNo ()
currentUserAvatar               :: SDSSequence () 	(Maybe ContactAvatar) ()

AISContactByMMSI                :: SDSSequence MMSI (Maybe AISContact) (Maybe AISContact)

contactNosByIncidentNos              :: SDSSequence (Maybe [IncidentNo]) [(IncidentNo,ContactNo)] [(IncidentNo,ContactNo)]
contactNosByIncidentNosIndexed       :: SDSLens (Maybe [IncidentNo]) (Map IncidentNo [ContactNo]) (Map IncidentNo [ContactNo])
communicationNosByIncidentNos        :: SDSSequence (Maybe [IncidentNo]) [(IncidentNo,CommunicationNo)] [(IncidentNo,CommunicationNo)]
communicationNosByIncidentNosIndexed :: SDSLens (Maybe [IncidentNo]) (Map IncidentNo [CommunicationNo]) (Map IncidentNo [CommunicationNo])
communicationNosByContactNos         :: SDSLens (Maybe [ContactNo]) [(ContactNo,CommunicationNo)] [(ContactNo,CommunicationNo)]
communicationNosByContactNosIndexed  :: SDSLens (Maybe [ContactNo]) (Map ContactNo [CommunicationNo]) (Map ContactNo [CommunicationNo])

incidentNosByContactNos              :: SDSSequence (Maybe [ContactNo]) [(ContactNo,IncidentNo)] [(ContactNo,IncidentNo)]
incidentNosByContactNosIndexed       :: SDSLens (Maybe [ContactNo]) (Map ContactNo [IncidentNo]) (Map ContactNo [IncidentNo])
incidentNosByCommunicationNos        :: SDSSequence (Maybe [CommunicationNo]) [(CommunicationNo,IncidentNo)] [(CommunicationNo,IncidentNo)]
incidentNosByCommunicationNosIndexed :: SDSLens (Maybe [CommunicationNo]) (Map CommunicationNo [IncidentNo]) (Map CommunicationNo [IncidentNo])

