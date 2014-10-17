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

allContactPhotos                :: Shared (Map ContactNo [ContactPhoto])
lastAISImport                   :: Shared (Maybe (DateTime,String,Int))

allCommunications			    :: ROShared ()              [CommunicationDetails]
allIncidents				    :: ROShared ()              [Incident]
allIncidentsShort			    :: ROShared ()              [IncidentShort]
allContacts					    :: ROShared ()              [Contact]
allContactsShort			    :: ROShared ()              [ContactShort]
filteredContactsShort           :: ROShared ContactFilter   [ContactShort]

allAISContacts                  :: ROShared ()              [AISContact]
boundedAISContacts              :: ROShared ContactBounds   [AISContact]

communicationByNo		        :: RWShared CommunicationNo Communication Communication
communicationDetailsByNo		:: RWShared CommunicationNo CommunicationDetails CommunicationDetails
phoneCallByNo                   :: RWShared CommunicationNo PhoneCall PhoneCall
phoneCallByReference            :: RWShared PhoneCallReference PhoneCall PhoneCall
radioCallByNo                   :: RWShared CommunicationNo RadioCall RadioCall
emailMessageByNo			    :: RWShared CommunicationNo EmailMessage EmailMessage
p2000MessageByNo			    :: RWShared CommunicationNo P2000Message P2000Message

incidentByNo                    :: RWShared IncidentNo Incident Incident
incidentTitleByNo               :: RWShared IncidentNo String String
incidentWeather                 :: RWShared IncidentNo WeatherData WeatherData
incidentLog                     :: RWShared IncidentNo [LogEntry] LogEntry
incidentOverview                :: ROShared IncidentNo IncidentOverview

contactByNo                     :: RWShared ContactNo Contact Contact
contactByMMSI                   :: RWShared MMSI (Maybe Contact) (Maybe Contact)
contactByCredentials            :: ROShared Credentials (Maybe Contact)

contactCommunicationMeans       :: ROShared ContactNo [CommunicationMean]
contactCommunications           :: ROShared ContactNo [CommunicationDetails]
contactPhotos                   :: RWShared ContactNo [ContactPhoto] [ContactPhoto]
contactMMSI                     :: ROShared ContactNo (Maybe MMSI)
contactAIS                      :: ROShared ContactNo (Maybe AISContact)
contactAccess                   :: RWShared ContactNo ContactAccess ContactAccess
contactAvatar                   :: ROShared ContactNo ContactAvatar


personDetailsByNo               :: RWShared ContactNo PersonDetails PersonDetails
vesselDetailsByNo               :: RWShared ContactNo VesselDetails VesselDetails
surferDetailsByNo               :: RWShared ContactNo SurferDetails SurferDetails
diverDetailsByNo                :: RWShared ContactNo DiverDetails DiverDetails
airplaneDetailsByNo             :: RWShared ContactNo AirplaneDetails AirplaneDetails
helicopterDetailsByNo           :: RWShared ContactNo HelicopterDetails HelicopterDetails

communicationMeanById           :: RWShared CommunicationMeanId CommunicationMean CommunicationMean

openIncidents				    :: ROShared ()  [Incident]
openIncidentsShort              :: ROShared ()  [IncidentShort]
openIncidentsDetails		    :: ROShared ()  [IncidentDetails]
recentIncidents                 :: ROShared ()  [Incident]
recentIncidentsDetails          :: ROShared ()  [IncidentDetails]

incidentsByContactShort	        :: RWShared ContactNo [IncidentShort] [IncidentNo]
incidentsByContactDetails       :: RWShared ContactNo [IncidentDetails] [IncidentNo]
incidentsByCommunicationShort   :: RWShared CommunicationNo [IncidentShort] [IncidentNo]
incidentsByNosShort             :: ROShared [IncidentNo] [IncidentShort]

contactsOfOpenIncidents         :: ROShared ()      [Contact]
contactsOfOpenIncidentsShort    :: ROShared ()      [ContactShortWithIncidents]
contactsOfOpenIncidentsGeo      :: ROShared ()      [ContactGeo]
contactsWithGroupShort          :: ROShared String  [ContactShort]
contactsNeedingHelpShort        :: ROShared ()      [ContactShort]
contactsProvidingHelpShort      :: ROShared ()      [ContactShort]
contactsProvidingHelpGeo        :: ROShared ()      [ContactGeo]

contactsByNos                   :: ROShared [ContactNo] [Contact]
contactsByNosShort              :: ROShared [ContactNo] [ContactShort]

contactsByIncident              :: RWShared IncidentNo [Contact] [ContactNo]
contactsByIncidentShort		    :: RWShared IncidentNo [ContactShort] [ContactNo]
contactsByIncidentGeo           :: ROShared IncidentNo [ContactGeo]

currentUserContactNo            :: ROShared ()      ContactNo
currentUserAvatar               :: ROShared ()      (Maybe ContactAvatar)

AISContactByMMSI                :: RWShared MMSI (Maybe AISContact) (Maybe AISContact)

contactNosByIncidentNos              :: RWShared (Maybe [IncidentNo]) [(IncidentNo,ContactNo)] [(IncidentNo,ContactNo)]
contactNosByIncidentNosIndexed       :: RWShared (Maybe [IncidentNo]) (Map IncidentNo [ContactNo]) (Map IncidentNo [ContactNo])
communicationNosByIncidentNos        :: RWShared (Maybe [IncidentNo]) [(IncidentNo,CommunicationNo)] [(IncidentNo,CommunicationNo)]
communicationNosByIncidentNosIndexed :: RWShared (Maybe [IncidentNo]) (Map IncidentNo [CommunicationNo]) (Map IncidentNo [CommunicationNo])
communicationNosByContactNos         :: RWShared (Maybe [ContactNo]) [(ContactNo,CommunicationNo)] [(ContactNo,CommunicationNo)]
communicationNosByContactNosIndexed  :: RWShared (Maybe [ContactNo]) (Map ContactNo [CommunicationNo]) (Map ContactNo [CommunicationNo])

incidentNosByContactNos              :: RWShared (Maybe [ContactNo]) [(ContactNo,IncidentNo)] [(ContactNo,IncidentNo)]
incidentNosByContactNosIndexed       :: RWShared (Maybe [ContactNo]) (Map ContactNo [IncidentNo]) (Map ContactNo [IncidentNo])
incidentNosByCommunicationNos        :: RWShared (Maybe [CommunicationNo]) [(CommunicationNo,IncidentNo)] [(CommunicationNo,IncidentNo)]
incidentNosByCommunicationNosIndexed :: RWShared (Maybe [CommunicationNo]) (Map CommunicationNo [IncidentNo]) (Map CommunicationNo [IncidentNo])

