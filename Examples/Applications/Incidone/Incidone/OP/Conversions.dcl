definition module Incidone.OP.Conversions
/**
* This module provides conversion functions between the various
* that are defined in the conceptual model of the operational picture.
*/
import Incidone.OP.Concepts

//Titles
class contactTitle a :: a -> String
instance contactTitle Contact
instance contactTitle ContactShort

class incidentTitle a :: a -> String
instance incidentTitle Incident
instance incidentTitle IncidentShort

//Subtitles
contactSubTitle :: Contact -> String
incidentSubTitle :: Incident -> String

//Icons
class contactIcon a :: a -> String
instance contactIcon Contact
instance contactIcon ContactShort

contactTypeIcon :: (Maybe ContactType) -> String

contactThumbHtml    :: Contact -> HtmlTag
contactAvatarHtml   :: Contact -> HtmlTag

// Identification types
communicationIdentity :: CommunicationDetails -> CommunicationNo

class contactIdentity a :: a -> ContactNo
instance contactIdentity Contact
instance contactIdentity ContactShort

class incidentIdentity a :: a -> IncidentNo
instance incidentIdentity Incident
instance incidentIdentity IncidentShort

// Summary types
contactSummary		:: Contact -> ContactShort

// Details types (extended summary)
contactDetails :: Contact -> ContactDetails
incidentDetails :: Incident -> IncidentDetails

:: IncidentOverview =
	{title                :: Maybe IncidentTitle
	,summary              :: Maybe IncidentSummary
	,type                 :: Maybe IncidentType
    ,contactsNeedingHelp  :: [ContactNameTypePosition]
    }

//Predicates
matchesMMSI :: MMSI Contact -> Bool

//AIS data conversion
:: AISDetails =
    { name                :: Maybe String
    , mmsi                :: MMSI
    , position            :: Maybe ContactPosition
    , callsign            :: Maybe String
    }

aisToContact    :: AISContact -> Contact
aisToContactGeo :: AISContact -> ContactGeo
aisToDetails    :: AISContact -> AISDetails
aisPosition     :: AIVDMCNB -> ContactPosition
aisHeading      :: AIVDMCNB -> ContactHeading


//Complex updates
updAISContactPosition   :: DateTime ContactPosition ContactHeading AISContact -> AISContact
updContactPosition      :: DateTime (Maybe ContactPosition) (Maybe ContactHeading) Contact -> Contact

//Representation expressing the exclusive nature of the communicationmean subtypes
:: XCommunicationMean
    = XCMTelephone TelephoneDetails
    | XCMVHFRadio VHFRadioDetails
    | XCMEmail EmailAccountDetails
    | XCMP2000 P2000ReceiverDetails

toCommunicationMean         :: CommunicationMeanId XCommunicationMean -> CommunicationMean
fromCommunicationMean       :: CommunicationMean -> (CommunicationMeanId,XCommunicationMean)

toNewCommunicationMean      :: XCommunicationMean -> NewCommunicationMean
fromNewCommunicationMean    :: NewCommunicationMean -> XCommunicationMean

//Users
contactUser     :: Contact -> User
userContactNo   :: User -> Maybe ContactNo
