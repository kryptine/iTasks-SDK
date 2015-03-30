implementation module Incidone.OP.Conversions

import Incidone.OP.Concepts
import Text

class contactTitle a :: a -> String
instance contactTitle Contact
where
    contactTitle {Contact|name,type} = contactTitle` name type
instance contactTitle ContactShort
where
    contactTitle {ContactShort|name,type} = contactTitle` name type

contactTitle` name type = fromMaybe ("Unknown "+++ typeName type) name
where
    typeName (Just Person)      = "person"
    typeName (Just Vessel)      = "vessel"
    typeName (Just Surfer)      = "surfer"
    typeName (Just Diver)       = "diver"
    typeName (Just Airplane)    = "airplane"
    typeName (Just Helicopter)  = "helicopter"
    typeName _                  = "contact"

class incidentTitle a :: a -> String
instance incidentTitle Incident
where
    incidentTitle {Incident|title} = fromMaybe "-" title

instance incidentTitle IncidentShort
where
    incidentTitle {IncidentShort|title} = fromMaybe "-" title

contactSubTitle :: Contact -> String
contactSubTitle {Contact|type} = toSingleLineText type

incidentSubTitle :: Incident -> String
incidentSubTitle {Incident|summary} = maybe "-" toString summary

instance contactIcon Contact
where contactIcon {Contact|type} = contactTypeIcon type

instance contactIcon ContactShort
where contactIcon {ContactShort|type} = contactTypeIcon type

contactTypeIcon :: (Maybe ContactType) -> String
contactTypeIcon (Just Person)     = "contact-person"
contactTypeIcon (Just Vessel)     = "contact-vessel"
contactTypeIcon (Just Surfer)     = "contact-surfer"
contactTypeIcon (Just Diver)      = "contact-diver"
contactTypeIcon (Just Airplane)   = "contact-airplane"
contactTypeIcon (Just Helicopter) = "contact-helicopter"
contactTypeIcon _                 = "contact-other"

contactThumbHtml :: Contact -> HtmlTag
contactThumbHtml{Contact|type,photos} = ImgTag [WidthAttr "200",HeightAttr "200",SrcAttr (src type photos)]
where
    src _ [{ContactPhoto|thumb}:_]	= thumb.Document.contentUrl
	src (Just Person) _				= "/thumbs/person.jpg"
	src (Just Vessel) _				= "/thumbs/vessel.jpg"
	src (Just Airplane) _			= "/thumbs/airplane.jpg"
	src (Just Helicopter) _			= "/thumbs/helicopter.jpg"
	src _ _						    = "/thumbs/other.jpg"

contactAvatarHtml :: Contact -> HtmlTag
contactAvatarHtml{Contact|type,photos} = ImgTag [WidthAttr "50",HeightAttr "50",SrcAttr (src type photos)]
where
    src _ [{ContactPhoto|avatar}:_]	= avatar.Document.contentUrl
	src (Just Person) _				= "/avatars/person.jpg"
	src (Just Vessel) _				= "/avatars/vessel.jpg"
	src (Just Airplane) _			= "/avatars/airplane.jpg"
	src (Just Helicopter) _			= "/avatars/helicopter.jpg"
	src _ _						    = "/avatars/other.jpg"

communicationIdentity :: CommunicationDetails -> CommunicationNo
communicationIdentity {CommunicationDetails|communicationNo} = communicationNo

class contactIdentity a :: a -> ContactNo
instance contactIdentity Contact
where
    contactIdentity {Contact|contactNo} = contactNo
instance contactIdentity ContactShort
where
    contactIdentity {ContactShort|contactNo} = contactNo

class incidentIdentity a :: a -> IncidentNo
instance incidentIdentity Incident
where
    incidentIdentity {Incident|incidentNo} = incidentNo
instance incidentIdentity IncidentShort
where
    incidentIdentity {IncidentShort|incidentNo} = incidentNo

contactSummary :: Contact -> ContactShort
contactSummary {Contact|contactNo,name,type,group}
	= {ContactShort|contactNo = contactNo, name = name, type = type, group = group}
		
incidentDetails :: Incident -> IncidentDetails
incidentDetails {Incident|incidentNo,title,summary,type,phase}
	= {IncidentDetails|incidentNo = incidentNo, title = title, summary = summary, type = type, phase = phase}
	
contactDetails :: Contact -> ContactDetails
contactDetails {Contact|contactNo,name,type,position,notes}
	= {ContactDetails|contactNo = contactNo, name = name, type = type, position = position, notes = notes}

contactUser :: Contact -> User
contactUser {Contact|contactNo,name,access}
    = AuthenticatedUser ("C"<+++ contactNo) (roles access) name
where
    roles (Just WOAccess)       = ["wo"]
    roles (Just PartnerAccess)  = ["partner"]
    roles _                     = []

userContactNo :: User -> Maybe ContactNo
userContactNo (AuthenticatedUser uid _ _)
    | startsWith "C" uid
        # contactNo = toInt (subString 1 (textSize uid) uid)
        = if (contactNo > 0)  (Just contactNo) Nothing
    | otherwise = Nothing
userContactNo _ = Nothing

matchesMMSI :: MMSI Contact -> Bool
matchesMMSI match contact = False

instance < LogEntry where (<) a b	= a.LogEntry.eventAt < b.LogEntry.eventAt

aisToContact :: AISContact -> Contact
aisToContact {AISContact|mmsi,position,heading,track,lastPositionMsg,lastInfoMsg,positionUpdated}
    = {Contact|defaultValue & contactNo= mmsi, name = aisName mmsi lastInfoMsg, position = position, heading = heading, track = track, positionUpdated = positionUpdated}

aisToContactGeo :: AISContact -> ContactGeo
aisToContactGeo {AISContact|mmsi,position,heading,track,lastInfoMsg,positionUpdated}
    = {ContactGeo|defaultValue & contactNo= mmsi, name = aisName mmsi lastInfoMsg, position = position, heading = heading, track = track, positionUpdated = positionUpdated}

aisToDetails :: AISContact -> AISDetails
aisToDetails {AISContact|mmsi,position,lastPositionMsg,lastInfoMsg}
    = {AISDetails|name = aisName mmsi lastInfoMsg, mmsi = mmsi, position = position, callsign = aisCallsign lastInfoMsg}

aisName mmsi (Just {AIVDM5|shipname})   = Just (if (shipname == "") (toString mmsi) shipname)
aisName mmsi _                          = Just (toString mmsi)

aisCallsign (Just {AIVDM5|callsign=""})    = Nothing
aisCallsign (Just {AIVDM5|callsign})       = Just callsign
aisCallsign _                               = Nothing

aisPosition :: AIVDMCNB -> ContactPosition
aisPosition {AIVDMCNB|lat,lon} = PositionLatLng (toReal lat / 600000.0, toReal lon / 600000.0)

aisHeading :: AIVDMCNB -> ContactHeading
aisHeading {AIVDMCNB|heading} = Degrees heading

updAISContactPosition :: DateTime ContactPosition ContactHeading AISContact -> AISContact
updAISContactPosition now nposition nheading c=:{AISContact|position,heading,track,positionUpdated}
    # ntrack = case (position,positionUpdated) of
        (Just (PositionLatLng (lat,lng)),Just dt)
            = Just (ContactTrack [(dt,lat,lng):maybe [] (\(ContactTrack t) -> t) track])
        _   = track
    = {AISContact|c & position = Just nposition, heading = Just nheading, track = ntrack, positionUpdated = Just now}

updContactPosition :: DateTime (Maybe ContactPosition) (Maybe ContactHeading) Contact -> Contact
updContactPosition now nposition nheading c=:{Contact|position,heading,track,positionUpdated}
    # ntrack = case (position,positionUpdated) of
        (Just (PositionLatLng (lat,lng)),Just dt)
            = Just (ContactTrack [(dt,lat,lng):maybe [] (\(ContactTrack t) -> t) track])
        _   = track
    = {Contact|c & position = nposition, heading = nheading, track = ntrack, positionUpdated = Just now}


toCommunicationMean :: CommunicationMeanId XCommunicationMean -> CommunicationMean
toCommunicationMean id (XCMTelephone {TelephoneDetails|phoneNo})        = {CommunicationMean|defaultValue & id = id, type = CMPhone, phoneNo = phoneNo}
toCommunicationMean id (XCMVHFRadio {VHFRadioDetails|callSign,mmsi})    = {CommunicationMean|defaultValue & id = id, type = CMVHF, callSign = callSign, mmsi = mmsi}
toCommunicationMean id (XCMEmail {EmailAccountDetails|emailAddress})    = {CommunicationMean|defaultValue & id = id, type = CMEmail, emailAddress = emailAddress}
toCommunicationMean id (XCMP2000 {P2000ReceiverDetails|capCode})        = {CommunicationMean|defaultValue & id = id, type = CMP2000, capCode = capCode}

fromCommunicationMean :: CommunicationMean -> (CommunicationMeanId,XCommunicationMean)
fromCommunicationMean {CommunicationMean|id,type,phoneNo,callSign,mmsi,emailAddress,capCode} = case type of
    CMPhone = (id,XCMTelephone {TelephoneDetails|phoneNo=phoneNo})
    CMVHF   = (id,XCMVHFRadio {VHFRadioDetails|callSign=callSign,mmsi=mmsi})
    CMEmail = (id,XCMEmail {EmailAccountDetails|emailAddress=emailAddress})
    CMP2000 = (id,XCMP2000 {P2000ReceiverDetails|capCode=capCode})

toNewCommunicationMean :: XCommunicationMean -> NewCommunicationMean
toNewCommunicationMean (XCMTelephone {TelephoneDetails|phoneNo})        = {NewCommunicationMean|defaultValue & type = CMPhone, phoneNo = phoneNo}
toNewCommunicationMean (XCMVHFRadio {VHFRadioDetails|callSign,mmsi})    = {NewCommunicationMean|defaultValue & type = CMVHF, callSign = callSign, mmsi = mmsi}
toNewCommunicationMean (XCMEmail {EmailAccountDetails|emailAddress})    = {NewCommunicationMean|defaultValue & type = CMEmail, emailAddress = emailAddress}
toNewCommunicationMean (XCMP2000 {P2000ReceiverDetails|capCode})        = {NewCommunicationMean|defaultValue & type = CMP2000, capCode = capCode}

fromNewCommunicationMean :: NewCommunicationMean -> XCommunicationMean
fromNewCommunicationMean {NewCommunicationMean|type=CMPhone,phoneNo}        = XCMTelephone {TelephoneDetails|phoneNo = phoneNo}
fromNewCommunicationMean {NewCommunicationMean|type=CMVHF,callSign,mmsi}    = XCMVHFRadio {VHFRadioDetails| callSign = callSign, mmsi = mmsi}
fromNewCommunicationMean {NewCommunicationMean|type=CMEmail,emailAddress}   = XCMEmail {EmailAccountDetails|emailAddress = emailAddress}
fromNewCommunicationMean {NewCommunicationMean|type=CMP2000,capCode}        = XCMP2000 {P2000ReceiverDetails|capCode = capCode}


