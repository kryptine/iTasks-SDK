implementation module Incidone.OP.ConceptsTOP
import iTasks, iTasks.UI.Editor
import Incidone.OP.Concepts

derive class iTask DSCMessage, DSCCategory, INMARSATCMessage, PhoneCall, RadioCall, EmailMessage, P2000Message
derive class iTask Communication, CommunicationDetails, CommunicationType, CommunicationDirection, CommunicationStatus
derive class iTask EmergencyPhase, Incident, IncidentType, Contact, ContactType, ContactAccessLevel, ContactStatus, ContactPhoto, ContactAvatar, VesselType
derive class iTask PersonDetails, VesselDetails, SurferDetails, DiverDetails, AirplaneDetails, HelicopterDetails
derive class iTask WeatherData, WeatherType,LogEntry, Gender
derive class iTask AISContact, AIVDMCNB, AIVDM5
derive class iTask CommunicationMean, CommunicationMeanType, TelephoneDetails, VHFRadioDetails, EmailAccountDetails, P2000ReceiverDetails, XCommunicationMean

derive class iTask NewContact, NewIncident, NewCommunicationMean

derive class iTask IncidentDetails, IncidentBasic, IncidentOverview, ContactDetails, ContactBasic,ContactGeo, ContactShortWithIncidents, ContactNameTypePosition, ContactAccess
derive class iTask AISDetails

gText{|Temperature|} _     (Just (Temperature t)) = [toString t +++ " C"]
gText{|Temperature|} _     _                      = [""]
gText{|Meters|} _          (Just (Meters m))  = [toString m +++ " m"]
gText{|Feet|} _            (Just (Feet f))    = [toString f +++ " ft"]
gText{|Miles|} _           (Just (Miles m))   = [toString m +++ " nm"]
gText{|Knots|} _           (Just (Knots k))   = [toString k +++ " knots"]
gText{|Degrees|} _         (Just (Degrees d)) = [toString d +++ " deg"]

gText{|ContactShort|} _    c = [maybe "" contactTitle c]
gText{|IncidentShort|} _   i = [maybe "" incidentTitle i]

derive JSONEncode   Temperature, Meters, Feet, Miles, Knots, Degrees, ContactShort, IncidentShort
derive JSONDecode   Temperature, Meters, Feet, Miles, Knots, Degrees, ContactShort, IncidentShort
derive gEditor      Temperature, Meters, Feet, Miles, Knots, Degrees, ContactShort, IncidentShort
derive gDefault     Temperature, Meters, Feet, Miles, Knots, Degrees, ContactShort, IncidentShort
derive gEq          Temperature, Meters, Feet, Miles, Knots, Degrees, ContactShort, IncidentShort

