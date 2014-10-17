definition module Incidone.OP.ConceptsTOP
/**
* This module provides the iTasks implementation for the
* types defined in the OP concepts module.
*/
import iTasks
import Incidone.OP.Concepts, Incidone.OP.Conversions

derive class iTask DSCMessage, DSCCategory, INMARSATCMessage, PhoneCall, RadioCall, EmailMessage, P2000Message
derive class iTask Communication, CommunicationDetails, CommunicationType, CommunicationDirection, CommunicationStatus
derive class iTask EmergencyPhase, Incident, IncidentType, Contact, ContactType, ContactStatus, ContactAccessLevel, ContactPhoto, ContactAvatar, VesselType
derive class iTask PersonDetails, VesselDetails, SurferDetails, DiverDetails, AirplaneDetails, HelicopterDetails
derive class iTask WeatherData, WeatherType,LogEntry, Gender
derive class iTask AISContact, AIVDMCNB, AIVDM5
derive class iTask CommunicationMean, CommunicationMeanType, TelephoneDetails, VHFRadioDetails, EmailAccountDetails, P2000ReceiverDetails, XCommunicationMean
derive class iTask NewContact, NewIncident, NewCommunicationMean
derive class iTask AISDetails
derive class iTask IncidentDetails, IncidentBasic, IncidentOverview, ContactDetails, ContactBasic, ContactGeo, ContactShortWithIncidents, ContactNameTypePosition, ContactAccess

derive JSONEncode       Temperature, Meters, Feet, Miles, Knots, Degrees, ContactShort, IncidentShort
derive JSONDecode       Temperature, Meters, Feet, Miles, Knots, Degrees, ContactShort, IncidentShort
derive gText            Temperature, Meters, Feet, Miles, Knots, Degrees, ContactShort, IncidentShort
derive gEditor          Temperature, Meters, Feet, Miles, Knots, Degrees, ContactShort, IncidentShort
derive gEditMeta        Temperature, Meters, Feet, Miles, Knots, Degrees, ContactShort, IncidentShort
derive gUpdate          Temperature, Meters, Feet, Miles, Knots, Degrees, ContactShort, IncidentShort
derive gVerify          Temperature, Meters, Feet, Miles, Knots, Degrees, ContactShort, IncidentShort
derive gDefault         Temperature, Meters, Feet, Miles, Knots, Degrees, ContactShort, IncidentShort
derive gEq              Temperature, Meters, Feet, Miles, Knots, Degrees, ContactShort, IncidentShort

