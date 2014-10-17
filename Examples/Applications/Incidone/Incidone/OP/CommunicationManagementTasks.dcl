definition module Incidone.OP.CommunicationManagementTasks
import iTasks
import Incidone.OP.Concepts, Incidone.Util.Workspace

//Create a new communication record in the system
createCommunication		:: CommunicationType CommunicationDirection (Maybe ContactNo) -> Task CommunicationNo

//Update a communication and type specific details
updateCommunication     :: CommunicationNo -> Task CommunicationNo

//Answer an incoming call
answerPhoneCall         :: CommunicationNo -> Task CommunicationNo
//Initiate an outgoing phone call
initiatePhoneCall       :: CommunicationNo -> Task CommunicationNo
//Answer an incoming radio call
answerRadioCall         :: CommunicationNo -> Task CommunicationNo
//Initiate an outgoing radio call
initiateRadioCall       :: CommunicationNo -> Task CommunicationNo
//Compose an outgoing e-mail message
composeEmailMessage     :: CommunicationNo -> Task CommunicationNo
//Compose an outgoing P2000 message
composeP2000Message     :: CommunicationNo -> Task CommunicationNo


//Send an outbound e-mail message
transmitEmailMessage :: CommunicationNo -> Task ()
//Send an outbound p2000 message
transmitP2000Message :: CommunicationNo -> Task ()

linkIncidentsToCommunication :: [IncidentNo] CommunicationNo -> Task CommunicationNo

//For integration
reportPhoneCallBegin        :: (Maybe String) (Maybe String)    -> Task CommunicationNo
reportPhoneCallConnected    :: (Either CommunicationNo String)  -> Task ()
reportPhoneCallEnd          :: (Either CommunicationNo String)  -> Task ()

injectEmail :: EmailMessage -> Task CommunicationNo
