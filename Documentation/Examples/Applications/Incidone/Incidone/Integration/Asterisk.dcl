definition module Incidone.Integration.Asterisk
import iTasks
import Incidone.OP.Concepts

//Initiate an outgoing channel
initiateAsteriskChannel     :: CommunicationNo -> Task ()

//Destroy (hangup or cancel) a channel
destroyAsteriskChannel       :: CommunicationNo -> Task ()

//Connect with to call events on an asterisk system using the AMI protocol
syncAsteriskAMI             :: Task ()
