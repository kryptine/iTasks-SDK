definition module Incidone.Integration.AIS
import iTasks, Message.Encodings.AIS

//Connect to an external AIS Server that streams AIVDM messages
syncAISStream       :: Task ()
updateAISContact    :: AIVDM -> Task ()
injectAISUpdates    :: String [String] -> Task ()
