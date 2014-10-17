definition module Incidone.Integration.AIS
import iTasks, Incidone.Util.AIS

//Connect to an external AIS Server that streams AIVDM messages
syncAISStream       :: Task ()
updateAISContact    :: AIVDM -> Task ()
injectAISUpdates    :: String [String] -> Task ()
