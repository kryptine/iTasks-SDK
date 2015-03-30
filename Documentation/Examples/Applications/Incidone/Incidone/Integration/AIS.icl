implementation module Incidone.Integration.AIS
import iTasks
import Incidone.Util.AIS
import Incidone.Configuration
import Incidone.OP.SDSs, Incidone.OP.Conversions
import Incidone.Util.TaskPatterns
import qualified Data.Map as DM

syncAISStream :: Task ()
syncAISStream = withShared ([],False,[],False) (\channel -> (sync channel -&&- consume channel) @! ())
where
    sync channel
        =   get aisLinkConfig
        >>- \{AISConfig|host,port} ->
            syncNetworkChannel host port "\r\n" id id channel

    consume channel
        =   consumeNetworkStream updateAISContacts channel

updateAISContacts :: [String] -> Task ()
updateAISContacts sentences
    # (msgs,remainder) = decodeAIVDM sentences
    = allTasks [updateAISContact msg \\ msg <- msgs] <<@ NoUserInterface
    @! ()

updateAISContact :: AIVDM -> Task ()
updateAISContact msg = case msg of
    (AIVDM1 cnb)	= updatePosition cnb
    (AIVDM3 cnb)	= updatePosition cnb
    (AIVDM5 info)	= updateInfo info
    _               = return ()
where
	updatePosition cnb=:{AIVDMCNB|mmsi}
        =   get currentDateTime
        >>- \now ->
		    upd (updA now) (sdsFocus mmsi AISContactByMMSI) -&&- upd (updC now) (sdsFocus mmsi contactByMMSI) @! ()
	where
		updA now mbContact
            # contact = {AISContact|fromMaybe defaultValue mbContact & lastPositionMsg = Just cnb}
            = Just (updAISContactPosition now (aisPosition cnb) (aisHeading cnb) contact)
        updC now mbContact
            = fmap (\c -> updContactPosition now (Just (aisPosition cnb)) (Just (aisHeading cnb)) c) mbContact

	updateInfo info=:{AIVDM5|mmsi}
		= upd update (sdsFocus mmsi AISContactByMMSI) @! ()
	where
		update mbContact = Just {AISContact|fromMaybe defaultValue mbContact & lastInfoMsg = Just info}

injectAISUpdates :: String [String] -> Task ()
injectAISUpdates reference msgs
    # (msgs,remainder) = decodeAIVDM msgs
	=   allTasks [updateAISContact msg \\ msg <- msgs] <<@ NoUserInterface @ length
    >>- \numImported ->
        get currentDateTime
    >>- \now ->
        set (Just (now,reference, numImported)) lastAISImport
    @!  ()
