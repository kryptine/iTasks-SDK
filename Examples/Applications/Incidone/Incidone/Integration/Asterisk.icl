implementation module Incidone.Integration.Asterisk

import iTasks
import Incidone.Configuration
import Incidone.OP.Conversions
import Incidone.OP.SDSs
import Incidone.OP.CommunicationManagementTasks
import Incidone.Util.TaskPatterns
import Text
import qualified Data.Map as DM

:: AsteriskEvent :== Map String String // Simple key/value mapping

syncAsteriskAMI :: Task ()
syncAsteriskAMI = withShared ([],False,[],False) (\channel -> (sync channel -&&- consume channel) @! ())
where
    sync channel
        =   get asteriskLinkConfig
        >>- \{AsteriskConfig|host,port,username,password} ->
            set ([],False,[authEvent username password],False) channel
        >>| syncNetworkChannel host port "\r\n\r\n" decodeAsteriskEvent encodeAsteriskEvent channel

    consume channel
        =   consumeNetworkStream processEvents channel

    processEvents events
        = allTasks [processEvent m \\ m <- events | isAsteriskEventType ["Dial","Newstate","Hangup"] m] @! ()

    processEvent event
        = case ('DM'.get "Event" event) of
            (Just "Dial") = case 'DM'.get "SubEvent" event of
                (Just "Begin")  = reportPhoneCallBegin CallerIDNum UniqueID @! ()
                (Just "End")    = maybe (return ()) (\id -> reportPhoneCallEnd (Right id)) UniqueID
                _               = return ()
            (Just "Newstate") = case 'DM'.get "ChannelStateDesc" event of
                (Just "Up")     = maybe (return ()) (\id -> reportPhoneCallConnected (Right id)) Uniqueid
                _               = return ()
            _
                = return ()
    where
        CallerIDNum = 'DM'.get "CallerIDNum" event
        UniqueID    = 'DM'.get "UniqueID" event
        Uniqueid    = 'DM'.get "Uniqueid" event

    authEvent username password
        = 'DM'.fromList [("Action","Login"),("Username",username),("Secret",password)]

    originateEvent fromExt toExt context
        = 'DM'.fromList [("Action","Originate"),("Channel","SIP/"+++fromExt),("Exten",toExt),("Priority","1"),("Context",context)]

encodeAsteriskEvent :: AsteriskEvent -> String
encodeAsteriskEvent event = join "\r\n" [k +++ ": " +++ v \\ (k,v) <- 'DM'.toList event] +++ "\r\n\r\n"

decodeAsteriskEvent :: String -> AsteriskEvent
decodeAsteriskEvent enc = foldr addLine 'DM'.newMap (split "\r\n" enc)
where
    addLine line msg = case split ":" line of
        [key,value] = 'DM'.put (trim key) (trim value) msg
        _           = msg

isAsteriskEventType :: [String] AsteriskEvent -> Bool
isAsteriskEventType types msg =  maybe False (\type -> isMember type types) ('DM'.get "Event" msg)

initiateAsteriskChannel :: CommunicationNo -> Task ()
initiateAsteriskChannel communicationNo
    //Set status to pending
    =   upd (\c -> {Communication|c & status = Just Pending}) (sdsFocus communicationNo communicationByNo)
    @!  ()

destroyAsteriskChannel :: CommunicationNo -> Task ()
destroyAsteriskChannel communicationNo
    =   upd (\c -> {Communication|c & status = Nothing}) (sdsFocus communicationNo communicationByNo)
    @!  ()


