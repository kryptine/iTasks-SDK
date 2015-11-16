implementation module iTasks._Framework.Tonic.Server

import iTasks
from Text import class Text, instance Text String
import qualified Text as T

derive class iTask TonicMessage, ServerState

debugMsg str = { TonicMessage
               | computationId = []
               , nodeId        = []
               , moduleName    = "DEBUG"
               , functionName  = str
               }

tonicServerShare :: Shared [TonicMessage]
tonicServerShare = sharedStore "tonicServerShare" []

acceptAndViewTonicTraces :: Task [TonicMessage]
acceptAndViewTonicTraces
  = acceptTonicTraces tonicServerShare
      ||-
    viewSharedInformation "Logged traces" [] tonicServerShare

:: ServerState =
  { oldData  :: String
  , clientIp :: String
  }

acceptTonicTraces :: !(RWShared () [TonicMessage] [TonicMessage]) -> Task [ServerState]
acceptTonicTraces tonicShare
  = tcplisten 9000 True tonicShare { ConnectionHandlers
                                   | onConnect      = onConnect
                                   , whileConnected = whileConnected
                                   , onDisconnect   = onDisconnect
                                   }
  where
  onConnect :: String [TonicMessage] -> (MaybeErrorString ServerState, Maybe [TonicMessage], [String], Bool)
  onConnect host olderMessages
    = ( Ok { oldData = ""
           , clientIp = host}
      , Just [debugMsg ("Connection from " +++ host) : olderMessages]
      , ["Welcome!"]
      , False)

  whileConnected :: (Maybe String) ServerState [TonicMessage] -> (MaybeErrorString ServerState, Maybe [TonicMessage], [String], Bool)
  whileConnected (Just newData) st=:{oldData} olderMessages
    # collectedData        = oldData +++ 'T'.trim newData
    # (messages, leftover) = partitionMessages ('T'.split "TONIC_EOL" collectedData)
    # mbTMsgs              = case [msg \\ Just msg <- map (fromJSON o fromString) messages] of
                               [] -> Nothing
                               xs -> Just (xs ++ olderMessages)
    = (Ok {st & oldData = leftover}, mbTMsgs, [], False)
    where
    partitionMessages :: [String] -> ([String], String)
    partitionMessages []  = ([], "")
    partitionMessages [x] = ([], x)
    partitionMessages [x:y:xs]
      # (msgs, leftover) = partitionMessages [y:xs]
      = ([x:msgs], leftover)

  whileConnected Nothing st olderMessages
    = (Ok st, Nothing, [], False)

  onDisconnect :: ServerState [TonicMessage] -> (MaybeErrorString ServerState, Maybe [TonicMessage])
  onDisconnect st lines
    = (Ok st, Just [debugMsg "Disconnect" : lines])


