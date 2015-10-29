implementation module iTasks._Framework.Tonic.Server

import iTasks
from Text import class Text, instance Text String
import qualified Text as T

derive class iTask TonicMessage

debugMsg str = { TonicMessage
               | computationId = []
               , nodeId = []
               , moduleName = "DEBUG"
               , functionName = str
               }

acceptAndViewTonicTraces :: Task [TonicMessage]
acceptAndViewTonicTraces
  = withShared []
      (\log -> acceptTonicTraces log
                 ||-
               viewSharedInformation "Logged traces" [] log)

acceptTonicTraces :: !(RWShared () [TonicMessage] [TonicMessage]) -> Task [String]
acceptTonicTraces log
  = tcplisten 9000 True log { ConnectionHandlers
                            | onConnect      = onConnect
                            , whileConnected = whileConnected
                            , onDisconnect   = onDisconnect
                            }
  where
  onConnect :: String [TonicMessage] -> (MaybeErrorString String, Maybe [TonicMessage], [String], Bool)
  onConnect host olderMessages
    = (Ok "", Just [debugMsg ("Connection from " +++ host) : olderMessages], ["Welcome!"], False)

  whileConnected :: (Maybe String) String [TonicMessage] -> (MaybeErrorString String, Maybe [TonicMessage], [String], Bool)
  whileConnected (Just newData) oldData olderMessages
    # collectedData        = oldData +++ 'T'.trim newData
    # (messages, leftover) = partitionMessages ('T'.split "TONIC_EOL" collectedData)
    # mbTMsgs              = case [msg \\ Just msg <- map (fromJSON o fromString) messages] of
                               [] -> Nothing
                               xs -> Just (xs ++ olderMessages)
    = (Ok leftover, mbTMsgs, [], False)
    where
    partitionMessages []  = ([], "")
    partitionMessages [x] = ([], x)
    partitionMessages [x:y:xs]
      # (msgs, leftover) = partitionMessages [y:xs]
      = ([x:msgs], leftover)

  whileConnected Nothing oldData olderMessages
    = (Ok oldData, Nothing, [], False)

  onDisconnect :: String [TonicMessage] -> (MaybeErrorString String, Maybe [TonicMessage])
  onDisconnect _ lines
    = (Ok "", Just [debugMsg "Disconnect" : lines])
