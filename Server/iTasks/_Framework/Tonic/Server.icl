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

acceptTonicTraces :: !(RWShared () [TonicMessage] [TonicMessage]) -> Task [()]
acceptTonicTraces log
  = tcplisten 9000 True log { ConnectionHandlers
                            | onConnect = onConnect
                            , whileConnected = whileConnected
                            , onDisconnect
                            }
  where
  onConnect :: String [TonicMessage] -> (MaybeErrorString (), Maybe [TonicMessage], [String], Bool)
  onConnect host lines
    = (Ok (), Just [debugMsg ("Connection from " +++ host) : lines], ["Welcome!"], False)

  whileConnected :: (Maybe String) () [TonicMessage] -> (MaybeErrorString (), Maybe [TonicMessage], [String], Bool)
  whileConnected (Just data) _ lines
    = case 'T'.split "\n" data of
        [msgJSONString : restData : _]
          = case fromJSON (fromString msgJSONString) of
              Just msg = (Ok (), Just [msg : lines], [], False)
              _        = (Ok (), Just lines, [], False)
        _ = (Ok (), Just lines, [], False)

  whileConnected Nothing _ lines
    = (Ok (), Nothing, [], False)

  onDisconnect :: () [TonicMessage] -> (MaybeErrorString (), Maybe [TonicMessage])
  onDisconnect () lines
    = (Ok (), Just [debugMsg "Disconnect" : lines])
