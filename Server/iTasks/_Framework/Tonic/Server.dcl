definition module iTasks._Framework.Tonic.Server

import iTasks
from iTasks._Framework.Tonic.AbsSyn import :: ModuleName
from iTasks._Framework.Tonic.Types import :: TonicMessage

:: ServerState =
  { oldData  :: String
  , clientIp :: String
  }

derive class iTask ServerState

playbackTonic :: Task ()

viewTonic :: Task ()

acceptAndViewTonicTraces :: Task (Bool, [TonicMessage])
