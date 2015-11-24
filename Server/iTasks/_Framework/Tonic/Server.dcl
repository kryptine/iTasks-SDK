definition module iTasks._Framework.Tonic.Server

import iTasks
import iTasks._Framework.Tonic.AbsSyn

:: ComputationId :== [Int]
:: NodeId        :== [Int]
:: FunctionName  :== String

:: TonicMessage =
  { computationId :: ComputationId // Abstraction from TaskId
  , nodeId        :: NodeId
  , moduleName    :: ModuleName
  , functionName  :: FunctionName
  }

:: ServerState =
  { oldData  :: String
  , clientIp :: String
  }

derive class iTask TonicMessage, ServerState

viewTonic :: Task ()

acceptAndViewTonicTraces :: Task [TonicMessage]
