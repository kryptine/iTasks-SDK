definition module iTasks._Framework.Tonic.Server

import iTasks
from iTasks._Framework.Tonic.AbsSyn import :: ModuleName
from iTasks._Framework.Tonic.Types import :: TonicBookkeeping

:: ComputationId :== [Int]
:: NodeId        :== [Int]
:: FunctionName  :== String

:: TonicMessage =
  { computationId  :: ComputationId // Abstraction from TaskId
  , nodeId         :: NodeId
  , bpModuleName   :: ModuleName
  , bpFunctionName :: FunctionName
  , appModuleName  :: ModuleName
  , appFunName     :: FunctionName
  }

:: ServerState =
  { oldData  :: String
  , clientIp :: String
  }

derive class iTask TonicMessage, ServerState

playbackTonic :: Task ()

viewTonic :: Task ()

acceptAndViewTonicTraces :: Task (Bool, [TonicMessage])
