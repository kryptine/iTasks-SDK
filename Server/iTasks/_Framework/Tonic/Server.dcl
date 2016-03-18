definition module iTasks._Framework.Tonic.Server

import iTasks
import iTasks._Framework.Tonic.AbsSyn

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
