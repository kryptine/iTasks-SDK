definition module iTasks._Framework.Tonic.Server

import iTasks

:: ComputationId :== [Int]
:: NodeId        :== [Int]
:: ModuleName    :== String
:: FunctionName  :== String

:: TonicMessage =
  { computationId :: ComputationId // Abstraction from TaskId
  , nodeId        :: NodeId
  , moduleName    :: ModuleName
  , functionName  :: FunctionName
  }

derive class iTask TonicMessage

