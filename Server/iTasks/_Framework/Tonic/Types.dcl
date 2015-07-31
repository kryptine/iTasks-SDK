definition module iTasks._Framework.Tonic.Types

import StdString
import StdOverloaded
from Data.IntMap.Strict import :: IntMap
import iTasks._Framework.Tonic.AbsSyn
import iTasks._Framework.Generic
import iTasks.API.Extensions.User
from iTasks.API.Core.Types import :: DateTime

:: ListId :== TaskId

:: BlueprintIdent =
  { bpr_moduleName :: !ModuleName
  , bpr_taskName   :: !FuncName
  }

:: BlueprintInstance =
  { bpi_taskId           :: !TaskId
  , bpi_startTime        :: !DateTime
  , bpi_lastUpdated      :: !DateTime
  , bpi_endTime          :: !Maybe DateTime
  , bpi_activeNodes      :: !Map ListId (IntMap (TaskId, ExprId))
  , bpi_previouslyActive :: !Map ExprId TaskId
  , bpi_parentTaskId     :: !TaskId
  , bpi_currentUser      :: !Maybe User
  , bpi_blueprint        :: !TonicFunc
  , bpi_case_branches    :: !Map ExprId Int
  , bpi_bpref            :: !BlueprintIdent
  }

:: TonicRTMap :== Map TaskId BlueprintInstance

:: Calltrace :== CircularStack TaskId

:: TStability = TNoVal | TStable | TUnstable

derive class iTask TStability, BlueprintIdent, BlueprintInstance

derive gEditor
  TonicModule, TonicFunc, TExpr, TPriority, TAssoc, IntMap, TLit

derive gEditMeta
  TonicModule, TonicFunc, TExpr, TPriority, TAssoc, IntMap, TLit

derive gDefault
  TonicModule, TonicFunc, TExpr, TPriority, TAssoc, IntMap, TLit

derive gUpdate
  TonicModule, TonicFunc, TExpr, TPriority, TAssoc, IntMap, TLit

derive gVerify
  TonicModule, TonicFunc, TExpr, TPriority, TAssoc, IntMap, TLit

derive gText
  TonicModule, TonicFunc, TExpr, TPriority, TAssoc, IntMap, TLit
