definition module iTasks._Framework.Tonic.Types

import StdString
import StdOverloaded
from Data.IntMap.Strict import :: IntMap
import iTasks._Framework.Tonic.AbsSyn
import iTasks._Framework.Generic
import iTasks.API.Extensions.User
from iTasks.API.Core.Types import :: DateTime

:: ListId :== TaskId

:: BlueprintRef =
  { bpr_moduleName :: !ModuleName
  , bpr_taskName   :: !TaskName
  , bpr_instance   :: !Maybe BlueprintInstance
  }

:: BlueprintInstance =
  { bpi_taskId           :: !TaskId
  , bpi_startTime        :: !DateTime
  , bpi_lastUpdated      :: !DateTime
  , bpi_endTime          :: !Maybe DateTime
  , bpi_activeNodes      :: !Map ListId (IntMap (TaskId, ExprId))
  , bpi_previouslyActive :: !Map ExprId TaskId
  , bpi_parentTaskId     :: !Maybe TaskId
  , bpi_currentUser      :: !Maybe User
  , bpi_blueprint        :: !TonicTask
  , bpi_nodeTaskIdMap    :: Map Int TaskId
  }

:: TonicRTMap :== Map TaskId BlueprintRef

:: Calltrace :== [TaskId]

:: TStability = TNoVal | TStable | TUnstable

derive class iTask TStability, BlueprintRef, BlueprintInstance

derive gEditor
  TonicModule, TonicTask, TExpr, TPriority, TAssoc, IntMap

derive gEditMeta
  TonicModule, TonicTask, TExpr, TPriority, TAssoc, IntMap

derive gDefault
  TonicModule, TonicTask, TExpr, TPriority, TAssoc, IntMap

derive gUpdate
  TonicModule, TonicTask, TExpr, TPriority, TAssoc, IntMap

derive gVerify
  TonicModule, TonicTask, TExpr, TPriority, TAssoc, IntMap

derive gText
  TonicModule, TonicTask, TExpr, TPriority, TAssoc, IntMap
