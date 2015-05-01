definition module iTasks._Framework.Tonic.Types

import StdString
import StdOverloaded
from Data.IntMap.Strict import :: IntMap
import iTasks._Framework.Tonic.AbsSyn
import iTasks._Framework.Generic
from iTasks.API.Core.Types import :: DateTime

:: ListsOfTasks :== Map (TaskId, ExprId) (IntMap (ModuleName, TaskName))

:: ListId :== TaskId

:: TStability = TNoVal | TStable | TUnstable

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
  }

:: TonicRTMap :== Map TaskId BlueprintRef

:: InstanceTrace :== [Int]

:: Calltrace :== [TaskId]


derive class iTask TStability, BlueprintRef, BlueprintInstance

derive gEditor
  TonicModule, TonicTask, TExpr, PPOr, TStepCont, TStepFilter, TUser,
  TParallel, IntMap, TCleanExpr, TAssoc, TGen

derive gEditMeta
  TonicModule, TonicTask, TExpr, PPOr, TStepCont, TStepFilter, TUser,
  TParallel, IntMap, TCleanExpr, TAssoc, TGen

derive gDefault
  TonicModule, TonicTask, TExpr, PPOr, TStepCont, TStepFilter, TUser,
  TParallel, IntMap, TCleanExpr, TAssoc, TGen

derive gUpdate
  TonicModule, TonicTask, TExpr, PPOr, TStepCont, TStepFilter, TUser,
  TParallel, IntMap, TCleanExpr, TAssoc, TGen

derive gVerify
  TonicModule, TonicTask, TExpr, PPOr, TStepCont, TStepFilter, TUser,
  TParallel, IntMap, TCleanExpr, TAssoc, TGen

derive gText
  TonicModule, TonicTask, TExpr, PPOr, TStepCont, TStepFilter, TUser,
  TParallel, IntMap, TCleanExpr, TAssoc, TGen