definition module iTasks.Framework.Tonic

from iTasks.Framework.SDS import :: Shared, :: ReadWriteShared, :: RWShared
from iTasks.Framework.IWorld import :: IWorld
from iTasks.Framework.Engine import :: PublishedTask
import iTasks.Framework.Generic
from iTasks.API.Core.TaskCombinators import class tune
from iTasks.API.Core.Types import :: User
from iTasks.API.Core.Tasks import :: Task, :: InstanceNo
import iTasks.Framework.Tonic.AbsSyn
from System.Time import :: Timestamp
from Data.Map import :: Map

derive gEditor
  TonicModule, TonicTask, TExpr, PPOr, TStepCont, TStepFilter, TUser,
  TParallel, TShare

derive gEditMeta
  TonicModule, TonicTask, TExpr, PPOr, TStepCont, TStepFilter, TUser,
  TParallel, TShare

derive gDefault
  TonicModule, TonicTask, TExpr, PPOr, TStepCont, TStepFilter, TUser,
  TParallel, TShare

derive gUpdate
  TonicModule, TonicTask, TExpr, PPOr, TStepCont, TStepFilter, TUser,
  TParallel, TShare

derive gVerify
  TonicModule, TonicTask, TExpr, PPOr, TStepCont, TStepFilter, TUser,
  TParallel, TShare

derive gText
  TonicModule, TonicTask, TExpr, PPOr, TStepCont, TStepFilter, TUser,
  TParallel, TShare

:: TonicRTMap :== Map TaskId TonicRT

tonicSharedRT :: Shared TonicRTMap

:: TonicRT =
  { trt_taskId       :: TaskId
  , trt_params       :: [(VarName, Task ())]
  , trt_bpref        :: (ModuleName, TaskName)
  , trt_bpinstance   :: Maybe TonicTask
  , trt_activeNodeId :: Maybe [Int]
  , trt_parentTaskId :: TaskId
  , trt_output       :: Maybe (Task ())
  }

derive class iTask TonicRT

tonicViewer :: String -> PublishedTask

tonicViewInformation :: !String !a -> Task () | iTask a

tonicWrapTaskBody :: !ModuleName TaskName [(VarName, Task ())] (Task a) -> Task a | iTask a

tonicWrapListOfTask :: ModuleName TaskName [Int] [Task a] -> [Task a]

tonicWrapApp :: ModuleName TaskName [Int] (Task a) -> Task a

tonicWrapAppLam1 :: ModuleName TaskName [Int] (a -> Task b) -> a -> Task b

tonicWrapAppLam2 :: ModuleName TaskName [Int] (a b -> Task c) -> a b -> Task c

tonicWrapAppLam3 :: ModuleName TaskName [Int] (a b c -> Task d) -> a b c -> Task d
