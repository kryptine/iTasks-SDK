definition module iTasks.Framework.Tonic

from iTasks.Framework.SDS import :: Shared, :: ReadWriteShared, :: RWShared
from iTasks.Framework.IWorld import :: IWorld
from iTasks.Framework.Engine import :: PublishedTask
import iTasks.Framework.Generic
from iTasks.API.Core.TaskCombinators import class tune
from iTasks.API.Core.Types import :: User
from iTasks.API.Core.Tasks import :: Task, :: InstanceNo
import iTasks.Framework.Tonic.AbsSyn
from iTasks.API.Extensions.Admin.WorkflowAdmin import :: Workflow
from System.Time import :: Timestamp
from Data.Map import :: Map
from Data.Set import :: Set
from Graphics.Scalable import :: Image, :: TagSource, :: TagRef, :: ImageTag

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

:: TaskAppRenderer :== Bool Bool ModuleName VarName [Image ModelTy] *TagSource -> *(!Maybe (Image ModelTy), !*TagSource)

:: ModelTy

tonicSharedRT :: Shared TonicRTMap

:: TonicRT =
  { trt_taskId        :: TaskId
  , trt_params        :: [(VarName, Task ())]
  , trt_bpref         :: (ModuleName, TaskName)
  , trt_bpinstance    :: Maybe TonicTask
  , trt_activeNodeId  :: Maybe [Int]
  , trt_parentTaskId  :: TaskId
  , trt_involvedUsers :: [User]
  , trt_output        :: Maybe (Task ())
  }

derive class iTask TonicRT

tDefaultTaskApp :: !Bool !Bool !ModuleName !VarName ![Image ModelTy] !*TagSource -> *(!Image ModelTy, !*TagSource)

tonicViewer :: [TaskAppRenderer] -> PublishedTask

tonicStaticBrowser :: [TaskAppRenderer] -> Task ()

tonicStaticWorkflow :: [TaskAppRenderer] -> Workflow

tonicDynamicBrowser :: [TaskAppRenderer] -> Task ()

tonicDynamicWorkflow :: [TaskAppRenderer] -> Workflow

tonicViewInformation :: !String !a -> Task () | iTask a

tonicWrapTaskBody :: !ModuleName TaskName [(VarName, Task ())] (Task a) -> Task a | iTask a

tonicWrapParallel :: ModuleName TaskName [Int] ([Task a] -> Task b) [Task a] -> Task b

tonicWrapApp :: ModuleName TaskName [Int] (Task a) -> Task a

tonicWrapAppLam1 :: ModuleName TaskName [Int] (a -> Task b) -> a -> Task b

tonicWrapAppLam2 :: ModuleName TaskName [Int] (a b -> Task c) -> a b -> Task c

tonicWrapAppLam3 :: ModuleName TaskName [Int] (a b c -> Task d) -> a b c -> Task d
