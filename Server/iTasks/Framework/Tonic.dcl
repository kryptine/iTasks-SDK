definition module iTasks.Framework.Tonic

from iTasks.Framework.SDS import :: Shared, :: ReadWriteShared, :: RWShared
from iTasks.Framework.IWorld import :: IWorld, :: SystemClocks
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

:: TonicRTMap :== Map TaskId BlueprintRef

:: TaskAppRenderer :== Bool Bool ModuleName VarName [Image ModelTy] *TagSource -> *(!Maybe (Image ModelTy), !*TagSource)

:: ModelTy

:: BlueprintRef =
  { bpr_moduleName :: !ModuleName
  , bpr_taskName   :: !TaskName
  , bpr_blueprint  :: !TonicTask
  , bpr_instance   :: !Maybe BlueprintInstance
  }

:: BlueprintInstance =
  { bpi_taskId        :: !TaskId
  , bpi_startTime     :: !SystemClocks
  , bpi_endTime       :: !Maybe SystemClocks
  , bpi_params        :: ![(!VarName, !Task ())]
  , bpi_activeNodeId  :: !Maybe [Int]
  , bpi_parentTaskId  :: !Maybe TaskId
  , bpi_involvedUsers :: ![User]
  , bpi_output        :: !Maybe (Task ())
  }

derive class iTask BlueprintRef, BlueprintInstance

tDefaultTaskApp :: !Bool !Bool !ModuleName !VarName ![TExpr] ![Image ModelTy] !*TagSource -> *(!Image ModelTy, !*TagSource)

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
