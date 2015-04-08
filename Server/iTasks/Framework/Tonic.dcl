definition module iTasks.Framework.Tonic

from iTasks.Framework.SDS import :: Shared, :: ReadWriteShared, :: RWShared
from iTasks.Framework.IWorld import :: IWorld, :: SystemClocks
from iTasks.Framework.Engine import :: PublishedTask
import iTasks.Framework.Generic
from iTasks.Framework.Task import :: TaskEvalOpts
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


:: TaskAppRenderer :== Bool Bool Bool Bool ModuleName VarName [Image ModelTy] *TagSource -> *(!Maybe (Image ModelTy), !*TagSource)

:: ModelTy
:: NodeId :== [Int]

tDefaultTaskApp       :: !Bool !Bool !Bool !Bool !ModuleName !VarName ![TExpr] ![Image ModelTy] !*TagSource -> *(!Image ModelTy, !*TagSource)

tonicStaticBrowser    :: [TaskAppRenderer] -> Task ()

tonicStaticWorkflow   :: [TaskAppRenderer] -> Workflow

tonicDynamicBrowser   :: [TaskAppRenderer] -> Task ()

tonicDynamicWorkflow  :: [TaskAppRenderer] -> Workflow

tonicViewInformation  :: !String !a -> Task () | iTask a

tonicWrapTaskBody     :: !ModuleName !TaskName [(VarName, Task ())] (         Task a) -> Task a | iTask a

tonicWrapTaskBodyLam1 :: !ModuleName !TaskName [(VarName, Task ())] (b     -> Task a) -> b     -> Task a | iTask a

tonicWrapTaskBodyLam2 :: !ModuleName !TaskName [(VarName, Task ())] (b c   -> Task a) -> b c   -> Task a | iTask a

tonicWrapTaskBodyLam3 :: !ModuleName !TaskName [(VarName, Task ())] (b c d -> Task a) -> b c d -> Task a | iTask a

tonicWrapParallel     :: !ModuleName !TaskName !NodeId !([Task a] -> Task b) [Task a] -> Task b | iTask b

tonicWrapApp          :: !ModuleName !TaskName !NodeId (Task a) -> Task a | iTask a

tonicWrapAppLam1      :: !ModuleName !TaskName !NodeId !(a -> Task b) -> a -> Task b | iTask b

tonicWrapAppLam2      :: !ModuleName !TaskName !NodeId !(a b -> Task c) -> a b -> Task c | iTask c

tonicWrapAppLam3      :: !ModuleName !TaskName !NodeId !(a b c -> Task d) -> a b c -> Task d | iTask d
