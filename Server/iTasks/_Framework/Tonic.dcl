definition module iTasks._Framework.Tonic

from iTasks._Framework.SDS import :: Shared, :: ReadWriteShared, :: RWShared
from iTasks._Framework.IWorld import :: IWorld, :: SystemClocks
from iTasks._Framework.Engine import :: PublishedTask
import iTasks._Framework.Generic
from iTasks._Framework.Task import :: TaskEvalOpts
from iTasks.API.Core.TaskCombinators import class tune
from iTasks.API.Core.Tasks import :: Task, :: InstanceNo
import iTasks._Framework.Tonic.AbsSyn
import iTasks._Framework.Tonic.Images
from iTasks.API.Extensions.Admin.WorkflowAdmin import :: Workflow
from System.Time import :: Timestamp
from Data.Map import :: Map
from Data.Set import :: Set
from Graphics.Scalable import :: Image, :: TagSource, :: TagRef, :: ImageTag
from iTasks.API.Core.Types import class TMonad, class TApplicative, class TFunctor

// For all of these classes goes that the iTask context restriction shouldn't
// be there. Ideally, we would have something like associated type families
// and constraintkinds to determine the context restriction per monad.
class TonicTopLevelBlueprint m | TMonad m where
  tonicWrapTopLevelBody :: !ModuleName !TaskName [(VarName, m ())] (m a) -> m a | iTask a

class TonicBlueprintPart m | TMonad m where
  tonicWrapPartApp :: !ModuleName !TaskName !ExprId (m a) -> m a | iTask a

instance TonicTopLevelBlueprint Task
instance TonicBlueprintPart Task
instance TonicBlueprintPart Maybe

tonicStaticBrowser    :: [TaskAppRenderer] -> Task ()

tonicStaticWorkflow   :: [TaskAppRenderer] -> Workflow

tonicDynamicBrowser   :: [TaskAppRenderer] -> Task ()

tonicDynamicWorkflow  :: [TaskAppRenderer] -> Workflow

tonicViewInformation  :: !String !a -> Task () | iTask a

tonicWrapTaskBody     :: !ModuleName !TaskName [(VarName, m ())] (         m a)          -> m a | TonicTopLevelBlueprint m & iTask a

tonicWrapTaskBodyLam1 :: !ModuleName !TaskName [(VarName, m ())] (b     -> m a) -> b     -> m a | TonicTopLevelBlueprint m & iTask a

tonicWrapTaskBodyLam2 :: !ModuleName !TaskName [(VarName, m ())] (b c   -> m a) -> b c   -> m a | TonicTopLevelBlueprint m & iTask a

tonicWrapTaskBodyLam3 :: !ModuleName !TaskName [(VarName, m ())] (b c d -> m a) -> b c d -> m a | TonicTopLevelBlueprint m & iTask a

tonicWrapApp          :: !ModuleName !TaskName !ExprId (          m a)          -> m a | TonicBlueprintPart m & iTask a

tonicWrapAppLam1      :: !ModuleName !TaskName !ExprId !(b     -> m a) -> b     -> m a | TonicBlueprintPart m & iTask a

tonicWrapAppLam2      :: !ModuleName !TaskName !ExprId !(b c   -> m a) -> b c   -> m a | TonicBlueprintPart m & iTask a

tonicWrapAppLam3      :: !ModuleName !TaskName !ExprId !(b c d -> m a) -> b c d -> m a | TonicBlueprintPart m & iTask a

tonicWrapParallel     :: !ModuleName !TaskName !ExprId !([Task a] -> Task b) [Task a] -> Task b | iTask b