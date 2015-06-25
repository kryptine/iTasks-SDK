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
from Data.Functor import class Functor
from Data.Foldable import class Foldable
from Data.Traversable import class Traversable

// For all of these classes goes that the iTask context restriction shouldn't
// be there. Ideally, we would have something like associated type families
// and constraintkinds to determine the context restriction per monad.
class TonicTopLevelBlueprint m | TMonad m where
  tonicWrapBody :: !ModuleName !TaskName [(VarName, m ())] (m a) -> m a | iTask a
  tonicWrapArg  :: !String a -> m () | iTask a

class TonicBlueprintPart m | TMonad m where
  tonicWrapApp         :: !(!ModuleName, !TaskName) !(!ModuleName, !TaskName) !(!ModuleName, !TaskName) !ExprId !ExprId (m a) -> m a | iTask a
  tonicWrapTraversable :: !(!ModuleName, !TaskName) !(!ModuleName, !TaskName) !(!ModuleName, !TaskName) !ExprId !([m a] -> m b) [m a] -> m b | iTask b // TODO Generalise

instance TonicTopLevelBlueprint Task
instance TonicBlueprintPart Task
instance TonicBlueprintPart Maybe

tonicStaticBrowser      :: [TaskAppRenderer] -> Task ()

tonicStaticWorkflow     :: [TaskAppRenderer] -> Workflow

tonicDynamicBrowser     :: [TaskAppRenderer] -> Task ()

tonicDynamicWorkflow    :: [TaskAppRenderer] -> Workflow

tonicExtWrapArg         :: !String !a -> m () | iTask a & TonicTopLevelBlueprint m

tonicExtWrapBody        :: !ModuleName !TaskName [(VarName, m ())] (         m a)          -> m a | TonicTopLevelBlueprint m & iTask a

tonicExtWrapBodyLam1    :: !ModuleName !TaskName [(VarName, m ())] (b     -> m a) -> b     -> m a | TonicTopLevelBlueprint m & iTask a

tonicExtWrapBodyLam2    :: !ModuleName !TaskName [(VarName, m ())] (b c   -> m a) -> b c   -> m a | TonicTopLevelBlueprint m & iTask a

tonicExtWrapBodyLam3    :: !ModuleName !TaskName [(VarName, m ())] (b c d -> m a) -> b c d -> m a | TonicTopLevelBlueprint m & iTask a

tonicExtWrapApp         :: !(!ModuleName, !TaskName) !(!ModuleName, !TaskName) !(!ModuleName, !TaskName) !ExprId !ExprId (          m a)          -> m a | TonicBlueprintPart m & iTask a

tonicExtWrapAppLam1     :: !(!ModuleName, !TaskName) !(!ModuleName, !TaskName) !(!ModuleName, !TaskName) !ExprId !ExprId !(b     -> m a) -> b     -> m a | TonicBlueprintPart m & iTask a

tonicExtWrapAppLam2     :: !(!ModuleName, !TaskName) !(!ModuleName, !TaskName) !(!ModuleName, !TaskName) !ExprId !ExprId !(b c   -> m a) -> b c   -> m a | TonicBlueprintPart m & iTask a

tonicExtWrapAppLam3     :: !(!ModuleName, !TaskName) !(!ModuleName, !TaskName) !(!ModuleName, !TaskName) !ExprId !ExprId !(b c d -> m a) -> b c d -> m a | TonicBlueprintPart m & iTask a

tonicExtWrapTraversable :: !(!ModuleName, !TaskName) !(!ModuleName, !TaskName) !(!ModuleName, !TaskName)         !ExprId !([m a] -> m b) [m a] -> m b | TonicBlueprintPart m & iTask b
