definition module iTasks._Framework.Tonic

from iTasks._Framework.SDS import :: Shared, :: ReadWriteShared, :: RWShared
from iTasks._Framework.IWorld import :: IWorld, :: SystemClocks
from iTasks._Framework.Engine import :: PublishedTask
import iTasks._Framework.Generic
from iTasks._Framework.Task import :: TaskEvalOpts, :: TaskResult
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

from System.IO import :: IO
from Text.Parsers.Parsers import :: Parser

// For all of these classes goes that the iTask context restriction shouldn't
// be there. Ideally, we would have something like associated type families
// and constraintkinds to determine the context restriction per monad.
class TonicTopLevelBlueprint m | TMonad m where
  tonicWrapBody :: !ModuleName !FuncName [(VarName, Int, m ())] [(ExprId, Int)] (m a) -> m a | iTask a
  tonicWrapArg  :: !VarName !Int a -> m () | iTask a

class TonicBlueprintPart m | TMonad m where
  tonicWrapApp  :: !ModuleName !FuncName !ExprId [(ExprId, Int)] (m a) -> m a | iTask a

instance TonicTopLevelBlueprint Task
instance TonicBlueprintPart Task
instance TonicBlueprintPart Maybe

instance TonicTopLevelBlueprint IO
instance TonicBlueprintPart IO

instance TApplicative IO
instance TFunctor IO
instance TMonad IO

instance TonicTopLevelBlueprint (Parser s t)
instance TonicBlueprintPart (Parser s t)

instance TFunctor (Parser s t)
instance TApplicative (Parser s t)
instance TMonad (Parser s t)

tonicStaticBrowser    :: [TaskAppRenderer] -> Task ()

tonicStaticWorkflow   :: [TaskAppRenderer] -> Workflow

tonicDynamicBrowser   :: [TaskAppRenderer] -> Task ()

tonicDynamicWorkflow  :: [TaskAppRenderer] -> Workflow

tonicExtWrapArg       :: !VarName !Int !a -> m () | iTask a & TonicTopLevelBlueprint m

tonicExtWrapBody      :: !ModuleName !FuncName [(VarName, Int, m ())] [(ExprId, Int)] (         m a)          -> m a | TonicTopLevelBlueprint m & iTask a

tonicExtWrapBodyLam1  :: !ModuleName !FuncName [(VarName, Int, m ())] [(ExprId, Int)] (b     -> m a) -> b     -> m a | TonicTopLevelBlueprint m & iTask a

tonicExtWrapBodyLam2  :: !ModuleName !FuncName [(VarName, Int, m ())] [(ExprId, Int)] (b c   -> m a) -> b c   -> m a | TonicTopLevelBlueprint m & iTask a

tonicExtWrapBodyLam3  :: !ModuleName !FuncName [(VarName, Int, m ())] [(ExprId, Int)] (b c d -> m a) -> b c d -> m a | TonicTopLevelBlueprint m & iTask a

tonicExtWrapApp       :: !ModuleName !FuncName !ExprId [(ExprId, Int)] (          m a)          -> m a | TonicBlueprintPart m & iTask a

tonicExtWrapAppLam1   :: !ModuleName !FuncName !ExprId [(ExprId, Int)] !(b     -> m a) -> b     -> m a | TonicBlueprintPart m & iTask a

tonicExtWrapAppLam2   :: !ModuleName !FuncName !ExprId [(ExprId, Int)] !(b c   -> m a) -> b c   -> m a | TonicBlueprintPart m & iTask a

tonicExtWrapAppLam3   :: !ModuleName !FuncName !ExprId [(ExprId, Int)] !(b c d -> m a) -> b c d -> m a | TonicBlueprintPart m & iTask a

storeTaskOutputViewer :: !(TaskResult a) !ExprId !TaskId !TaskId !*IWorld -> *IWorld | iTask a
