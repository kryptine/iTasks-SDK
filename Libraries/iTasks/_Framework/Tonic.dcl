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
from System.Time import :: Timestamp
from Data.Map import :: Map
from Data.Set import :: Set
from Graphics.Scalable import :: Image, :: TagSource, :: TagRef, :: ImageTag
from iTasks.API.Core.Types import class TMonad, class TApplicative
import Data.Functor

from System.IO import :: IO

// For all of these classes goes that the iTask context restriction shouldn't
// be there. Ideally, we would have something like associated type families
// and constraintkinds to determine the context restriction per monad.
class TonicTopLevelBlueprint m | TonicBlueprintPart m where
  tonicWrapBody :: !ModuleName !FuncName [(VarName, Int, m ())] [(ExprId, Int)     ] (m a) -> m a
  tonicWrapArg  :: !VarName !Int a -> m ()

class TonicBlueprintPart m | TMonad m where
  tonicWrapApp  :: !ModuleName !FuncName !ExprId                [(ExprId, a -> Int)] (m a) -> m a

instance TonicTopLevelBlueprint Task
instance TonicBlueprintPart Task

//instance TonicTopLevelBlueprint Maybe
instance TonicBlueprintPart Maybe

//instance TonicTopLevelBlueprint (Either e)
//instance TonicBlueprintPart (Either e)

//instance TonicTopLevelBlueprint IO
//instance TonicBlueprintPart IO

//instance TApplicative IO
//instance TMonad IO

tonicExtWrapArg       :: !VarName !Int !a -> m () | TonicTopLevelBlueprint m

tonicExtWrapBody      :: !ModuleName !FuncName [(VarName, Int, m ())] [(ExprId, Int)] (         m a)          -> m a | TonicTopLevelBlueprint m

tonicExtWrapBodyLam1  :: !ModuleName !FuncName [(VarName, Int, m ())] [(ExprId, Int)] (b     -> m a) -> b     -> m a | TonicTopLevelBlueprint m

tonicExtWrapBodyLam2  :: !ModuleName !FuncName [(VarName, Int, m ())] [(ExprId, Int)] (b c   -> m a) -> b c   -> m a | TonicTopLevelBlueprint m

tonicExtWrapBodyLam3  :: !ModuleName !FuncName [(VarName, Int, m ())] [(ExprId, Int)] (b c d -> m a) -> b c d -> m a | TonicTopLevelBlueprint m

tonicExtWrapApp       :: !ModuleName !FuncName !ExprId [(ExprId, a -> Int)] (          m a)          -> m a | TonicBlueprintPart m

tonicExtWrapAppLam1   :: !ModuleName !FuncName !ExprId [(ExprId, a -> Int)] !(b     -> m a) -> b     -> m a | TonicBlueprintPart m

tonicExtWrapAppLam2   :: !ModuleName !FuncName !ExprId [(ExprId, a -> Int)] !(b c   -> m a) -> b c   -> m a | TonicBlueprintPart m

tonicExtWrapAppLam3   :: !ModuleName !FuncName !ExprId [(ExprId, a -> Int)] !(b c d -> m a) -> b c d -> m a | TonicBlueprintPart m