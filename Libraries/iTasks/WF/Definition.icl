implementation module iTasks.WF.Definition

from iTasks._Framework.IWorld import :: IWorld
from iTasks._Framework.TaskState import :: TaskTree
from iTasks._Framework.TaskEval import :: TaskEvalOpts, :: TaskEvalInfo
from iTasks.UI.Definition import :: UIChange
from Text.JSON import :: JSONNode
from Data.Map import :: Map(..)
from Data.Maybe import :: Maybe
from System.Time import :: Timestamp

from StdString import class toString(..)

exception :: !e -> TaskException | TC, toString e
exception e = (dynamic e, toString e)
