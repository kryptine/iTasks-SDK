implementation module iTasks.API.Core.TaskLayout

from iTasks._Framework.Task import :: TaskId
from Text.JSON import generic JSONEncode, generic JSONDecode, ::JSONNode
import Data.Maybe

:: UITag :== TaskId
