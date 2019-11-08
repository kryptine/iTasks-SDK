implementation module iTasks.Internal.TaskState

import Text.GenJSON, StdString, Data.Func, Data.GenEq, Data.Maybe, Data.Functor, Data.Map.GenJSON, Data.Set.GenJSON
import iTasks.UI.Definition, iTasks.UI.Layout
import iTasks.WF.Definition
from iTasks.WF.Combinators.Core import :: AttachmentStatus

from iTasks.Internal.Task	import exception
from iTasks.Internal.TaskEval import :: TaskTime, :: TaskEvalInfo(..)
from iTasks.Util.DeferredJSON import :: DeferredJSON
import iTasks.Internal.Serialization, iTasks.Internal.Generic.Visualization
import Data.Error, Data.Either
import iTasks.WF.Derives

derive JSONEncode TIMeta, TIType, TIValue, TIReduct, ParallelTaskState, TaskChange, TaskResult, TaskEvalInfo
derive JSONDecode TIMeta, TIType, TIValue, TIReduct, ParallelTaskState, TaskChange, TaskResult, TaskEvalInfo

derive gDefault TIMeta, InstanceProgress, TIType, TaskId, ValueStatus
