definition module iTasks.Extensions.Distributed._Types

from iTasks.WF.Definition import :: TaskAttributes, :: Task, class iTask, :: TaskValue, :: TaskResult
from iTasks.SDS.Definition import :: ReadWriteShared, :: SDS
from iTasks.Internal.IWorld import :: IWorld
from iTasks.Internal.TaskState import :: TaskTree
from iTasks.Internal.TaskEval import :: TaskEvalOpts
from iTasks.UI.Editor import :: Editor
from iTasks.UI.Editor.Generic import generic gEditor
from iTasks.Internal.Generic.Visualization import generic gText, :: TextFormat
from iTasks.Internal.Generic.Defaults import generic gDefault
from Text.JSON import generic JSONEncode, generic JSONDecode, :: JSONNode
from Data.Map import :: Map
from Data.Generics.GenEq import generic gEq
from Data.Maybe import :: Maybe

:: Remote_Task = E. a: Remote_Task (Task a) TaskAttributes Int & iTask a | Remote_Taks_NotUsed

:: Remote_Share = E. a w: Remote_Share (ReadWriteShared a w) & iTask a & iTask w | Remote_Share_NotUsed

:: Remote_TaskValue = E. a: Remote_TaskValue (TaskValue a) & iTask a | Remote_TaskValue_NotUsed
