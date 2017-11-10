definition module iTasks.API.Extensions.Distributed.InteractionTasks

from iTasks._Framework.Generic import class iTask
from iTasks._Framework.SDS import :: ReadWriteShared, :: RWShared
from iTasks.API.Core.Types	import :: Task, generic gEq, generic gDefault, generic JSONDecode, generic JSONEncode, generic gText, generic gEditor, :: Editor
from Data.Maybe import :: Maybe 
from Text.JSON import :: JSONNode, generic JSONEncode, generic JSONDecode
from iTasks._Framework.Generic.Visualization	import :: TextFormat(..)
from iTasks.API.Common.InteractionTasks         import :: ViewOption(..)

viewSharedInformation :: String [ViewOption r] !(ReadWriteShared r w) -> Task r | iTask r & iTask w
