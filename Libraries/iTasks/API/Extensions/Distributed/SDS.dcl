definition module iTasks.API.Extensions.Distributed.SDS

from iTasks._Framework.Generic import class iTask
from iTasks._Framework.SDS import :: ReadWriteShared, :: RWShared
from iTasks.API.Core.Types	import :: Task, generic gEq, generic gDefault, generic JSONDecode, generic JSONEncode, generic gText, generic gEditor, :: Editor
from Data.Maybe import :: Maybe 
from Text.JSON import :: JSONNode, generic JSONEncode, generic JSONDecode
from iTasks._Framework.Generic.Visualization	import :: TextFormat(..)

get :: !(ReadWriteShared a w) -> Task a | iTask a & iTask w

upd :: !(r -> w) !(ReadWriteShared r w) -> Task w | iTask r & iTask w

set :: !a !(ReadWriteShared r a)  -> Task a | iTask a & iTask r

watch :: !(ReadWriteShared r w) -> Task r | iTask r & iTask w
