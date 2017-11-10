definition module iTasks.Extensions.Distributed.SDS

from iTasks.WF.Definition import class iTask
from iTasks.Internal.SDS import :: SDS, :: ReadWriteShared, :: RWShared
from iTasks.WF.Definition	import :: Task, generic gEq, generic gDefault, generic JSONDecode, generic JSONEncode, generic gText, generic gEditor, :: Editor
from Data.Maybe import :: Maybe 
from Text.JSON import :: JSONNode, generic JSONEncode, generic JSONDecode
from iTasks.Internal.Generic.Visualization	import :: TextFormat(..)

get :: !(ReadWriteShared a w) -> Task a | iTask a & iTask w

upd :: !(r -> w) !(ReadWriteShared r w) -> Task w | iTask r & iTask w

set :: !a !(ReadWriteShared r a)  -> Task a | iTask a & iTask r

watch :: !(ReadWriteShared r w) -> Task r | iTask r & iTask w
