definition module iTasks.Extensions.Distributed.SDS

from iTasks.WF.Definition import class iTask
import iTasks.Internal.SDS
from iTasks.WF.Definition	import :: Task, generic gEq, generic gDefault, generic JSONDecode, generic JSONEncode, generic gText, generic gEditor, :: Editor
from Data.Maybe import :: Maybe 
from Text.GenJSON import :: JSONNode, generic JSONEncode, generic JSONDecode
from iTasks.Internal.Generic.Visualization	import :: TextFormat(..)

get :: !(sds () a w) -> Task a | iTask a & iTask w& RWShared sds

upd :: !(r -> w) !(sds () r w) -> Task w | iTask r & iTask w & RWShared sds

set :: !a !(sds () r a)  -> Task a | iTask a & iTask r & RWShared sds

watch :: !(sds () r w) -> Task r | iTask r & iTask w & RWShared sds
