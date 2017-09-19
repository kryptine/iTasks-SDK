implementation module iTasks.Extensions.Distributed.SDS

from iTasks._Framework.Generic import class iTask
from iTasks._Framework.SDS import :: ReadWriteShared, :: RWShared
from iTasks.Core.Types      import :: Task, generic gEq, generic gDefault, generic JSONDecode, generic JSONEncode, generic gText, generic gEditor, :: Editor
from Data.Maybe import :: Maybe 
from Text.JSON import :: JSONNode, generic JSONEncode, generic JSONDecode
from iTasks._Framework.Generic.Visualization    import :: TextFormat(..)
import qualified iTasks.Extensions.Distributed._SDS as R

get :: !(ReadWriteShared a w) -> Task a | iTask a & iTask w
get share = 'R'.rr_get share

upd :: !(r -> w) !(ReadWriteShared r w) -> Task w | iTask r & iTask w
upd func share = 'R'.rr_upd func share

set :: !a !(ReadWriteShared r a)  -> Task a | iTask a & iTask r
set val share = 'R'.rr_set val share

watch :: !(ReadWriteShared r w) -> Task r | iTask r & iTask w
watch share = 'R'.rr_watch share
