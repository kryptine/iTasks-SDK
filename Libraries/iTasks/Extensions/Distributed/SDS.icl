implementation module iTasks.Extensions.Distributed.SDS

from iTasks.WF.Definition import class iTask
from iTasks.WF.Definition      import :: Task, generic gEq, generic gDefault, generic JSONDecode, generic JSONEncode, generic gText, generic gEditor, :: Editor
from Data.Maybe import :: Maybe 
from Text.GenJSON import :: JSONNode, generic JSONEncode, generic JSONDecode
from iTasks.Internal.Generic.Visualization    import :: TextFormat(..)
import qualified iTasks.Extensions.Distributed._SDS as R
import iTasks.SDS.Definition

get :: !(sds () a w) -> Task a | iTask a & iTask w & RWShared sds
get share = 'R'.rr_get share

upd :: !(r -> w) !(sds () r w) -> Task w | iTask r & iTask w & RWShared sds
upd func share = 'R'.rr_upd func share

set :: !a !(sds () r a)  -> Task a | iTask a & iTask r & RWShared sds
set val share = 'R'.rr_set val share

watch :: !(sds () r w) -> Task r | iTask r & iTask w & RWShared sds
watch share = 'R'.rr_watch share
