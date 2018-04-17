definition module iTasks.Extensions.Distributed._SDS

import iTasks

shareOperation :: String String [String] (String -> Task ()) -> Task ()

rr_get :: !(sds () r w) -> Task r | iTask r & iTask w & RWShared sds

rr_upd :: !(r -> w) !(sds () r w) -> Task w | iTask r & iTask w & RWShared sds

rr_set :: !w !(sds () r w)  -> Task w | iTask w & iTask r & RWShared sds

rr_watch :: !(sds () r w) -> Task r | iTask r & iTask w & RWShared sds
