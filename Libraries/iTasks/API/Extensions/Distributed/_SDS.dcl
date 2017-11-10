definition module iTasks.API.Extensions.Distributed._SDS

import iTasks

shareOperation :: String String [String] (String -> Task ()) -> Task ()

rr_get :: !(ReadWriteShared a w) -> Task a | iTask a & iTask w

rr_upd :: !(r -> w) !(ReadWriteShared r w) -> Task w | iTask r & iTask w

rr_set :: !a !(ReadWriteShared r a)  -> Task a | iTask a & iTask r

rr_watch :: !(ReadWriteShared r w) -> Task r | iTask r & iTask w
