definition module iTasks.Extensions.Distributed._Util

import iTasks

memoryShare :: String a -> RWShared () a a | iTask a

repeatClient :: (Task (Maybe a)) -> Task (Maybe a) | iTask a
