definition module iTasks.API.Extensions.Distributed._Util

import iTasks

memoryShare_ :: String a -> RWShared () a a | iTask a

repeatClient :: (Task (Maybe a)) -> Task (Maybe a) | iTask a
