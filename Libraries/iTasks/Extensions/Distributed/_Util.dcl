definition module iTasks.Extensions.Distributed._Util

import iTasks

memoryShare_ :: String a -> SDSLens () a a | iTask a

repeatClient :: (Task (Maybe a)) -> Task (Maybe a) | iTask a
