implementation module Chapter5

// Examples showing the extension of editors with buttons

import iTasks

derive bimap (,), Maybe

Start :: *World -> *World
Start world = startEngine flows5 world

flows5 :: [Workflow]
flows5 =  []


show :: (Task a) -> Task a | iTask a
show task = task >>= showMessageAbout "The result is:"
