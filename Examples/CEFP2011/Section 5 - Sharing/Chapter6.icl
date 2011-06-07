implementation module Chapter6

import iTasks

derive bimap (,), Maybe

Start :: *World -> *World
Start world = startEngine flows6 world

flows6 :: [Workflow]
flows6 =  []
