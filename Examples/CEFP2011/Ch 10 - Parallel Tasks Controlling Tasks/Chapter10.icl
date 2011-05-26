implementation module Chapter10

import iTasks

derive bimap (,), Maybe

Start :: *World -> *World
Start world = startEngine flows10 world

flows10 :: [Workflow]
flows10 =  []
