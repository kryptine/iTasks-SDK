implementation module Chapter9

import iTasks

derive bimap (,), Maybe

Start :: *World -> *World
Start world = startEngine flows9 world

flows9 :: [Workflow]
flows9 =  []
