implementation module Chapter10

// Examples showing the usage of editors with multiple buttons

import iTasks

derive bimap (,)

Start :: *World -> *World
Start world = startEngine flows10 world

flows10 :: [Workflow]
flows10 =  []