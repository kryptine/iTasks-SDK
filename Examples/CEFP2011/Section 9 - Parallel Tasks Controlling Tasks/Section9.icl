implementation module Section9

// Examples showing the usage of editors with multiple buttons

import iTasks

derive bimap (,)

Start :: *World -> *World
Start world = startEngine flows9 world

flows9 :: [Workflow]
flows9 =  []