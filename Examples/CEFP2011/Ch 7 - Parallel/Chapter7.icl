implementation module Chapter7

// Examples showing the usage of frequently used iTask combinators

import iTasks, StdMisc

derive bimap (,), Maybe

Start :: *World -> *World
Start world = startEngine 	[ 
							] world

