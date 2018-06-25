module positiveNumber

// Enter a postive Integer number

import iTasks

Start :: *World -> *World
Start world 
	= startEngine positiveNumber world
	
positiveNumber :: Task Int
positiveNumber 
	= 		enterInformation "Please enter a positive number" []
	>>* 	[ OnAction  ActionOk (ifValue (\n -> n > 0) return) 
	        ] 
	>>=		viewInformation "Entered number is:" []
	>>=		return
