module EnterListOfInt

// Enter a list of Integer numbers

import iTasks

Start :: *World -> *World
Start world 
	= startEngine enterListOfInt world
	
enterListOfInt :: Task [Int]
enterListOfInt 
	= 	enterInformation "Enter a list of Integer numbers:" []
	>>=	viewInformation "You Entered:" []
	>>= return