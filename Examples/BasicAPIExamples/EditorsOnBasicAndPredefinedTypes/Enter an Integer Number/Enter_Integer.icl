module Enter_Integer

// Enter an Integer Number

import iTasks

Start :: *World -> *World
Start world 
	= startEngine enterInt world
	
enterInt :: Task Int
enterInt 
	= 	enterInformation "Enter an Integer number:" []
	>>=	viewInformation "You entered:" []
	>>=	return
	
