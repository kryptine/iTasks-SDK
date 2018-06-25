module EnterDateAndTime

// Enter a list of dates and a times

import iTasks
import iTasks.Extensions.DateTime

Start :: *World -> *World
Start world 
	= startEngine enterDateTime world
	
enterDateTime :: Task [(Date, Time)]
enterDateTime 
	= 	enterInformation "Enter a date and time" []
	>>=	viewInformation "You Entered:" []
	>>= return
