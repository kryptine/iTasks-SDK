module CurrentDateAndTime

// Just show the current date and time which are offered in a share
// Next the current time is displayed in the view of an analog clock

import iTasks
import iTasks.Extensions.DateTime
import iTasks.Extensions.Clock

Start :: *World -> *World
Start world 
	= startEngine showDateAndTime world
	

showDateAndTime :: Task Time 
showDateAndTime 
	= 	viewSharedInformation "The current Date and Time is:" [] currentDateTime
	>>| viewSharedInformation "The current time is:" [ViewAs AnalogClock] currentTime
	>>= return
