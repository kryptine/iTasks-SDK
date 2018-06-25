module Hello_World

// Just displaying a message to welcome you.

import iTasks

Start :: *World -> *World
Start world 
	= startEngine helloWorld world
	
helloWorld :: Task String
helloWorld 
	= 	viewInformation "You have a message from iTasks:" [] "Hello world!" 
	

