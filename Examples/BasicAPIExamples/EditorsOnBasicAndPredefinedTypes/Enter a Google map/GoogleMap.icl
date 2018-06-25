module GoogleMap

// Enter a Google Map position

import iTasks
import iTasks.Extensions.GIS.GoogleMap

Start :: *World -> *World
Start world 
	= startEngine googleMap world
	
googleMap :: Task GoogleMap
googleMap 
	= 	enterInformation "Enter a Google map:" []
	>>=	viewInformation "You entered:" [ViewAs (gText{|*|} AsMultiLine o Just)] 
	>>= return 