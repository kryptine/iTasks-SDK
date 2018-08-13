implementation module BasicAPIExamples.EditorsOnBasicAndPredefinedTypes.GoogleMap

// Enter a Google Map position

import iTasks
import iTasks.Extensions.GIS.GoogleMap

wf :: String -> Workflow
wf a = workflow a "Enter a Google map" googleMap

Start :: *World -> *World
Start world
	= doTasks googleMap world

googleMap :: Task GoogleMap
googleMap
	=   enterInformation "Enter a Google map:" []
	>>= viewInformation "You entered:" [ViewAs (gText{|*|} AsMultiLine o Just)]
