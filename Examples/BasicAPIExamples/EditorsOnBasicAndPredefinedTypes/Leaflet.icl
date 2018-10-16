implementation module BasicAPIExamples.EditorsOnBasicAndPredefinedTypes.Leaflet

// Enter a Leaflet Map position

import iTasks
import iTasks.Extensions.GIS.Leaflet

wf :: String -> Workflow
wf a = workflow a "Enter a Leaflet map" leafletMap

main :: Task ()
main = leafletMap @! ()

leafletMap :: Task LeafletMap
leafletMap
	=   enterInformation "Enter a Leaflet map:" []
	>>= viewInformation "You entered:" [ViewAs (gText{|*|} AsMultiLine o Just)]