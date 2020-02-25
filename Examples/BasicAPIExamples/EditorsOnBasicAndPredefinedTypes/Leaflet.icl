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
	=   Hint "Enter a Leaflet map:" @>> enterInformation []
	>>! \result -> Hint "You entered:" @>> viewInformation [ViewAs (gText{|*|} AsMultiLine o Just)] result
