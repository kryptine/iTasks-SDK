implementation module BasicAPIExamples.InteractionUsingShares.BrowseAndViewLeafletMap

// Browse a Leaflet map while viewing it

import iTasks
import iTasks.Extensions.GIS.Leaflet

wf :: String -> Workflow
wf a = workflow a "Browse a shared LeafletMap" browseAndViewLeafletMap

main :: Task ()
main = browseAndViewLeafletMap @! ()

browseAndViewLeafletMap :: Task LeafletMap
browseAndViewLeafletMap
	= withShared defaultValue 												// create shared default value for the map
		(\smap -> 	(Hint "Browse Map" @>> updateSharedInformation [] smap) 			// update it here
					-||
					(Hint "View Browsing Map" @>> viewSharedInformation [] smap) )		// while viewing it here
	>>! \result -> Hint "Resulting map looks as follows" @>> viewInformation [] result					// show final result
