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
		(\smap -> 	updateSharedInformation "Browse Map" [] smap 			// update it here
					-||
					viewSharedInformation [ViewWithHint "View Browsing Map"] smap )		// while viewing it here
	>>= viewInformation [ViewWithHint "Resulting map looks as follows"]					// show final result
