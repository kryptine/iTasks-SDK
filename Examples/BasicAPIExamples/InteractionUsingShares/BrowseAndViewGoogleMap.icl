implementation module BasicAPIExamples.InteractionUsingShares.BrowseAndViewGoogleMap

// Browse a Google map while viewing it

import iTasks
import iTasks.Extensions.GIS.GoogleMap

wf :: String -> Workflow
wf a = workflow a "Browse a shared GoogleMap" browseAndViewGoogleMap

Start :: *World -> *World
Start world
	= startEngine browseAndViewGoogleMap world

browseAndViewGoogleMap :: Task GoogleMap
browseAndViewGoogleMap
	= withShared defaultValue 												// create shared default value for the map
		(\smap -> 	updateSharedInformation "Browse Map" [] smap 			// update it here
					-||
					viewSharedInformation "View Browsing Map" [] smap )		// while viewing it here
	>>= viewInformation "Resulting map looks as follows" []					// show final result
