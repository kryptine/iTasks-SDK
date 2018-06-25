module BrowseAndViewGoogleMap

// Browse a Google map while viewing it

import iTasks
import iTasks.Extensions.GIS.GoogleMap

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
	>>= return																// and return