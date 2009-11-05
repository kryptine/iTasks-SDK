definition module GeoDomain

import Html, InteractionTasks

derive gPrint 	  	Map, MapMarker, MapInfoWindow, GoogleMapType
derive gParse 	  	Map, MapMarker, MapInfoWindow, GoogleMapType
derive gVisualize   Map, MapMarker, MapInfoWindow, GoogleMapType
derive gUpdate	  	Map, MapMarker, MapInfoWindow, GoogleMapType

:: Map = 
	{ center				:: Coordinate 		// Coordinate of the center point (Required by maps)
	, width					:: Int		 		// Width &
	, height				:: Int				// Height of the map
	, mapTypeControl		:: Bool		  		// Show the control for switching between map types
	, navigationControl		:: Bool		  		// Show the control for panning
	, scaleControl			:: Bool		  		// Show the control for zooming
	, zoom					:: Int	      		// The zoom level (Required by maps)
	, mapType				:: GoogleMapType	// The map type
	, markers				:: [MapMarker]		// Markers placed on the map
	}
	
:: Coordinate :== (Real, Real) // (Lattitude, Longitude)

:: MapMarker =
	{ position				:: Coordinate		// Coordinate of the marker point
	, infoWindow			:: MapInfoWindow	// Information which is shown on click
	}
	
:: MapInfoWindow =
	{ content				:: String			// Contents of the window
	, width					:: Int				// Width of the window
	}

:: GoogleMapType = ROADMAP | SATELLITE | HYBRID | TERRAIN

instance toString GoogleMapType
