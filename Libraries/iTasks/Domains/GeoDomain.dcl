definition module GeoDomain

import Html, InteractionTasks, CommonDomain

derive gPrint 	  	Map, MapMarker, MapInfoWindow, GoogleMapType, StaticMap
derive gParse 	  	Map, MapMarker, MapInfoWindow, GoogleMapType, StaticMap
derive gVisualize   Map, MapMarker, MapInfoWindow, GoogleMapType, StaticMap
derive gUpdate	  	Map, MapMarker, MapInfoWindow, GoogleMapType, StaticMap

GOOGLE_API_KEY :== "ABQIAAAAaZ6XgbNqm4h_DL45IQMnSRT2yXp_ZAY8_ufC3CFXhHIE1NvwkxT4lboFdTKu2o9gr_i8kRV0Pn1fNw"

:: StaticMap = StaticMap Int Int String

:: Map = 
	{ center				:: Coordinate 				  // Coordinate of the center point (Required by maps)
	, width					:: Int		 				  // Width &
	, height				:: Int						  // Height of the map
	, mapTypeControl		:: Bool		  				  // Show the control for switching between map types
	, navigationControl		:: Bool		  				  // Show the control for panning
	, scaleControl			:: Bool		  				  // Show the control for zooming
	, zoom					:: Int	      				  // The zoom level (Required by maps)
	, mapType				:: GoogleMapType			  // The map type
	, markers				:: [MapMarker]				  // Markers placed on the map
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

:: MVCUpdate = 
	{ center			:: Coordinate
	, zoom				:: Int
	, type				:: GoogleMapType
	}	
	
:: ClickUpdate = 
	{ event				:: ClickEvent
	, source			:: ClickSource
	, point				:: Coordinate
	}
	
:: ClickEvent	= LEFTCLICK | RIGHTCLICK | DBLCLICK
:: ClickSource  = MAP | MARKER Coordinate

convertToStaticMap :: Map -> StaticMap
mkMap :: Map

instance toString GoogleMapType
//instance html	  StaticMap
