definition module GeoDomain

import Html, InteractionTasks, CommonDomain, GenVerify

derive gPrint 	  		GoogleMap, GoogleMapMarker, GoogleMapInfoWindow, GoogleMapType, GoogleStaticMap
derive gParse 	  		GoogleMap, GoogleMapMarker, GoogleMapInfoWindow, GoogleMapType, GoogleStaticMap
derive gVisualize  		GoogleMap, GoogleMapMarker, GoogleMapInfoWindow, GoogleMapType, GoogleStaticMap
derive gUpdate	  		GoogleMap, GoogleMapMarker, GoogleMapInfoWindow, GoogleMapType, GoogleStaticMap
derive gMerge	  		GoogleMap, GoogleMapMarker, GoogleMapInfoWindow, GoogleMapType, GoogleStaticMap
derive gError			GoogleMap, GoogleMapMarker, GoogleMapInfoWindow, GoogleMapType, GoogleStaticMap
derive gHint			GoogleMap, GoogleMapMarker, GoogleMapInfoWindow, GoogleMapType, GoogleStaticMap

//API Key for http://localhost
GOOGLE_API_KEY :== "ABQIAAAAaZ6XgbNqm4h_DL45IQMnSRT2yXp_ZAY8_ufC3CFXhHIE1NvwkxT4lboFdTKu2o9gr_i8kRV0Pn1fNw"

:: GoogleStaticMap = GoogleStaticMap Int Int String

:: GoogleMap = 
	{ center				:: Coordinate 				// Coordinate of the center point (Required by maps)
	, width					:: Int		 				// Width &
	, height				:: Int						// Height of the map
	, mapTypeControl		:: Bool		  				// Show the control for switching between map types
	, navigationControl		:: Bool		  				// Show the control for panning
	, scaleControl			:: Bool		  				// Show the control for zooming
	, scrollwheel			:: Bool						// Scrollwheel zooming on the map
	, draggable				:: Bool						// Map can be dragged
	, zoom					:: Int	      				// The zoom level (Required by maps)
	, mapType				:: GoogleMapType			// The map type
	, markers				:: [GoogleMapMarker]		// Markers placed on the map
	}
	
:: Coordinate :== (Real, Real) // (Lattitude, Longitude)

:: GoogleMapMarker =
	{ position				:: Coordinate			// Coordinate of the marker point
	, infoWindow			:: GoogleMapInfoWindow	// Information which is shown on click
	}
	
:: GoogleMapInfoWindow =
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

convertToStaticMap :: GoogleMap -> GoogleStaticMap
mkMap :: GoogleMap

instance toString GoogleMapType
