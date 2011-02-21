definition module GoogleMaps
/*
* This extension allows the use of GoogleMaps in editors
*/
import HTML, GenVisualize

derive gVisualize  		GoogleMap, GoogleMapPosition, GoogleMapMarker, GoogleMapInfoWindow, GoogleMapType, GoogleStaticMap
derive gUpdate	  		GoogleMap, GoogleMapPosition, GoogleMapMarker, GoogleMapInfoWindow, GoogleMapType, GoogleStaticMap
derive gDefaultMask		GoogleMap, GoogleMapPosition, GoogleMapMarker, GoogleMapInfoWindow, GoogleMapType, GoogleStaticMap
derive gVerify			GoogleMap, GoogleMapPosition, GoogleMapMarker, GoogleMapInfoWindow, GoogleMapType, GoogleStaticMap
derive JSONEncode		GoogleMap, GoogleMapPosition, GoogleMapMarker, GoogleMapInfoWindow, GoogleMapType, GoogleStaticMap
derive JSONDecode		GoogleMap, GoogleMapPosition, GoogleMapMarker, GoogleMapInfoWindow, GoogleMapType, GoogleStaticMap
derive gEq				GoogleMap, GoogleMapPosition, GoogleMapMarker, GoogleMapInfoWindow, GoogleMapType, GoogleStaticMap

//API Key for http://localhost
GOOGLE_API_KEY :== "ABQIAAAAaZ6XgbNqm4h_DL45IQMnSRT2yXp_ZAY8_ufC3CFXhHIE1NvwkxT4lboFdTKu2o9gr_i8kRV0Pn1fNw"

:: GoogleStaticMap = GoogleStaticMap Int Int String

:: GoogleMap = 
	{ center				:: GoogleMapPosition 		// Coordinate of the center point (Required by maps)
	, mapTypeControl		:: Bool		  				// Show the control for switching between map types
	, panControl			:: Bool		  				// Show the control for panning
	, zoomControl			:: Bool						// Show the control for zooming
	, streetViewControl		:: Bool						// Show the control for street view
	, scaleControl			:: Bool		  				// Show the scale of the map
	, scrollwheel			:: Bool						// Scrollwheel zooming on the map
	, draggable				:: Bool						// Map can be dragged
	, zoom					:: Int	      				// The zoom level (Required by maps)
	, mapType				:: GoogleMapType			// The map type
	, markers				:: [GoogleMapMarker]		// Markers placed on the map
	}
	
:: GoogleMapPosition = 
	{ lat		:: !Real	//Lattitude
	, lng		:: !Real	//Longitude
	}
	
:: GoogleMapMarker =
	{ position				:: !GoogleMapPosition			// Position of the marker
	, title					:: !Maybe String				// Title of the marker
	, infoWindow			:: !Maybe GoogleMapInfoWindow	// Information which is shown on click
	, draggable				:: !Bool						// Can the marker be dragged
	}
	
:: GoogleMapInfoWindow =
	{ content				:: !String			// Contents of the window
	}

:: GoogleMapType = ROADMAP | SATELLITE | HYBRID | TERRAIN

:: MVCUpdate = 
	{ center			:: !GoogleMapPosition
	, zoom				:: !Int
	, type				:: !GoogleMapType
	}	
	
:: ClickUpdate = 
	{ event				:: !ClickEvent
	, source			:: !ClickSource
	, point				:: !GoogleMapPosition
	}

:: ClickEvent	= LEFTCLICK | RIGHTCLICK | DBLCLICK
:: ClickSource  = MAP | MARKER GoogleMapPosition

:: MarkerDragUpdate = 
	{ index				:: !Int
	, point				:: !GoogleMapPosition
	}
/*
* Convert a dynamic map into a static image
*
* @param The map to convert
*
* @return The converted result
*/
convertToStaticMap :: GoogleMap -> GoogleStaticMap

/**
* Create a default map
*
* @return A default map
*/
mkMap :: GoogleMap
/**
* Create a default map without any control options switched on
*
* @return A minimal default map 
*/
minimalMap :: GoogleMap

instance toString GoogleMapType
